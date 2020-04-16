{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wall #-}

module Cat.Parse
    ( module Cat.Parse
    , parse
    ) where

import           Control.Monad                  (void)
import           Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import           Data.Char                      (isDigit)
import           Data.Foldable                  (traverse_)
import           Data.Function                  ((&))
import           Data.HashSet                   (HashSet)
import qualified Data.HashSet                   as HashSet (fromList, member)
import           Data.List                      (foldl')
import           Data.Text                      (Text)
import qualified Data.Text                      as Text (pack, unpack)
import qualified Data.Text.Read                 as Text.Read (decimal)
import           Data.Void                      (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import           Cat.Common
import           Cat.Syntax

type Parser = Parsec Void Text

-- Primitives

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser ()
symbol = void . L.symbol sc

symbol' :: Text -> Parser ()
symbol' = void . L.symbol' sc

identifier :: Parser Text
identifier = label "identifier" $ lexeme $ do
    let f c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'
    candidate <- takeWhile1P (Just "letter") f
    if candidate `HashSet.member` reserved
        then fail $ "\"" ++ Text.unpack candidate ++ "\" is a reserved word"
        else pure candidate

reserved :: HashSet Text
reserved = HashSet.fromList
    [ "if", "then", "else" , "while", "do", "let", "break", "for", "to"
    , "in", "end", "function", "array", "type", "of", "and", "or", "var"
    , "array"
    ]

intLit :: Parser Int64
intLit = label "integer" $ lexeme $ mkNum =<< takeWhile1P (Just "digit") isDigit
    where
    mkNum txt =
        case Text.Read.decimal txt of
          Right (n, "") -> pure n
          Right _ -> error "This shouldn't happen"
          Left err -> fail err

stringLit :: Parser Text
stringLit = label "string" $ lexeme $ Text.pack <$> between (char '"') (char '"') (many $ satisfy (/= '"'))

-----------------------------------------------------------------------------

-- <prog> ::= <top_level_dec>*
parseProgram :: Parser Program
parseProgram = some parseTopLevelDec <* sc <* eof

-- <top_level_dec> ::= type <id> = array of <id>
--                   | type <id> = { <field_dec>* }
--                   | function <id> ( <formal_arg>* ) -> <id> { <exp>+ }
parseTopLevelDec :: Parser TopLevelDec
parseTopLevelDec = typedef <|> funDef
    where
        typedef = label "type definition" $ do
            symbol "type"
            t <- identifier
            symbol "="
            arrTypedef t <|> recTypedef t
        arrTypedef t = do
            traverse_ symbol ["array", "of"]
            ty <- identifier
            pure $ TyDecArray t ty
        recTypedef t =
            let parseFld = do
                    fld <- identifier
                    symbol ":"
                    ty <- identifier
                    pure (fld, ty)
             in TyDecRecord t <$> between (symbol "{") (symbol "}") (parseFld `sepBy` symbol ",")
        funDef = label "function definition" $ do
            symbol "function"
            name <- identifier
            args <- between (symbol "(") (symbol ")") $ 
                let formalArg = (,) <$> identifier <* symbol ":" <*> identifier
                 in formalArg `sepBy` symbol ","
            symbol "->"
            retTy <- identifier
            body <- between (symbol "{") (symbol "}") $ some parseExp
            pure $ case body of
              [e] -> FunDec name retTy args e
              _ -> FunDec name retTy args (ExpSequence body)

-- Parse an atomic expression
parseExpAtomic :: Parser Exp
parseExpAtomic =
        parseIf -- Begin with a unique reserved identifier
    <|> parseFor
    <|> parseWhile
    <|> parseLet
    <|> parseBreak
    <|> (ExpIntLit <$> intLit) -- Unique
    <|> (ExpStringLit <$> stringLit)
    <|> between (symbol "(") (symbol ")") (ExpSequence <$> parseExp `sepBy` symbol ";")
    <|> try parseArrCreate -- Here on out, choices can be confused, use 'try'
    <|> try parseRecordCreate
    <|> try parseAssign
    <|> try parseCall
    <|> (ExpLValue <$> parseLValue)
    where
        parseIf = label "if" $ do
            symbol "if"
            e1 <- parseExp
            symbol "then"
            e2 <- parseExp -- This munches 'else's, so no "dangling else" problem
            me3 <- optional (symbol "else" >> parseExp)
            pure $ case me3 of
              Nothing -> ExpIfThen e1 e2
              Just e3 -> ExpIfThenElse e1 e2 e3
        parseFor = label "for" $ do
            symbol "for"
            i <- identifier
            symbol ":="
            begin <- parseExp
            symbol "to"
            end <- parseExp
            symbol "do"
            body <- parseExp
            pure $ ExpFor i begin end body
        parseWhile = label "while" $ do
            symbol "while"
            e <- parseExp
            symbol "do"
            ExpWhile e <$> parseExp
        parseLet = label "let" $ do
            symbol "let"
            decs <- many parseDec
            symbol "in"
            e <- parseExp
            symbol "end"
            pure $ ExpLet decs e
        parseArrCreate = label "array" $ do
            t <- identifier
            e <- between (symbol "[") (symbol "]") parseExp
            symbol "of"
            ExpArrayCreate t e <$> parseExp
        parseAssign = label "assignment" $ ExpAssign <$> parseLValue <* symbol ":=" <*> parseExp
        parseCall = label "function call" $ do
            f <- identifier
            args <- between (symbol "(") (symbol ")") $ parseExp `sepBy` symbol ","
            pure $ ExpCall f args
        parseBreak = label "break" $ ExpBreak <$ symbol "break"
        parseRecordCreate = label "record" $ do
            t <- identifier
            let parseFld = (,) <$> identifier <* symbol "=" <*> parseExp
            flds <- between (symbol "{") (symbol "}") $ parseFld `sepBy` symbol ","
            pure $ ExpRecordCreate t flds


parseExp :: Parser Exp
parseExp = makeExprParser parseExpAtomic tbl
    where
        tbl = [ [ Prefix $ ExpNegate <$ symbol "-" ]
              , [ InfixL $ flip ExpInfix SOpMult <$ symbol "*"
                , InfixL $ flip ExpInfix SOpDiv  <$ symbol "/"
                ]
              , [ InfixL $ flip ExpInfix SOpAdd <$ symbol "+"
                , InfixL $ flip ExpInfix SOpSub <$ symbol "-"
                ]
              , [ InfixL $ flip ExpInfix SOpEq  <$ symbol "="
                , InfixL $ flip ExpInfix SOpNeq <$ symbol "<>"
                , InfixL $ flip ExpInfix SOpGte <$ symbol ">="
                , InfixL $ flip ExpInfix SOpLte <$ symbol "<="
                , InfixL $ flip ExpInfix SOpGt  <$ symbol ">" <* notFollowedBy (char '=')
                , InfixL $ flip ExpInfix SOpLt  <$ symbol "<" <* notFollowedBy (char '=')
                ]
              , [ InfixL $ flip ExpInfix SOpAnd <$ symbol "and" <* notFollowedBy letterChar
                ]
              , [ InfixL $ flip ExpInfix SOpOr <$ symbol "or" <* notFollowedBy letterChar
                ]
              ]

parseDec :: Parser Dec
parseDec = do
    symbol "var"
    x <- identifier
    symbol ":"
    t <- identifier
    symbol ":="
    VarDec x t <$> parseExp

parseLValue :: Parser LValue
parseLValue = do
    x <- identifier
    subs <- many $
        (flip LValueSubscript <$> between (symbol "[") (symbol "]") parseExp)
        <|> 
        (flip LValueFieldExp <$> (symbol "." *> identifier))
    pure $ foldl' (&) (LValueId x) subs 



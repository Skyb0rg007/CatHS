{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Cat.Syntax
    ( module Cat.Syntax
    , Int64
    ) where

import           Control.Lens.TH
import           Data.Functor                              ((<&>))
import           Data.Functor.Foldable.TH
import           Data.Int                                  (Int64)
import           Data.Text                                 (Text)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal

import Cat.Common

type Program = [TopLevelDec]
type Ty = Text

data TopLevelDec
    = TyDecArray {- New Type -} !Text {- Element Type -} !Ty
    | TyDecRecord {- New Type -} !Text {- Fields -} ![(Text, Ty)]
    | FunDec {- Name -} !Text {- Return Type -} !Ty {- Args -} ![(Text, Ty)] {- Body -} !Exp
    deriving (Show, Eq)

data Exp
    = ExpBreak
    | ExpIntLit !Int64
    | ExpStringLit !Text
    | ExpLValue !LValue
    | ExpSequence ![Exp]
    | ExpNegate !Exp
    | ExpInfix !Exp !InfixSourceOp !Exp
    | ExpArrayCreate {- Type Id -} !Ty {- Length -} !Exp {- Initial Value -} !Exp
    | ExpRecordCreate {- Type Id -} !Ty {- Fields -} ![(Text, Exp)]
    | ExpAssign !LValue !Exp
    | ExpIfThenElse !Exp !Exp !Exp
    | ExpIfThen !Exp !Exp
    | ExpWhile !Exp !Exp
    | ExpFor !Text !Exp !Exp !Exp -- for name = e1 to e2 do e3
    | ExpLet ![Dec] !Exp
    | ExpCall !Text ![Exp]
    deriving (Show, Eq, Ord)

data Dec
    = VarDec !Text !Ty !Exp
    deriving (Show, Eq, Ord)

data LValue
    = LValueId !Text -- x
    | LValueSubscript !LValue !Exp -- arr[x]
    | LValueFieldExp !LValue !Text -- obj.x
    deriving (Show, Eq, Ord)


data ExpAnn
    = AnnInt
    | AnnString
    | AnnOp
    | AnnKeyword
    | AnnVar
    | AnnTy
    | AnnField
    | AnnFun
    deriving (Show, Eq, Ord, Enum)

annMap :: ExpAnn -> AnsiStyle
annMap = \case
    AnnInt -> color Red
    AnnString -> colorDull Red
    AnnOp -> colorDull Green
    AnnKeyword -> color Green
    AnnVar -> color Cyan
    AnnTy -> colorDull Yellow
    AnnField -> colorDull Magenta
    AnnFun -> colorDull Cyan


prettyProg :: Program -> Doc ExpAnn
prettyProg = vsep . fmap prettyToplevelDec
    where
        kw  = annotate AnnKeyword
        op  = annotate AnnOp
        ty  = annotate AnnTy
        fld = annotate AnnField
        var = annotate AnnVar
        fun = annotate AnnFun
        parenIf True = parens
        parenIf False = id
        prettyFields :: [(Text, Ty)] -> Doc ExpAnn
        prettyFields [] = "{}"
        prettyFields fs = encloseSep (op "{ ") (op " }") (op ", ") (fs <&> \(x, t) -> fld (pretty x) <+> op ":" <+> ty (pretty t))
        prettyFields' :: [(Text, Exp)] -> Doc ExpAnn
        prettyFields' [] = "{}"
        prettyFields' fs = encloseSep (op "{ ") (op " }") (op ", ") (fs <&> \(x, e) -> fld (pretty x) <+> op ":" <+> prettyExp 0 e)
        prettyArgs :: [(Text, Ty)] -> Doc ExpAnn
        prettyArgs fs = encloseSep (op "(") (op ")") (op ", ") (fs <&> \(x, t) -> fld (pretty x) <+> op ":" <+> ty (pretty t))
        prettyToplevelDec :: TopLevelDec -> Doc ExpAnn
        prettyToplevelDec = \case
            TyDecArray t elemTy ->
                kw "type" <+> ty (pretty t) <+> op "="
                <+> kw "array" <+> kw "of" <+> ty (pretty elemTy)
            TyDecRecord t fields ->
                kw "type" <+> ty (pretty t) <+> op "=" <+> prettyFields fields
            FunDec name retTy args body ->
                kw "function" <+> fun (pretty name) <+> prettyArgs args <+> op "->" <+> ty (pretty retTy) <+> op "{" <> hardline
                <> indent 4 (prettyExp 0 body) <> hardline
                <> "}"
        prettyExp :: Int -> Exp -> Doc ExpAnn
        prettyExp p = \case
            ExpBreak -> kw "break"
            ExpIntLit n -> annotate AnnInt (pretty n)
            ExpStringLit s -> annotate AnnString ("\"" <> pretty s <> "\"")
            ExpLValue l -> prettyLValue l
            ExpSequence exps -> encloseSep (op "(") (op ")") (op ";") (prettyExp 0 <$> exps)
            ExpNegate e -> "-" <> prettyExp 7 e
            ExpInfix e1 SOpMult e2 -> parenIf (p > 6) $ prettyExp 6 e1 <+> "*" <+> prettyExp 6 e2
            ExpInfix e1 SOpDiv  e2 -> parenIf (p > 6) $ prettyExp 6 e1 <+> "/" <+> prettyExp 6 e2
            ExpInfix e1 SOpAdd  e2 -> parenIf (p > 5) $ prettyExp 5 e1 <+> "+" <+> prettyExp 5 e2
            ExpInfix e1 SOpSub  e2 -> parenIf (p > 5) $ prettyExp 5 e1 <+> "-" <+> prettyExp 5 e2
            ExpInfix e1 SOpLt   e2 -> parenIf (p > 4) $ prettyExp 4 e1 <+> "<" <+> prettyExp 4 e2
            ExpInfix e1 SOpGt   e2 -> parenIf (p > 4) $ prettyExp 4 e1 <+> ">" <+> prettyExp 4 e2
            ExpInfix e1 SOpLte  e2 -> parenIf (p > 4) $ prettyExp 4 e1 <+> "<=" <+> prettyExp 4 e2
            ExpInfix e1 SOpGte  e2 -> parenIf (p > 4) $ prettyExp 4 e1 <+> ">=" <+> prettyExp 4 e2
            ExpInfix e1 SOpNeq  e2 -> parenIf (p > 4) $ prettyExp 4 e1 <+> "<>" <+> prettyExp 4 e2
            ExpInfix e1 SOpEq   e2 -> parenIf (p > 4) $ prettyExp 4 e1 <+> "=" <+> prettyExp 4 e2
            ExpInfix e1 SOpAnd  e2 -> parenIf (p > 3) $ prettyExp 3 e1 <+> "and" <+> prettyExp 3 e2
            ExpInfix e1 SOpOr   e2 -> parenIf (p > 2) $ prettyExp 2 e1 <+> "or" <+> prettyExp 2 e2
            ExpArrayCreate t len init -> ty (pretty t) <> op "[" <> prettyExp 0 len <> op "]" <+> kw "of" <+> prettyExp 0 init
            ExpRecordCreate t flds -> ty (pretty t) <> prettyFields' flds
            ExpAssign lv e -> prettyLValue lv <+> op ":=" <+> prettyExp 0 e
            ExpIfThenElse e1 e2 e3 -> kw "if" <+> prettyExp 0 e1 <+> kw "then" <+> prettyExp 0 e2 <+> kw "else" <+> prettyExp 0 e3
            ExpIfThen e1 e2 -> kw "if" <+> prettyExp 0 e1 <+> kw "then" <+> prettyExp 0 e2
            ExpWhile cond body -> kw "while" <+> prettyExp 0 cond <+> kw "do" <+> prettyExp 0 body
            ExpFor i begin end body -> kw "for" <+> var (pretty i) <+> ":=" <+> prettyExp 0 begin <+> kw "to" <+> prettyExp 0 end <+> kw "do" <+> prettyExp 0 body
            ExpLet binds body -> kw "let" <+> align (vsep (prettyDec <$> binds)) <+> kw "in" <+> prettyExp 0 body <+> kw "end"
            ExpCall f args -> fun (pretty f) <> encloseSep "(" ")" ", " (prettyExp 0 <$> args)

        prettyDec :: Dec -> Doc ExpAnn
        prettyDec (VarDec x t e) = kw "var" <+> var (pretty x) <+> op ":" <+> ty (pretty t) <+> ":=" <+> prettyExp 0 e
        prettyLValue :: LValue -> Doc ExpAnn
        prettyLValue = \case
            LValueId x -> var (pretty x)
            LValueSubscript a i -> prettyLValue a <> op "[" <> prettyExp 0 i <> op "]"
            LValueFieldExp o x -> prettyLValue o <> op "." <> fld (pretty x)



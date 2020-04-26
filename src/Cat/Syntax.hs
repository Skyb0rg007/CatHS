{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE ViewPatterns              #-}

{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module Cat.Syntax
    ( module Cat.Syntax
    , Int64
    ) where

import           Data.Bifunctor                            (second)
import           Data.Functor                              ((<&>))
import           Data.Functor.Classes                      (liftCompare, liftEq,
                                                            liftShowsPrec)
import           Data.IFunctor.Foldable
import           Data.Int                                  (Int64)
import           Data.Text                                 (Text)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Singlethongs
import qualified Text.Show                                 as Show

import           Cat.Common

-- * Types

{- In an ideal world...

data Program = Program [TopLevelDec]

data TopLevelDec
    = TyDecArray Text Ty
    | TyDecRecord Text [(Text, Ty)]
    | FunDec Text Ty [(Text, Ty)]

data Exp
    = ExpBreak
    | ExpIntLit Int
    | ExpStringLit Text
    | ExpLValue LValue
    | ExpSequence [Exp]
    | ExpNegate Exp
    | ExpInfix Exp InfixSourceOp Exp
    | ExpArrayCreate Ty Exp Exp
    | ExpRecordCreate Ty [(Text, Exp)]
    | ExpAssign LValue Exp
    | ExpIfThenElse Exp Exp Exp
    | ExpIfThen Exp Exp
    | ExpWhile Exp Exp
    | ExpFor Text Exp Exp Exp
    | ExpLet [Dec] Exp
    | ExpCall Text [Exp]

data Dec = VarDec Text Ty Exp

data LValue
    = LValueId Text
    | LValueSubscript LValue Exp
    | LValueFieldExp LValue Text

makeBaseFunctor "AST" [''Program, ''TopLevelDec, ''Exp, ''Dec, ''LValue]

-}

data ASTIdx = ExpIdx | DecIdx | LValueIdx | TopLevelDecIdx | ProgramIdx | TyIdx
singlethongs ''ASTIdx

-- * Pretty

-- | Annotations of the pretty expression
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

-- | Default terminal annotation
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

-- | Pretty-print a program
prettyProg :: Program -> Doc ExpAnn
prettyProg = ($ 0) . getConst . cata alg
    where
        kw, op, ty, fld, var, fun, int, str :: Doc ExpAnn -> Doc ExpAnn
        kw  = annotate AnnKeyword
        op  = annotate AnnOp
        ty  = annotate AnnTy
        fld = annotate AnnField
        var = annotate AnnVar
        fun = annotate AnnFun
        int = annotate AnnInt
        str = annotate AnnString
        parenIf :: Bool -> Doc ann -> Doc ann
        parenIf True  = parens
        parenIf False = id
        alg :: forall ix. SingI ix
            => AST (Const (Int -> Doc ExpAnn)) ix
            -> Const (Int -> Doc ExpAnn) ix
        alg x = Const $ \d ->
          case sing :: Sing ix of
            SExpIdx ->
              case x of
                ExpBreakF -> kw "break"
                ExpIntLitF n -> int (pretty n)
                ExpStringLitF s -> str (pretty s)
                ExpLValueF (Const l) -> l d
                ExpSequenceF [Const e] -> e d
                ExpSequenceF (map getConst -> es) -> encloseSep (op "(") (op ")") (op ";") (map ($ 0) es)
                ExpNegateF (Const e) -> "-" <> e 7
                ExpInfixF (Const e1) SOpMult (Const e2) -> parenIf (d > 6) $ e1 6 <+> op "*"   <+> e2 6
                ExpInfixF (Const e1) SOpDiv  (Const e2) -> parenIf (d > 6) $ e1 6 <+> op "/"   <+> e2 6
                ExpInfixF (Const e1) SOpAdd  (Const e2) -> parenIf (d > 5) $ e1 5 <+> op "+"   <+> e2 5
                ExpInfixF (Const e1) SOpSub  (Const e2) -> parenIf (d > 5) $ e1 5 <+> op "-"   <+> e2 5
                ExpInfixF (Const e1) SOpLt   (Const e2) -> parenIf (d > 4) $ e1 4 <+> op "<"   <+> e2 4
                ExpInfixF (Const e1) SOpGt   (Const e2) -> parenIf (d > 4) $ e1 4 <+> op ">"   <+> e2 4
                ExpInfixF (Const e1) SOpLte  (Const e2) -> parenIf (d > 4) $ e1 4 <+> op "<="  <+> e2 4
                ExpInfixF (Const e1) SOpGte  (Const e2) -> parenIf (d > 4) $ e1 4 <+> op ">="  <+> e2 4
                ExpInfixF (Const e1) SOpNeq  (Const e2) -> parenIf (d > 4) $ e1 4 <+> op "!="  <+> e2 4
                ExpInfixF (Const e1) SOpEq   (Const e2) -> parenIf (d > 4) $ e1 4 <+> op "="   <+> e2 4
                ExpInfixF (Const e1) SOpAnd  (Const e2) -> parenIf (d > 3) $ e1 3 <+> op "and" <+> e2 3
                ExpInfixF (Const e1) SOpOr   (Const e2) -> parenIf (d > 2) $ e1 2 <+> op "or"  <+> e2 2
                ExpArrayCreateF (Const t) (Const len) (Const init) ->
                    t 0 <> op "[" <> group (len 0) <> op "]" <+> kw "of" <+> group (init 0)
                ExpRecordCreateF (Const t) [] -> t 0 <> op "{}"
                ExpRecordCreateF (Const t) (map (second getConst) -> flds) ->
                    t 0 <> align (encloseSep (op "{") (op "}") (op ", ") (flds <&> \(lbl, e) -> fld (pretty lbl) <+> op "=" <+> e 0))
                ExpAssignF (Const lv) (Const e) ->
                    lv 0 <+> op ":=" <+> group (e 0)
                ExpIfThenElseF (Const e1) (Const e2) (Const e3) -> group $
                    flatAlt
                    (vsep [kw "if" <+> group (e1 0), indent 2 $ vsep [kw "then" <+> group (e2 0), kw "else" <+> group (e3 0)]])
                    (align $ hsep [kw "if", group (e1 0), kw "then", group (e2 0), kw "else", group (e3 0)])
                ExpIfThenF (Const e1) (Const e2) ->
                    kw "if" <+> e1 0 <+> kw "then" <+> e2 0
                ExpWhileF (Const cond) (Const body) ->
                    kw "while" <+> cond 0 <+> kw "do"
                    <> hardline <> indent 4 (body 0)
                ExpForF i (Const b) (Const e) (Const body) ->
                    kw "for" <+> var (pretty i) <+> op ":=" <+> b 0 <+> kw "to" <+> e 0 <+> kw "do"
                    <> hardline <> indent 4 (body 0)
                ExpLetF (map getConst -> binds) (Const body) ->
                    align $ vsep
                    [ kw "let"
                    , indent 2 $ vsep (map ($ 0) binds)
                    , kw "in"
                    , indent 2 $ body 0
                    , kw "end"
                    ]
                ExpCallF fn (map getConst -> args) ->
                    fun (pretty fn) <> align (encloseSep (op "(") (op ")") (op ", ") (map ($ 0) args))
            SDecIdx ->
              case x of
                VarDecF x (Const t) (Const e) ->
                    kw "var" <+> var (pretty x) <+> op ":" <+> t 0 <+> op ":=" <+> e 0
            SLValueIdx ->
              case x of
                LValueIdF x -> var (pretty x)
                LValueSubscriptF (Const lv) (Const i) -> lv 0 <> op "[" <> group (i 0) <> op "]"
                LValueFieldExpF (Const lv) x -> lv 0 <> op "." <> fld (pretty x)
            STopLevelDecIdx ->
              case x of
                TyDecArrayF name (Const t) ->
                    kw "type" <+> ty (pretty name) <+> op "="
                    <+> kw "array" <+> kw "of" <+> t 0
                TyDecRecordF name (map (second getConst) -> flds) ->
                    kw "type" <+> ty (pretty name) <+> op "="
                    <+> align (encloseSep (op "{ ") (op " }") (op ", ") $ flds <&> \(x, t) -> fld (pretty x) <+> op ":" <+> t 0)
                FunDecF fn (Const retTy) (map (second getConst) -> args) (Const body) ->
                    kw "function" <+> fun (pretty fn)
                    <+> group (align $ encloseSep (op "(") (op ")") (op ", ") $ args <&> \(x, t) -> var (pretty x) <+> op ":" <+> t 0)
                    <+> op "->" <+> retTy 0 <+> op "{" <> hardline
                    <> indent 4 (body 0) <> hardline
                    <> "}"
            SProgramIdx ->
              case x of
                ProgramF (map getConst -> decls) ->
                  vsep $ map ($ 0) decls
            STyIdx ->
              case x of
                TyF t -> ty (pretty t)

-- * recursion-schemes-ix boilerplate

data AST :: (ASTIdx -> *) -> ASTIdx -> * where
    -- Program
    ProgramF :: ![f 'TopLevelDecIdx] -> AST f 'ProgramIdx
    -- Ty
    TyF :: !Text -> AST f 'TyIdx
    -- TopLevelDec
    TyDecArrayF  :: !Text -> !(f 'TyIdx)                                        -> AST f 'TopLevelDecIdx
    TyDecRecordF :: !Text -> ![(Text, f 'TyIdx)]                                -> AST f 'TopLevelDecIdx
    FunDecF      :: !Text -> !(f 'TyIdx) -> ![(Text, f 'TyIdx)] -> !(f 'ExpIdx) -> AST f 'TopLevelDecIdx
    -- Exp
    ExpBreakF        ::                                                          AST f 'ExpIdx
    ExpIntLitF       :: !Int64                                                -> AST f 'ExpIdx
    ExpStringLitF    :: !Text                                                 -> AST f 'ExpIdx
    ExpLValueF       :: !(f 'LValueIdx)                                       -> AST f 'ExpIdx
    ExpSequenceF     :: ![f 'ExpIdx]                                          -> AST f 'ExpIdx
    ExpNegateF       :: !(f 'ExpIdx)                                          -> AST f 'ExpIdx
    ExpInfixF        :: !(f 'ExpIdx) -> !InfixSourceOp -> !(f 'ExpIdx)        -> AST f 'ExpIdx
    ExpArrayCreateF  :: !(f 'TyIdx) -> !(f 'ExpIdx) -> !(f 'ExpIdx)           -> AST f 'ExpIdx
    ExpRecordCreateF :: !(f 'TyIdx) -> ![(Text, f 'ExpIdx)]                   -> AST f 'ExpIdx
    ExpAssignF       :: !(f 'LValueIdx) -> !(f 'ExpIdx)                       -> AST f 'ExpIdx
    ExpIfThenElseF   :: !(f 'ExpIdx) -> !(f 'ExpIdx) -> !(f 'ExpIdx)          -> AST f 'ExpIdx
    ExpIfThenF       :: !(f 'ExpIdx) -> !(f 'ExpIdx)                          -> AST f 'ExpIdx
    ExpWhileF        :: !(f 'ExpIdx) -> !(f 'ExpIdx)                          -> AST f 'ExpIdx
    ExpForF          :: !Text -> !(f 'ExpIdx) -> !(f 'ExpIdx) -> !(f 'ExpIdx) -> AST f 'ExpIdx
    ExpLetF          :: ![f 'DecIdx] -> !(f 'ExpIdx)                          -> AST f 'ExpIdx
    ExpCallF         :: !Text -> ![f 'ExpIdx]                                 -> AST f 'ExpIdx
    -- Dec
    VarDecF :: !Text -> !(f 'TyIdx) -> !(f 'ExpIdx) -> AST f 'DecIdx
    -- LValue
    LValueIdF        :: !Text                           -> AST f 'LValueIdx
    LValueSubscriptF :: !(f 'LValueIdx) -> !(f 'ExpIdx) -> AST f 'LValueIdx
    LValueFieldExpF  :: !(f 'LValueIdx) -> !Text        -> AST f 'LValueIdx

type Exp         = IFix AST 'ExpIdx
type Dec         = IFix AST 'DecIdx
type LValue      = IFix AST 'LValueIdx
type Ty          = IFix AST 'TyIdx
type TopLevelDec = IFix AST 'TopLevelDecIdx
type Program     = IFix AST 'ProgramIdx

pattern Program :: [TopLevelDec] -> Program
pattern Program d = IFix (ProgramF d)
{-# COMPLETE Program #-}

pattern Ty :: Text -> Ty
pattern Ty x = IFix (TyF x)
{-# COMPLETE Ty #-}

pattern TyDecArray :: Text -> Ty -> TopLevelDec
pattern TyDecArray a b = IFix (TyDecArrayF a b)
pattern TyDecRecord :: Text -> [(Text, Ty)] -> TopLevelDec
pattern TyDecRecord a b = IFix (TyDecRecordF a b)
pattern FunDec :: Text -> Ty -> [(Text, Ty)] -> Exp -> TopLevelDec
pattern FunDec a b c d = IFix (FunDecF a b c d)
{-# COMPLETE TyDecArray, TyDecRecord, FunDec #-}

pattern ExpBreak :: Exp
pattern ExpBreak = IFix ExpBreakF
pattern ExpIntLit :: Int64 -> Exp
pattern ExpIntLit n = IFix (ExpIntLitF n)
pattern ExpStringLit :: Text -> Exp
pattern ExpStringLit s = IFix (ExpStringLitF s)
pattern ExpLValue :: LValue -> Exp
pattern ExpLValue l = IFix (ExpLValueF l)
pattern ExpSequence :: [Exp] -> Exp
pattern ExpSequence s = IFix (ExpSequenceF s)
pattern ExpNegate :: Exp -> Exp
pattern ExpNegate e = IFix (ExpNegateF e)
pattern ExpInfix :: Exp -> InfixSourceOp -> Exp -> Exp
pattern ExpInfix e1 op e2 = IFix (ExpInfixF e1 op e2)
pattern ExpArrayCreate  :: Ty -> Exp -> Exp -> Exp
pattern ExpArrayCreate t e1 e2 = IFix (ExpArrayCreateF t e1 e2)
pattern ExpRecordCreate :: Ty -> [(Text, Exp)] -> Exp
pattern ExpRecordCreate t r = IFix (ExpRecordCreateF t r)
pattern ExpAssign :: LValue -> Exp -> Exp
pattern ExpAssign l t = IFix (ExpAssignF l t)
pattern ExpIfThenElse :: Exp -> Exp -> Exp -> Exp
pattern ExpIfThenElse e1 e2 e3 = IFix (ExpIfThenElseF e1 e2 e3)
pattern ExpIfThen :: Exp -> Exp -> Exp
pattern ExpIfThen e1 e2 = IFix (ExpIfThenF e1 e2)
pattern ExpWhile :: Exp -> Exp -> Exp
pattern ExpWhile e1 e2 = IFix (ExpWhileF e1 e2)
pattern ExpFor :: Text -> Exp -> Exp -> Exp -> Exp
pattern ExpFor i s e b = IFix (ExpForF i s e b)
pattern ExpLet :: [Dec] -> Exp -> Exp
pattern ExpLet d e = IFix (ExpLetF d e)
pattern ExpCall :: Text -> [Exp] -> Exp
pattern ExpCall f a = IFix (ExpCallF f a)
{-# COMPLETE ExpBreak, ExpIntLit, ExpStringLit, ExpLValue, ExpSequence, ExpNegate, ExpInfix, ExpArrayCreate, ExpRecordCreate, ExpAssign, ExpIfThenElse, ExpIfThen, ExpWhile, ExpFor, ExpLet, ExpCall #-}

pattern VarDec :: Text -> Ty -> Exp -> Dec
pattern VarDec x t e = IFix (VarDecF x t e)
{-# COMPLETE VarDec #-}

pattern LValueId :: Text -> LValue
pattern LValueId x = IFix (LValueIdF x)
pattern LValueSubscript :: LValue -> Exp -> LValue
pattern LValueSubscript l f = IFix (LValueSubscriptF l f)
pattern LValueFieldExp  :: LValue -> Text -> LValue
pattern LValueFieldExp l x = IFix (LValueFieldExpF l x)
{-# COMPLETE LValueId, LValueSubscript, LValueFieldExp #-}

-- * Recursion-schemes-ix

instance IFunctor AST where
    imap = imapDefault
    {-# INLINE imap #-}

instance ITraversable AST where
    itraverse :: forall ix m a b. (SingI ix, Applicative m)
              => (forall ix. SingI ix => a ix -> m (b ix))
              -> AST a ix
              -> m (AST b ix)
    itraverse f =
      case sing :: Sing ix of
        SProgramIdx -> \case
          ProgramF tldecs -> ProgramF <$> traverse f tldecs
        STyIdx -> \case
          TyF x -> pure $ TyF x
        STopLevelDecIdx -> \case
          TyDecArrayF name ty        -> TyDecArrayF name <$> f ty
          TyDecRecordF name flds     -> TyDecRecordF name <$> (traverse . traverse) f flds
          FunDecF fn retTy args body -> FunDecF fn <$> f retTy <*> (traverse . traverse) f args <*> f body
        SExpIdx -> \case
          ExpBreakF               -> pure ExpBreakF
          ExpIntLitF n            -> pure $ ExpIntLitF n
          ExpStringLitF s         -> pure $ ExpStringLitF s
          ExpLValueF lv           -> ExpLValueF       <$> f lv
          ExpSequenceF es         -> ExpSequenceF     <$> traverse f es
          ExpNegateF e            -> ExpNegateF       <$> f e
          ExpInfixF e1 op e2      -> ExpInfixF        <$> f e1   <*> pure op <*> f e2
          ExpArrayCreateF t l e   -> ExpArrayCreateF  <$> f t    <*> f l     <*> f e
          ExpRecordCreateF t fs   -> ExpRecordCreateF <$> f t    <*> (traverse . traverse) f fs
          ExpAssignF lv e         -> ExpAssignF       <$> f lv   <*> f e
          ExpIfThenElseF e1 e2 e3 -> ExpIfThenElseF   <$> f e1   <*> f e2    <*> f e3
          ExpIfThenF e1 e2        -> ExpIfThenF       <$> f e1   <*> f e2
          ExpWhileF cond e        -> ExpWhileF        <$> f cond <*> f e
          ExpForF i s e body      -> ExpForF i        <$> f s    <*> f e     <*> f body
          ExpLetF decs body       -> ExpLetF          <$> traverse f decs    <*> f body
          ExpCallF fn args        -> ExpCallF fn      <$> traverse f args
        SDecIdx -> \case
          VarDecF x ty e -> VarDecF x <$> f ty <*> f e
        SLValueIdx -> \case
          LValueIdF x           -> pure $ LValueIdF x
          LValueSubscriptF lv i -> LValueSubscriptF <$> f lv <*> f i
          LValueFieldExpF lv x  -> LValueFieldExpF  <$> f lv <*> pure x
    {-# INLINE itraverse #-}

instance IShow AST where
    ishowsPrec :: forall ix a. SingI ix
              => (forall ix. SingI ix => Int -> a ix -> ShowS)
              -> Int -> AST a ix -> ShowS
    ishowsPrec sp d =
      case sing :: Sing ix of
        SProgramIdx -> \case
          ProgramF tldecs -> showParen (d > 10) $ showString "ProgramF " . Show.showListWith (sp 0) tldecs
        STyIdx -> \case
          TyF x -> showParen (d > 10) $ showString "TyF " . showsPrec 11 x
        STopLevelDecIdx -> \case
          TyDecArrayF name ty -> showParen (d > 10) $
              showString "TyDecArrayF " . showsPrec 11 name . showString " " . sp 11 ty
          TyDecRecordF name flds -> showParen (d > 10) $
              showString "TyDecRecordF " . showsPrec 11 name . showString " " . Show.showListWith (showTupleWith (sp 0)) flds
          FunDecF fn retTy args body -> showParen (d > 10) $
              showString "FunDecF " . showsPrec 11 fn . showString " " . sp 11 retTy . showString " " . Show.showListWith (showTupleWith (sp 0)) args . showString " " . sp 11 body
        SExpIdx -> \case
          ExpBreakF -> showString "ExpBreakF"
          ExpIntLitF n -> showParen (d > 10) $ showString "ExpIntLitF " . showsPrec 11 n
          ExpStringLitF s -> showParen (d > 10) $ showString "ExpStringLitF " . showsPrec 11 s
          ExpLValueF lv -> showParen (d > 10) $ showString "ExpLValueF " . sp 11 lv
          ExpSequenceF es -> showParen (d > 10) $ showString "ExpSequenceF " . Show.showListWith (sp 0) es
          ExpNegateF e -> showParen (d > 10) $ showString "ExpNegateF " . sp 11 e
          ExpInfixF e1 op e2 -> showParen (d > 10) $ showString "ExpInfixF " . sp 11 e1 . showString " " . showsPrec 11 op . showString " " . sp 11 e2
          ExpArrayCreateF t l e -> showParen (d > 10) $ showString "ExpArrayCreateF " . sp 11 t . showString " " . sp 11 l . showString " " . sp 11 e
          ExpRecordCreateF t fs -> showParen (d > 10) $ showString "ExpRecordCreateF " . sp 11 t . showString " " . Show.showListWith (showTupleWith (sp 0)) fs
          ExpAssignF lv e -> showParen (d > 10) $ showString "ExpAssignF " . sp 11 lv . showString " " . sp 11 e
          ExpIfThenElseF e1 e2 e3 -> showParen (d > 10) $ showString "ExpIfThenElseF " . sp 11 e1 . showString " " . sp 11 e2 . showString " " . sp 11 e3
          ExpIfThenF e1 e2 -> showParen (d > 10) $ showString "ExpIfThenF " . sp 11 e1 . showString " " . sp 11 e2
          ExpWhileF cond e -> showParen (d > 10) $ showString "ExpWhileF " . sp 11 cond . showString " " . sp 11 e
          ExpForF i s e body -> showParen (d > 10) $ showString "ExpForF " . showsPrec 11 i . showString " " .  sp 11 s . showString " " . sp 11 e . showString " " . sp 11 body
          ExpLetF decs body -> showParen (d > 10) $ showString "ExpLetF " . Show.showListWith (sp 0) decs . showString " " . sp 11 body
          ExpCallF fn args -> showParen (d > 10) $ showString "ExpCallF " . showsPrec 11 fn . showString " " . Show.showListWith (sp 0) args
        SDecIdx -> \case
          VarDecF x ty e -> showParen (d > 10) $ showString "VarDecF " . showsPrec 11 x . showString " " . sp 11 ty . showString " " . sp 11 e
        SLValueIdx -> \case
          LValueIdF x -> showParen (d > 10) $ showString "LValueIdF " . showsPrec 11 x
          LValueSubscriptF lv i -> showParen (d > 10) $ showString "LValueSubscriptF " . sp 11 lv . showString " " . sp 11 i
          LValueFieldExpF lv x -> showParen (d > 10) $ showString "LValueFieldExpF " . sp 11 lv . showString " " . showsPrec 11 x
      where
        showTupleWith :: forall a b. Show a => (b -> ShowS) -> (a, b) -> ShowS
        showTupleWith sp x = liftShowsPrec (const sp) (Show.showListWith sp) 0 x

instance {-# OVERLAPPING #-} Show Program where
    showsPrec d (Program decs) = showParen (d > 10) $
        showString "Program " . showsPrec 11 decs
instance {-# OVERLAPPING #-} Show TopLevelDec where
    showsPrec d (TyDecArray name ty) = showParen (d > 10) $
        showString "TyDecArray " . showsPrec 11 name
      . showString " " . showsPrec 11 ty
    showsPrec d (TyDecRecord name flds) = showParen (d > 10) $
        showString "TyDecRecord " . showsPrec 11 name
      . showString " " . showsPrec 11 flds
    showsPrec d (FunDec name retTy args body) = showParen (d > 10) $
        showString "FunDec " . showsPrec 11 name
      . showString " " . showsPrec 11 retTy
      . showString " " . showsPrec 11 args
      . showString " " . showsPrec 11 body
instance {-# OVERLAPPING #-} Show Exp where
    showsPrec _ ExpBreak = showString "ExpBreak"
    showsPrec d (ExpIntLit n) = showParen (d > 10) $
        showString "ExpIntLit " . showsPrec 11 n
    showsPrec d (ExpStringLit s) = showParen (d > 10) $
        showString "ExpStringLit " . showsPrec 11 s
    showsPrec d (ExpLValue lv) = showParen (d > 10) $
        showString "ExpLValue " . showsPrec 11 lv
    showsPrec d (ExpSequence es) = showParen (d > 10) $
        showString "ExpSequence " . showsPrec 11 es
    showsPrec d (ExpNegate e) = showParen (d > 10) $
        showString "ExpNegate " . showsPrec 11 e
    showsPrec d (ExpInfix e1 op e2) = showParen (d > 10) $
        showString "ExpInfix " . showsPrec 11 e1
      . showString " " . showsPrec 11 op
      . showString " " . showsPrec 11 e2
    showsPrec d (ExpArrayCreate lv l e) = showParen (d > 10) $
        showString "ExpArrayCreate " . showsPrec 11 lv
      . showString " " . showsPrec 11 l
      . showString " " . showsPrec 11 e
    showsPrec d (ExpRecordCreate ty flds) = showParen (d > 10) $
        showString "ExpRecordCreate " . showsPrec 11 ty
      . showString " " . showsPrec 11 flds
    showsPrec d (ExpAssign lv e) = showParen (d > 10) $
        showString "ExpAssign " . showsPrec 11 lv
      . showString " " . showsPrec 11 e
    showsPrec d (ExpIfThenElse e1 e2 e3) = showParen (d > 10) $
        showString "ExpIfThenElse " . showsPrec 11 e1
      . showString " " . showsPrec 11 e2
      . showString " " . showsPrec 11 e3
    showsPrec d (ExpIfThen e1 e2) = showParen (d > 10) $
        showString "ExpIfThen " . showsPrec 11 e1
      . showString " " . showsPrec 11 e2
    showsPrec d (ExpWhile e1 e2) = showParen (d > 10) $
        showString "ExpWhile " . showsPrec 11 e1
      . showString " " . showsPrec 11 e2
    showsPrec d (ExpFor i s e body) = showParen (d > 10) $
        showString "ExpFor " . showsPrec 11 i
      . showString " " . showsPrec 11 s
      . showString " " . showsPrec 11 e
      . showString " " . showsPrec 11 body
    showsPrec d (ExpLet decs body) = showParen (d > 10) $
        showString "ExpLet " . showsPrec 11 decs
      . showString " " . showsPrec 11 body
    showsPrec d (ExpCall fn args) = showParen (d > 10) $
        showString "ExpCall " . showsPrec 11 fn
      . showString " " . showsPrec 11 args
instance {-# OVERLAPPING #-} Show Dec where
    showsPrec d (VarDec x ty e) = showParen (d > 10) $
        showString "VarDec " . showsPrec 11 x
      . showString " " . showsPrec 11 ty
      . showString " " . showsPrec 11 e
instance {-# OVERLAPPING #-} Show LValue where
    showsPrec d (LValueId x) = showParen (d > 10) $
        showString "LValueId " . showsPrec 11 x
    showsPrec d (LValueSubscript lv i) = showParen (d > 10) $
        showString "LValueSubscript " . showsPrec 11 lv
      . showString " " . showsPrec 11 i
    showsPrec d (LValueFieldExp lv x) = showParen (d > 10) $
        showString "LValueFieldExp " . showsPrec 11 lv
      . showString " " . showsPrec 11 x

instance IEq AST where
    ieq :: forall ix a. SingI ix
        => (forall ix. SingI ix => a ix -> a ix -> Bool)
        -> AST a ix -> AST a ix -> Bool
    ieq eq x y =
      case sing :: Sing ix of
        SProgramIdx ->
          case (x, y) of
            (ProgramF x, ProgramF y) -> liftEq eq x y
        STyIdx ->
          case (x, y) of
            (TyF x, TyF y) -> x == y
        STopLevelDecIdx ->
          case (x, y) of
            (TyDecArrayF name ty, TyDecArrayF name' ty') -> name == name' && ty `eq` ty'
            (TyDecRecordF name flds, TyDecRecordF name' flds') -> name == name' && (liftEq . liftEq) eq flds flds'
            (FunDecF fn retTy args body, FunDecF fn' retTy' args' body') ->
                fn == fn' && retTy `eq` retTy' && (liftEq . liftEq) eq args args' && body `eq` body'
            _ -> False
        SExpIdx ->
          case (x, y) of
            (ExpBreakF, ExpBreakF) -> True
            (ExpIntLitF n, ExpIntLitF n') -> n == n'
            (ExpStringLitF s, ExpStringLitF s') -> s == s'
            (ExpLValueF lv, ExpLValueF lv') -> lv `eq` lv'
            (ExpSequenceF es, ExpSequenceF es') -> liftEq eq es es'
            (ExpNegateF e, ExpNegateF e') -> e `eq` e'
            (ExpInfixF e1 op e2, ExpInfixF e1' op' e2') -> e1 `eq` e1' && op == op' && e2 `eq` e2'
            (ExpArrayCreateF t l e, ExpArrayCreateF t' l' e') ->
                t `eq` t' && l `eq` l' && e `eq` e'
            (ExpRecordCreateF t fs, ExpRecordCreateF t' fs') ->
                t `eq` t' && (liftEq . liftEq) eq fs fs'
            (ExpAssignF lv e, ExpAssignF lv' e') -> lv `eq` lv' && e `eq` e'
            (ExpIfThenElseF e1 e2 e3, ExpIfThenElseF e1' e2' e3') ->
                e1 `eq` e1' && e2 `eq` e2' && e3 `eq` e3'
            (ExpIfThenF e1 e2, ExpIfThenF e1' e2') ->
                e1 `eq` e1' && e2 `eq` e2'
            (ExpWhileF e1 e2, ExpWhileF e1' e2') ->
                e1 `eq` e1' && e2 `eq` e2'
            (ExpForF i s e b, ExpForF i' s' e' b') ->
                i == i' && s `eq` s' && e `eq` e' && b `eq` b'
            (ExpLetF ds b, ExpLetF ds' b') ->
                liftEq eq ds ds' && b `eq` b'
            (ExpCallF f as, ExpCallF f' as') ->
                f == f' && liftEq eq as as'
            _ -> False
        SDecIdx ->
          case (x, y) of
            (VarDecF x t e, VarDecF x' t' e') -> x == x' && t `eq` t' && e `eq` e'
        SLValueIdx ->
          case (x, y) of
            (LValueIdF x, LValueIdF y) -> x == y
            (LValueSubscriptF lv i, LValueSubscriptF lv' i') -> lv `eq` lv' && i `eq` i'
            (LValueFieldExpF lv x, LValueFieldExpF lv' x') -> lv `eq` lv' && x == x'
            _ -> False

instance IOrd AST where
    icompare :: forall ix a. SingI ix
             => (forall ix. SingI ix => a ix -> a ix -> Ordering)
             -> AST a ix -> AST a ix -> Ordering
    icompare comp x y =
      case sing :: Sing ix of
        SProgramIdx ->
          case (x, y) of
            (ProgramF x, ProgramF y) -> liftCompare comp x y
        STyIdx ->
          case (x, y) of
            (TyF x, TyF y) -> compare x y
        STopLevelDecIdx ->
          let ord :: AST a 'TopLevelDecIdx -> Int
              ord TyDecArrayF{}  = 0
              ord TyDecRecordF{} = 1
              ord FunDecF{}      = 2
           in 
          case (x, y) of
            (TyDecArrayF name ty, TyDecArrayF name' ty') -> compare name name' <> comp ty ty'
            (TyDecRecordF name flds, TyDecRecordF name' flds') -> compare name name' <> (liftCompare . liftCompare) comp flds flds'
            (FunDecF fn retTy args body, FunDecF fn' retTy' args' body') ->
                compare fn fn' <> comp retTy retTy' <> (liftCompare . liftCompare) comp args args' <> comp body body'
            (x, y) -> ord x `compare` ord y
        SExpIdx ->
          let ord :: AST a 'ExpIdx -> Int
              ord ExpBreakF{}        = 0
              ord ExpIntLitF{}       = 1
              ord ExpStringLitF{}    = 2
              ord ExpLValueF{}       = 3
              ord ExpSequenceF{}     = 4
              ord ExpNegateF{}       = 5
              ord ExpInfixF{}        = 6
              ord ExpArrayCreateF{}  = 7
              ord ExpRecordCreateF{} = 8
              ord ExpAssignF{}       = 9
              ord ExpIfThenElseF{}   = 10
              ord ExpIfThenF{}       = 11
              ord ExpWhileF{}        = 12
              ord ExpForF{}          = 13
              ord ExpLetF{}          = 14
              ord ExpCallF{}         = 15
           in 
          case (x, y) of
            (ExpBreakF, ExpBreakF) -> EQ
            (ExpIntLitF n, ExpIntLitF n') -> n `compare` n'
            (ExpStringLitF s, ExpStringLitF s') -> s `compare` s'
            (ExpLValueF lv, ExpLValueF lv') -> lv `comp` lv'
            (ExpSequenceF es, ExpSequenceF es') -> liftCompare comp es es'
            (ExpNegateF e, ExpNegateF e') -> e `comp` e'
            (ExpInfixF e1 op e2, ExpInfixF e1' op' e2') -> e1 `comp` e1' <> op `compare` op' <> e2 `comp` e2'
            (ExpArrayCreateF t l e, ExpArrayCreateF t' l' e') ->
                t `comp` t' <> l `comp` l' <> e `comp` e'
            (ExpRecordCreateF t fs, ExpRecordCreateF t' fs') ->
                t `comp` t' <> (liftCompare . liftCompare) comp fs fs'
            (ExpAssignF lv e, ExpAssignF lv' e') -> lv `comp` lv' <> e `comp` e'
            (ExpIfThenElseF e1 e2 e3, ExpIfThenElseF e1' e2' e3') ->
                e1 `comp` e1' <> e2 `comp` e2' <> e3 `comp` e3'
            (ExpIfThenF e1 e2, ExpIfThenF e1' e2') ->
                e1 `comp` e1' <> e2 `comp` e2'
            (ExpWhileF e1 e2, ExpWhileF e1' e2') ->
                e1 `comp` e1' <> e2 `comp` e2'
            (ExpForF i s e b, ExpForF i' s' e' b') ->
                i `compare` i' <> s `comp` s' <> e `comp` e' <> b `comp` b'
            (ExpLetF ds b, ExpLetF ds' b') ->
                liftCompare comp ds ds' <> b `comp` b'
            (ExpCallF f as, ExpCallF f' as') ->
                f `compare` f' <> liftCompare comp as as'
            _ -> ord x `compare` ord y
        SDecIdx ->
          case (x, y) of
            (VarDecF x t e, VarDecF x' t' e') -> x `compare` x' <> t `comp` t' <> e `comp` e'
        SLValueIdx ->
          let ord :: AST a 'LValueIdx -> Int
              ord LValueIdF{}        = 0
              ord LValueSubscriptF{} = 1
              ord LValueFieldExpF{}  = 2
           in 
          case (x, y) of
            (LValueIdF x, LValueIdF y) -> x `compare` y
            (LValueSubscriptF lv i, LValueSubscriptF lv' i') -> lv `comp` lv' <> i `comp` i'
            (LValueFieldExpF lv x, LValueFieldExpF lv' x') -> lv `comp` lv' <> x `compare` x'
            _ -> ord x `compare` ord y


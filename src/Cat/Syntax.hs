
module Cat.Syntax
    ( 
    ) where

import Data.Text (Text)
import Control.Lens.TH
import Data.Functor.Foldable.TH
import Data.Int (Int64)

import Cat.Common

type Program = [TopLevelDec]
type Ty = Text

data TopLevelDec
    = TyDecArray {- New Type -} !Text {- Element Type -} !Ty
    | TyDecRecord {- New Type -} !Text {- Fields -} ![(Text, Ty)]
    | FunDec {- Name -} !Text {- Return Type -} !Ty {- Args -} ![(Text, Ty)] {- Body -} !Exp

data Exp
    = ExpBreak
    | ExpIntLit !Int64
    | ExpStringLit !Text
    | ExpLValue !LValue
    | ExpSequence ![Exp]
    | ExpNegate !Exp
    | ExpInfix !Exp !InfixOp !Exp
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

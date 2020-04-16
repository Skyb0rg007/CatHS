
module Cat.CheckedSyntax
    ( module Cat.CheckedSyntax
    ) where

import Data.Text (Text)
import Control.Lens.TH
import Data.Functor.Foldable.TH
import Data.Int (Int64)

import Cat.Common

type Program = [TopLevelDec]

data TopLevelDec
    = FunDec {- Name -} !Text {- Args -} ![Symbol] {- Body -} !Exp

data Exp
    = ExpBreak
    | ExpIntLit !Int64
    | ExpStringLit !Text
    | ExpLValue !LValue
    | ExpSequence ![Exp]
    | ExpNegate !Exp
    | ExpInfix !Exp !InfixOp !Exp
    | ExpArrayCreate {- Length -} !Exp {- Initial Value -} !Exp
    | ExpRecordCreate {- Fields -} ![(Text, Exp)]
    | ExpAssign !LValue !Exp
    | ExpIfThenElse !Exp !Exp !Exp
    | ExpIfThen !Exp !Exp
    | ExpWhile !Exp !Exp
    | ExpFor !Text !Exp !Exp !Exp -- for name = e1 to e2 do e3
    | ExpLet ![Dec] !Exp
    | ExpCall !Text ![Exp]
    deriving (Show, Eq, Ord)

data Dec
    = VarDec {- Name -} !Text {- Value -} !Exp
    deriving (Show, Eq, Ord)

data LValue
    = LValueId !Symbol -- x
    | LValueSubscript !LValue !Exp -- arr[x]
    | LValueFieldExp !LValue !Text -- obj.x
    deriving (Show, Eq, Ord)


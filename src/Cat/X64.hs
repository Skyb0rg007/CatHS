{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_GHC -Wall #-}

module Cat.X64
    (
    -- * Representation of an assembly label
      Label (..)
    -- * X64 registers
    , Register (..)
    -- * Memory access operation, including offsets
    , Addr (..)
    -- * Type-level tag for whether an operand is writable
    , Access (..)
    -- * Monad-transformer for compiling code
    , CodeMT (..)
    -- * Monad-transformer runner
    , runCodeMT
    , CodeM
    , runCodeM
    , MonadCode (..)
    -- * Supported conditionals
    , Condition (..)
    -- * Negate a Condition
    , notCond
    -- * Immediate operands
    , Immediate (..)
    -- * Operands, can be read-only (with tag 'R)
    , Operand (..)
    -- * Create an operand from a label
    , pattern LblOp
    -- * Create an operand from an Int64
    , pattern IntOp
    -- * Create an operand from a memory access of a register ex. (%rax)
    , pattern MemRegOp
    -- * Instructions
    , ret, nop, add, sub, or, and, mov,  cmp, shl, lea, imul, idiv, push, call
    , neg, pop, jmp, j
    -- * Pseudo instructions
    , global
    , rawLabel
    -- * High-Level Helpers
    , function
    , callFunction
    -- * Compile a program
    , compileProg
    , def
    -- * Lenses
    , addrBaseReg, addrDisplacement, addrIndexReg
    ) where

-- Inspired by the X86-64bit package from Hackage

import           Control.Lens.TH
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
import           Data.Default
import           Data.Foldable               (foldl')
import           Data.Functor.Identity
import           Data.Int                    (Int32, Int64)
import           Data.Sequence               (Seq)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Data.Word                   (Word8)
import           Prelude                     hiding (and, or)

data Label = Label !Int | LabelRaw !Text
    deriving (Eq, Ord)
instance Show Label where
    show (Label i) = "lbl_" ++ show i
    show (LabelRaw l) = Text.unpack l

data Register
    = Rax
    | Rbx
    | Rcx
    | Rdx
    | Rsp
    | Rsi
    | Rdi
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15
    deriving (Show, Eq, Ord, Enum, Read)

tmpReg :: Register
tmpReg = R15

data Addr = Addr
    { _addrBaseReg      :: Maybe Register
    , _addrDisplacement :: Maybe Int32
    , _addrIndexReg     :: Maybe (Word8, Register)
    } deriving (Show, Eq, Ord, Read)

instance Default Addr where
    def = Addr Nothing Nothing Nothing

data Access = R | RW

-- Use a GADT to prevent writing to immediate values
data Operand :: Access -> * where
    ImmOp :: Immediate -> Operand 'R
    RegOp :: Register  -> Operand rw
    MemOp :: Addr      -> Operand rw

pattern LblOp :: Label -> Operand 'R
pattern LblOp l = ImmOp (LabelRef l)
pattern IntOp :: Int64 -> Operand 'R
pattern IntOp n = ImmOp (Immediate n)
pattern MemRegOp :: Register -> Operand rw
pattern MemRegOp reg = MemOp (Addr (Just reg) Nothing Nothing)

data Immediate = Immediate !Int64 | LabelRef !Label

data CodeLine where
    Ret, Nop                         :: CodeLine
    Add, Sub, Or, And, Mov, Cmp, Shl :: Operand r -> Operand 'RW -> CodeLine
    Lea                              :: Operand 'RW -> Operand 'RW -> CodeLine
    IMul, IDiv, Push, Call           :: Operand r -> CodeLine
    Neg, Pop                         :: Operand 'RW -> CodeLine
    Jmp                              :: Label -> CodeLine
    J                                :: Condition -> Label -> CodeLine
    Lbl                              :: Int -> CodeLine
    Raw                              :: Text -> CodeLine

data Condition
    = E
    | NE
    | G
    | GE
    | L
    | LE
    deriving (Show, Eq, Ord, Enum, Read)

notCond :: Condition -> Condition
notCond E = NE
notCond NE = E
notCond G = LE
notCond GE = L
notCond L = GE
notCond LE = G

class MonadFix m => MonadCode m where
    tellCodeLine :: CodeLine -> m ()
    label :: m Label

newtype CodeMT m a = CodeMT
    { unCodeMT :: StateT Int (WriterT (Seq CodeLine) m) a
    } deriving newtype ( Functor
                       , Applicative
                       , Monad
                       , MonadFix
                       )

instance MonadFix m => MonadCode (CodeMT m) where
    tellCodeLine c = CodeMT $ tell $ pure c
    label = CodeMT $ do
        i <- get
        put (i + 1)
        tell $ pure $ Lbl i
        pure $ Label i


type CodeM = CodeMT Identity

runCodeMT :: Monad m => CodeMT m a -> m (a, Seq CodeLine)
runCodeMT (CodeMT m) = runWriterT $ evalStateT m 0

runCodeM :: CodeM a -> (a, Seq CodeLine)
runCodeM = runIdentity . runCodeMT

-- Ops

ret, nop :: MonadCode m => m ()
ret = tellCodeLine Ret
nop = tellCodeLine Nop

add, sub, or, and, mov, cmp, shl
    :: MonadCode m => Operand r -> Operand 'RW -> m ()
add (MemOp a) (MemOp b) = do
    tellCodeLine $ Mov (MemOp a) (RegOp tmpReg)
    tellCodeLine $ Add (RegOp tmpReg) (MemOp b)
add a b = tellCodeLine $ Add a b
sub (MemOp a) (MemOp b) = do
    tellCodeLine $ Mov (MemOp a) (RegOp tmpReg)
    tellCodeLine $ Sub (RegOp tmpReg) (MemOp b)
sub a b = tellCodeLine $ Sub a b
or  (MemOp a) (MemOp b) = do
    tellCodeLine $ Mov (MemOp a) (RegOp tmpReg)
    tellCodeLine $ Or  (RegOp tmpReg) (MemOp b)
or  a b = tellCodeLine $ Or  a b
and (MemOp a) (MemOp b) = do
    tellCodeLine $ Mov (MemOp a) (RegOp tmpReg)
    tellCodeLine $ And (RegOp tmpReg) (MemOp b)
and a b = tellCodeLine $ And a b
mov (MemOp a) (MemOp b) = do
    tellCodeLine $ Mov (MemOp a) (RegOp tmpReg)
    tellCodeLine $ Mov (RegOp tmpReg) (MemOp b)
mov a b = tellCodeLine $ Mov a b
cmp (MemOp a) (MemOp b) = do
    tellCodeLine $ Mov (MemOp a) (RegOp tmpReg)
    tellCodeLine $ Cmp (RegOp tmpReg) (MemOp b)
cmp a b = tellCodeLine $ Cmp a b
shl (MemOp a) (MemOp b) = do
    tellCodeLine $ Mov (MemOp a) (RegOp tmpReg)
    tellCodeLine $ Shl (RegOp tmpReg) (MemOp b)
shl a b = tellCodeLine $ Shl a b

lea :: MonadCode m => Operand 'RW -> Operand 'RW -> m ()
lea a b = tellCodeLine $ Lea a b

imul, idiv, push, call :: MonadCode m => Operand r -> m ()
imul a = tellCodeLine $ IMul a
idiv a = tellCodeLine $ IDiv a
push a = tellCodeLine $ Push a
call a = tellCodeLine $ Call a

neg, pop :: MonadCode m => Operand 'RW -> m ()
neg a = tellCodeLine $ Neg a
pop a = tellCodeLine $ Pop a

jmp :: MonadCode m => Label -> m ()
jmp l = tellCodeLine $ Jmp l

j :: MonadCode m => Condition -> Label -> m ()
j c l = tellCodeLine $ J c l

global :: MonadCode m => Label -> m ()
global lbl = tellCodeLine $ Raw $ ".global " <> Text.pack (show lbl)

rawLabel :: MonadCode m => Text -> m Label
rawLabel t = do
    let lbl = LabelRaw t
    tellCodeLine $ Raw $ Text.pack (show lbl) <> ":"
    pure lbl

-- * Compilation

compileReg :: Register -> Text
compileReg = \case
    Rax -> "%rax"
    Rbx -> "%rbx"
    Rcx -> "%rcx"
    Rdx -> "%rdx"
    Rsp -> "%rsp"
    Rsi -> "%rsi"
    Rdi -> "%rdi"
    R8  -> "%r8"
    R9  -> "%r9"
    R10 -> "%r10"
    R11 -> "%r11"
    R12 -> "%r12"
    R13 -> "%r13"
    R14 -> "%r14"
    R15 -> "%r15"

compileAddr :: Addr -> Text
compileAddr = \case
    Addr Nothing Nothing Nothing ->
        "0"
    Addr Nothing (Just disp) Nothing ->
        Text.pack (show disp)
    Addr (Just reg) Nothing Nothing ->
        "(" <> compileReg reg <> ")"
    Addr (Just reg) (Just disp) Nothing ->
        Text.pack (show disp) <> "(" <> compileReg reg <> ")"
    Addr (Just reg) (Just disp) (Just (scale, index)) ->
        Text.pack (show disp) <>
        "(" <> compileReg reg <>
        ", " <> Text.pack (show scale) <>
        ", " <> compileReg index <>
        ")"
    addr -> error $ "Ill-formed Addr: " <> show addr

compileOp :: Operand r -> Text
compileOp = \case
    ImmOp (Immediate n) -> "$" <> Text.pack (show n)
    ImmOp (LabelRef l) -> Text.pack (show l)
    RegOp reg -> compileReg reg
    MemOp addr -> compileAddr addr


compileLine :: CodeLine -> Text
compileLine = \case
    Ret     -> "\tret"
    Nop     -> "\tnop"
    Add a b -> "\tadd "  <> compileOp a <> ", " <> compileOp b
    Sub a b -> "\tsub "  <> compileOp a <> ", " <> compileOp b
    Or  a b -> "\tor "   <> compileOp a <> ", " <> compileOp b
    And a b -> "\tand "  <> compileOp a <> ", " <> compileOp b
    Mov a b -> "\tmovq " <> compileOp a <> ", " <> compileOp b
    Cmp a b -> "\tcmp "  <> compileOp a <> ", " <> compileOp b
    Shl a b -> "\tshl "  <> compileOp a <> ", " <> compileOp b
    Lea a b -> "\tlea "  <> compileOp a <> ", " <> compileOp b
    IMul a  -> "\timulq " <> compileOp a
    IDiv a  -> "\tidiv " <> compileOp a
    Push a  -> "\tpush " <> compileOp a
    Call a  -> "\tcall " <> compileOp a
    Neg a   -> "\tneg "  <> compileOp a
    Pop a   -> "\tpop "  <> compileOp a
    Jmp l   -> "\tjmp " <> Text.pack (show l)
    J c l   -> "\tj" <> Text.toLower (Text.pack (show c)) <> " " <> Text.pack (show l)
    Lbl l   -> "lbl_" <> Text.pack (show l) <> ":"
    Raw x   -> x

compileProg :: Seq CodeLine -> Text
compileProg = foldl' (\acc c -> acc <> "\n" <> compileLine c) ""


-- * Helpers

abi :: [Register]
abi =
    [ Rdi
    , Rsi
    , Rdx
    , Rcx
    , R8
    , R9
    ]

-- | Define a function with n arguments
-- Parameters are allocated on the stack
function :: MonadCode m => Int -> Int -> ([Operand 'RW] -> [Operand 'RW] -> Operand 'RW -> m a) -> m Label
function arity numLocals f = do
    lbl <- label
    let paramSize = arity * 8
        localSize = numLocals * 8
        retSize = 8
    sub (IntOp (conv (paramSize + localSize + retSize))) (RegOp Rsp)
    let ops = flip fmap [0 .. arity - 1] $ \i ->
            MemOp (Addr (Just Rsp) (Just (conv (i * 8))) Nothing)
    let locals = flip fmap [arity .. arity + numLocals - 1] $ \i ->
            MemOp (Addr (Just Rsp) (Just (conv (i * 8))) Nothing)
    let ret' = MemOp (Addr (Just Rsp) (Just (conv ((arity + numLocals) * 8))) Nothing)
    forM_ (zip ops abi) $ \(op, reg) ->
        mov (RegOp reg) op
    _ <- f ops locals ret'
    mov ret' (RegOp Rax)
    add (IntOp (conv (paramSize + localSize + retSize))) (RegOp Rsp)
    ret
    pure lbl

-- Call a function
callFunction :: MonadCode m => Operand 'RW -> Label -> [Operand r] -> m ()
callFunction retOp lbl args = do
    forM_ (zip args abi) $ \(op, reg) ->
        mov op (RegOp reg)
    call (LblOp lbl)
    mov (RegOp Rax) retOp

conv :: (Integral a, Num b) => a -> b
conv = fromInteger . toInteger

makeLenses ''Addr



















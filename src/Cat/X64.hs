{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

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
    , newLabel
    , db
    -- * High-Level Helpers
    , function
    , callFunction
    -- * Lenses
    , addrBaseReg, addrDisplacement, addrIndexReg
    , def
    -- * Effects
    , X64
    , runX64
    , evalX64
    ) where

-- Inspired by the X86-64bit package from Hackage

import           Control.Lens.TH   (makeLenses)
import           Control.Monad     (forM_, void)
import           Data.Default      (Default (def))
import           Data.Int          (Int32, Int64)
import           Data.Text         (Text)
import qualified Data.Text         as Text
import           Data.Word         (Word8)
import           Fmt               ((+|), (+||), (|+), (||+))
import qualified Fmt
import qualified Fmt.Internal.Core as Fmt
import           Polysemy
import           Polysemy.State
import           Polysemy.Writer
import           Prelude           hiding (and, or)

class ToX64 a where
    toX64 :: a -> Text

-- Formatting
infixr 1 +%, %+
(+%) :: Fmt.FromBuilder b => Fmt.Builder -> Fmt.Builder -> b
(+%) = (Fmt.+|)
(%+) :: (ToX64 a, Fmt.FromBuilder b) => a -> Fmt.Builder -> b
(%+) = (Fmt.|+) . toX64

data Label = Label !Int | LabelRaw !Text
    deriving (Eq, Ord, Show)
instance ToX64 Label where
    toX64 (Label i) = "lbl_"+||i||+""
    toX64 (LabelRaw l) = l

-- | Types of usable registers
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

instance ToX64 Register where
    toX64 = Text.cons '%' . Text.toLower . Text.pack . show

tmpReg :: Register
tmpReg = R15

-- | Possible ways to address a location in memory
data Addr = Addr
    { _addrBaseReg      :: Maybe Register
    , _addrDisplacement :: Maybe Int32
    , _addrIndexReg     :: Maybe (Word8, Register)
    } deriving (Show, Eq, Ord, Read)

-- Useful for Lens
instance Default Addr where
    def = Addr Nothing Nothing Nothing
instance ToX64 Addr where
    toX64 = \case
        Addr Nothing Nothing Nothing ->
            "0"
        Addr Nothing (Just disp) Nothing ->
            ""+||disp||+""
        Addr (Just reg) Nothing Nothing ->
            "("+%reg%+")"
        Addr (Just reg) (Just disp) Nothing ->
            ""+||disp||+"("+%reg%+")"
        Addr (Just reg) (Just disp) (Just (scale, index)) ->
            ""+||disp||+"("+%reg%+", "+||scale||+", "+%index%+")"
        addr -> error $ "Ill-formed Addr: "+||addr||+""

-- | Type-level type to determine if a value is writable
-- Ie. register operands are read/write, literals are read-only
data Access = R | RW

-- | Types of operands to assembly instructions
-- Use a GADT to prevent writing to immediate values
data Operand :: Access -> * where
    ImmOp :: Immediate -> Operand 'R
    RegOp :: Register  -> Operand rw
    MemOp :: Addr      -> Operand rw

instance ToX64 (Operand r) where
    toX64 = \case
        ImmOp (Immediate n) -> "$"+|| n ||+""
        ImmOp (LabelRef l) -> ""+% l %+""
        ImmOp (LabelMem l) -> "$"+% l %+""
        ImmOp (ImmMem n) -> ""+|| n ||+""
        RegOp reg -> ""+% reg %+""
        MemOp addr -> ""+% addr %+""

pattern LblOp :: Label -> Operand 'R
pattern LblOp l = ImmOp (LabelRef l)
pattern IntOp :: Int64 -> Operand 'R
pattern IntOp n = ImmOp (Immediate n)
pattern MemRegOp :: Register -> Operand rw
pattern MemRegOp reg = MemOp (Addr (Just reg) Nothing Nothing)

-- | Kinds of immediate operands
data Immediate
    = Immediate !Int64
    | LabelRef !Label
    | ImmMem !Int64
    | LabelMem !Label
    deriving (Show, Eq, Ord)

-- | X64 conditionals (subset that we need)
data Condition
    = E
    | NE
    | G
    | GE
    | L
    | LE
    deriving (Show, Eq, Ord, Enum, Read)

instance ToX64 Condition where
    toX64 = Text.toLower . Text.pack . show

-- | Negate an X64 conditional
notCond :: Condition -> Condition
notCond E = NE
notCond NE = E
notCond G = LE
notCond GE = L
notCond L = GE
notCond LE = G

-- * Compilation effect

-- | Polysemy effect needed to generate assembly
data X64 (m :: * -> *) a where
    NewLabel :: X64 m Label
    Code :: Text -> X64 m ()

makeSem_ ''X64

-- | Generate a new label, labeling the current position. Use MonadFix to
-- reference a label declared later.
newLabel :: forall r . Member X64 r => Sem r Label

-- | Insert code into the final assembly. Should not be used directly
code :: forall r . Member X64 r => Text -> Sem r ()

-- | Run the X64 compilation effect, returning a list of the compiled lines
runX64
    :: Sem (X64 ': r) a
    -> Sem r ([Text], a)
runX64 = runWriter @[Text] . evalState @Int 0 . reinterpret2 \case
    NewLabel -> do
        n <- get
        put (n + 1)
        tell [toX64 (Label n) <> ":"]
        pure $ Label n
    Code c -> do
        tell [c]

-- | Run the X64 compilation effect, returning the compiled assembly
evalX64
    :: Sem (X64 ': r) a
    -> Sem r Text
evalX64 = fmap (Text.unlines . fst) . runX64

-- * Ops
-- Note that binary operations handle multiple-memory calls by
-- emitting two assembly instructions, using %r15 as the intermediary

ret, nop :: Member X64 r => Sem r ()
ret = code "\tret"
nop = code "\tnop"

add, sub, or, and, mov, cmp, shl
    :: Member X64 r => Operand r' -> Operand 'RW -> Sem r ()
add (MemOp a) (MemOp b) = do
    code $ "\tmovq "+% MemOp a %+", "+% RegOp tmpReg %+""
    code $ "\tadd "+% RegOp tmpReg %+", "+% MemOp b %+""
add a b =
    code $ "\tadd "+% a %+", "+% b %+""
sub (MemOp a) (MemOp b) = do
    code $ "\tmovq "+% MemOp a %+", "+% RegOp tmpReg %+""
    code $ "\tsub "+% RegOp tmpReg %+", "+% MemOp b %+""
sub a b =
    code $ "\tsub "+% a %+", "+% b %+""
or  (MemOp a) (MemOp b) = do
    code $ "\tmovq "+% MemOp a %+", "+% RegOp tmpReg %+""
    code $ "\tor "+%  RegOp tmpReg %+", "+% MemOp b %+""
or  a b =
    code $ "\tor "+%  a %+", "+% b %+""
and (MemOp a) (MemOp b) = do
    code $ "\tmovq "+% MemOp a %+", "+% RegOp tmpReg %+""
    code $ "\tand "+% RegOp tmpReg %+", "+% MemOp b %+""
and a b =
    code $ "\tand "+% a %+", "+% b %+""
mov (MemOp a) (MemOp b) = do
    code $ "\tmovq "+% MemOp a %+", "+% RegOp tmpReg %+""
    code $ "\tmovq "+% RegOp tmpReg %+", "+% MemOp b %+""
mov a b =
    code $ "\tmovq "+% a %+", "+% b %+""
cmp (MemOp a) (MemOp b) = do
    code $ "\tmovq "+% MemOp a %+", "+% RegOp tmpReg %+""
    code $ "\tcmp "+% RegOp tmpReg %+", "+% MemOp b %+""
cmp a b =
    code $ "\tcmp "+% a %+", "+% b %+""
shl (MemOp a) (MemOp b) = do
    code $ "\tmovq "+% MemOp a %+", "+% RegOp tmpReg %+""
    code $ "\tshl "+% RegOp tmpReg %+", "+% MemOp b %+""
shl a b =
    code $ "\tshl "+% a %+", "+% b %+""

lea :: Member X64 r => Operand 'RW -> Operand 'RW -> Sem r ()
lea a b = code $ "\tlea "+% a %+" "+% b %+""

imul, idiv, push, call
    :: Member X64 r => Operand r' -> Sem r ()
imul a = code $ "\timulq "+% a %+""
idiv a = code $ "\tidivq "+% a %+""
push a = code $ "\tpushq "+% a %+""
call a = code $ "\tcall "+% a %+""

neg, pop :: Member X64 r => Operand 'RW -> Sem r ()
neg a = code $ "\tneg "+% a %+""
pop a = code $ "\tpop "+% a %+""

jmp :: Member X64 r => Label -> Sem r ()
jmp l = code $ "\tjmp "+% l %+""

j :: Member X64 r => Condition -> Label -> Sem r ()
j c l = code $ "\tj"+% c %+" "+% l %+""

global :: Member X64 r => Label -> Sem r ()
global l = code $ ".global "+% l %+""

db :: Member X64 r => Text -> Sem r Label
db str = do
    code $ ".section .rodata"
    lbl <- newLabel
    code $ "\t.asciz \""+| str |+"\""
    code $ ".section .text"
    pure lbl

rawLabel :: Member X64 r => Text -> Sem r Label
rawLabel t = do
    let lbl = LabelRaw t
    code $ ""+% lbl %+":"
    pure lbl

-- * Helpers
makeLenses ''Addr

-- Register order
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
function
    :: Member X64 r
    => Int -- ^ Function arity
    -> Int -- ^ Number of local variables
    -> ([Operand 'RW] -> [Operand 'RW] -> Operand 'RW -> Sem r a)
           -- ^ Function producing the X64 code
    -> Sem r Label
           -- ^ Return label to the function
function arity numLocals f = do
    lbl <- newLabel
    let paramSize = arity * 8
        localSize = numLocals * 8
        retSize = 8
    -- Make room for locals + parameters
    sub (IntOp (conv (paramSize + localSize + retSize))) (RegOp Rsp)
    let ops = flip fmap [0 .. arity - 1] $ \i ->
            MemOp (Addr (Just Rsp) (Just (conv (i * 8))) Nothing)
    let locals = flip fmap [arity .. arity + numLocals - 1] $ \i ->
            MemOp (Addr (Just Rsp) (Just (conv (i * 8))) Nothing)
    let ret' = MemOp (Addr (Just Rsp) (Just (conv ((arity + numLocals) * 8))) Nothing)
    -- Move the parameters to where they were allocated
    forM_ (zip ops abi) $ \(op, reg) ->
        mov (RegOp reg) op
    -- Call the function
    void $ f ops locals ret'
    -- Move the return value to %rax
    mov ret' (RegOp Rax)
    -- Restore the stack
    add (IntOp (conv (paramSize + localSize + retSize))) (RegOp Rsp)
    -- Return
    ret
    pure lbl
        where
            -- Helper to convert between integer types
            conv :: (Integral a, Num b) => a -> b
            conv = fromInteger . toInteger

-- | Call a function previously defined with 'function'
callFunction
    :: Member X64 r
    => Operand 'RW  -- ^ Return location
    -> Label        -- ^ Function label
    -> [Operand r'] -- ^ Args
    -> Sem r ()
callFunction retOp lbl args = do
    forM_ (zip args abi) $ \(op, reg) ->
        mov op (RegOp reg)
    call (LblOp lbl)
    mov (RegOp Rax) retOp





















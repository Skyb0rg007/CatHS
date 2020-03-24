{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo, FlexibleContexts #-}

module Cat.Backend
    ( compileProgram
    ) where

import           Cat.LIR
import           Cat.X64               as X64
import           Control.Lens
import           Control.Monad.Reader
import           Data.Functor.Foldable
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Text             (Text)
import qualified Data.Text             as Text

import           Cat.Common
import           Cat.X64

type FunEnv = Map LIRLabel Label
type SymEnv = Map Symbol (Operand 'RW)

compileProgram :: LIRProgram -> Text
compileProgram prog = compileProg $ snd $ runCodeM $ do
    rec
        funEnv <- traverse (compileFun funEnv) (prog^.lirProgramFuns)
    main' <- rawLabel "main"
    global main'
    compileFun funEnv (prog^.lirProgramMain)

-- Compiles a function, returning the function's label
compileFun :: MonadCode m => FunEnv -> LIRFunction -> m Label
compileFun funEnv fun = function arity numLocals $ \params locals returnOp -> do
    let p = zip (fun^.lirFunctionArgs) params
        l = zip (fun^.lirFunctionLocals) locals
        symEnv = Map.fromList (p ++ l ++ [(fun^.lirFunctionReturnSym, returnOp)])
    forM_ (fun^.lirFunctionInstructions) $ \instr -> do
        compileInstr funEnv symEnv instr
    where
        arity = fun^.lirFunctionArgs.to length
        numLocals = fun^.lirFunctionLocals.to length

convertLbl :: LIRLabel -> Label
convertLbl = \case
    LIRLabel n                -> LabelRaw $ "lir_lbl_" <> Text.pack (show n)
    LIRLabelAllocate          -> LabelRaw "allocate"
    LIRLabelAllocateAndMemset -> LabelRaw "allocate_and_memset"
    LIRLabelPrintlnInt        -> LabelRaw "print_line_int"
    LIRLabelPrintlnString     -> LabelRaw "print_line_string"
    LIRLabelPrintInt          -> LabelRaw "print_int"
    LIRLabelPrintString       -> LabelRaw "print_string"

defaultFunEnv = Map.fromList
    [ (LIRLabelAllocate          , LabelRaw "allocate")
    , (LIRLabelAllocateAndMemset , LabelRaw "allocate_and_memset")
    , (LIRLabelPrintlnInt        , LabelRaw "print_line_int")
    , (LIRLabelPrintlnString     , LabelRaw "print_line_string")
    , (LIRLabelPrintInt          , LabelRaw "print_int")
    , (LIRLabelPrintString       , LabelRaw "print_string")
    ]

compileInstr
    :: MonadCode m 
    => FunEnv
    -> SymEnv
    -> LIRAssembly
    -> m ()
compileInstr funEnv symEnv (LIRAsmLabel lbl) = 
    case convertLbl lbl of
      LabelRaw x -> void $ rawLabel x
compileInstr funEnv' symEnv (LIRAsmInstruction instr) =
    let funEnv = funEnv' <> defaultFunEnv in
    case instr of
      -- nop
      LIRNop -> nop
      -- sym = n
      LIRAssignLit (LitInt n) sym
        | Just op <- Map.lookup sym symEnv
        -> mov (IntOp n) op
      -- *(loc + off) = val
      LIRStoreToMemoryAtOffset loc off val
        | Just locOp <- Map.lookup loc symEnv
        , Just offOp <- Map.lookup off symEnv
        , Just valOp <- Map.lookup val symEnv
        -> do
            mov locOp (RegOp Rax)
            mov offOp (RegOp Rbx)
            mov valOp (MemOp $ def & addrBaseReg._Just .~ Rax
                                   & addrIndexReg._Just .~ (1, Rbx))
      -- val = *(loc + off)
      LIRLoadFromMemoryAtOffset loc off val
        | Just locOp <- Map.lookup loc symEnv
        , Just offOp <- Map.lookup off symEnv
        , Just valOp <- Map.lookup val symEnv
        -> do
            mov locOp (RegOp Rax)
            mov offOp (RegOp Rbx)
            mov (MemOp $ def & addrBaseReg._Just .~ Rax
                             & addrIndexReg._Just .~ (8, Rbx))
                valOp
      -- assign_to = val
      LIRAssign val assign_to
        | Just valOp <- Map.lookup val symEnv
        , Just assOp <- Map.lookup assign_to symEnv
        -> mov valOp assOp
      -- assign_to = -val
      LIRNegate val assign_to
        | Just valOp <- Map.lookup val symEnv
        , Just assOp <- Map.lookup assign_to symEnv
        -> do
            mov valOp assOp
            neg assOp
      -- assign_to = f(args)
      LIRCall lirlbl args assign_to
        | Just argOps <- traverse (flip Map.lookup symEnv) args
        , length args == length argOps
        , Just assOp <- Map.lookup assign_to symEnv
        -> let ~(Just lbl) = Map.lookup lirlbl funEnv
            in callFunction assOp lbl argOps
      -- assign_to = left `binop` right
      LIRBinOp left binop right assign_to
        | Just leftOp <- Map.lookup left symEnv
        , Just rightOp <- Map.lookup right symEnv
        , Just assOp <- Map.lookup assign_to symEnv
        -> let defaultOp asm = mov leftOp assOp >> asm rightOp assOp
            in case binop of
                 OpMult -> do
                     mov leftOp (RegOp Rax)
                     imul rightOp
                     mov (RegOp Rax) assOp
                 OpDiv -> do
                     mov leftOp (RegOp Rax)
                     mov (IntOp 0) (RegOp Rdx)
                     idiv rightOp
                     mov (RegOp Rax) assOp
                 OpAdd -> defaultOp add
                 OpSub -> defaultOp sub
                 OpAnd -> defaultOp X64.and
                 OpOr  -> defaultOp X64.or
      LIRJump lbl -> jmp (convertLbl lbl)
      LIRJumpC lbl left cond right
        | Just leftOp <- Map.lookup left symEnv
        , Just rightOp <- Map.lookup right symEnv
        -> do
            -- Swaps!
            cmp rightOp leftOp
            j cond (convertLbl lbl)
      lir -> error $ "Operand " ++ show lir ++ " not found in table"
                ++ "\n\nTable: " ++ show (Map.keys symEnv)


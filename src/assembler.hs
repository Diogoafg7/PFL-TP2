module Assembler where

import Data_info
import Data.Char (isDigit)

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)

--Arithmetic op 
run (Add:code, Number v1:Number v2:stack, state) = run( code, Number (v1+v2):stack,state) -- if the stack is not empty and the top elements are numbers add the first two elements
run (Add:code, _:_:stack, state) = error "Error.EmptyStack"

run (Mult:code, Number v1:Number v2:stack, state) = run(code, Number (v1*v2):stack,state)
run (Mult:code, _:_:stack, state) = error "Error.EmptyStack"

run (Sub:code, Number v1:Number v2:stack, state) = run(code, Number (v1-v2):stack,state)
run (Sub:code, _:_:stack, state) = error "Error.EmptyStack"

--boolean op
run (Equ:code, v1:v2:stack, state) = do --puts true on top of the stack if the first 2 values are equal , else false
    let val = if v1 == v2 then TT else FF
    let newState = (code, val:stack, state)
    run newState

run (Le:code, Number n1: Number n2:stack, state) = do --puts true if the first value is less or equal than the second value, else false
    let val = if n1 <= n2 then TT else FF
    let newState = (code, val:stack, state)
    run newState

run (Fals:code, stack, state) = do --Add false to the top of the stack
    let val = FF
    let newState = (code, val:stack, state)
    run newState

run (Tru:code, stack, state) = do --Add true to the top of the stack
    let val = TT
    let newState = (code, val:stack, state)
    run newState

run (And:code, TT:TT:stack, state) = do 
    let val = TT
    let newState = (code, val:stack, state)
    run newState
run (And:code, TT:FF:stack, state) = do 
    let val = FF
    let newState = (code, val:stack, state)
    run newState
run (And:code, FF:TT:stack, state) = do 
    let val = FF
    let newState = (code, val:stack, state)
    run newState    
run (And:code, FF:FF:stack, state) = do 
    let val = FF
    let newState = (code, val:stack, state)
    run newState
run (And:code, Number i:stack, state) = error "Error. Not a truth value"


run (Neg:code, FF:stack, state) = do 
    let val = TT
    let newState = (code, val:stack, state)
    run newState
run (Neg:code, TT:stack, state) = do 
    let val = FF
    let newState = (code, val:stack, state)
    run newState
run (Neg:code, Number i:stack, state) = error "Error. Not a truth value"



--modify the evaluation stack
run ((Push n):code, stack, state) = do
    let newState = (code,(Number n):stack, state)
    run newState

run((Fetch var):code, stack, state) = do --looks ffor var in the state list, if found add it to the stack else error
    case lookup var state of
        Just val -> run (code, val:stack, state)
        Nothing  -> error $ "Run-time error: Variable " ++ var ++ " not found"
         
run ((Store var):code, val:stack, state) = do
    let newState = (code, stack, (var, val):filter (\(v, _) -> v /= var) state)
    run newState

-- Handle branch(c1, c2) instruction
run ((Branch c1 c2):code, TT:stack, state) = do
    let newState =  (c1 ++ code, stack, state)  
    run newState

run ((Branch c1 c2):code, FF:stack, state) = do
    let newState = (c2 ++ code, stack, state)
    run newState

run ((Branch c1 c2):code, Number i:stack, state) = error "Error. Not a truth value"


run (Loop c1 c2 : code, stack, state) = do
  let branchCode = Branch (c2 ++ [Loop c1 c2]) [Noop]
  run (c1 ++ branchCode : code, stack, state)


-- Handle noop instruction
run (Noop:code, stack, state) = do
    let newState = (code, stack, state)
    run newState
run (code, stack, state) = error $ "Run-time error: Unhandled case for code " ++ show code ++ ", stack " ++ show stack ++ ", state " ++ show state

-----------------------------------part2

assembleAexp :: Aexp -> Code
assembleAexp (ANum n) = [Push n]
assembleAexp (AVar var) = [Fetch var]
assembleAexp (AAdd e1 e2) = assembleAexp e1 ++ assembleAexp e2 ++ [Add]
assembleAexp (AMult e1 e2) = assembleAexp e1 ++ assembleAexp e2 ++ [Mult]
assembleAexp (ASub e1 e2) = assembleAexp e1 ++ assembleAexp e2 ++ [Sub]

assembleBexp :: Bexp -> Code
assembleBexp BTrue = [Tru]
assembleBexp BFalse = [Fals]
assembleBexp (BEq e1 e2) = assembleAexp e1 ++ assembleAexp e2 ++ [Equ]
assembleBexp (BLe e1 e2) = assembleAexp e1 ++ assembleAexp e2 ++ [Le]
assembleBexp (BNot b) = assembleBexp b ++ [Neg]
assembleBexp (BAnd b1 b2) = assembleBexp b1 ++ assembleBexp b2 ++ [And]

assembleStm :: Stm -> Code
assembleStm (Assign var aexp) = assembleAexp aexp ++ [Store var]
assembleStm (If bexp stm1 stm2) = assembleBexp bexp ++ [Branch (assembleStm stm1) (assembleStm stm2)]
assembleStm (While bexp stm) = assembleBexp bexp ++ [Loop (assembleStm stm) [Noop]]
assembleStm (Seq stms) = concatMap assembleStm stms

-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

-- Part 1

-- Do not modify our definition of Inst and Code
module Data_info where

import Prelude
import Data.List (intercalate)
import Data.List (sort)

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data StackElement = Number Integer | TT | FF deriving (Show, Eq, Ord)
type Stack = [StackElement]

stackElementValue::StackElement -> String
stackElementValue (Number n) = show n
stackElementValue TT = "True"
stackElementValue FF = "False"


type State = [(String, StackElement )] --string is the key and int the value associated

createEmptyStack :: Stack
createEmptyStack = []

stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map stackElementValue stack)

createEmptyState :: State
createEmptyState = []

state2Str :: State -> String
state2Str state = intercalate "," [var ++ "=" ++ show val | (var, val) <- sort state]  --after sorting the state list, iterate through the list and make the var = val with , separating them



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


-- Example program: Push 2, Push 3, Add
exampleProgram :: Code
exampleProgram = [Fals,Store "var",Fetch "var"]
-- Initial state and stack
initialState :: State
initialState = []

initialStack :: Stack
initialStack = []

-- Test the program
main :: IO ()
main = do
  let (finalCode, finalStack, finalState) = run (exampleProgram, initialStack, initialState)
  putStrLn "Final Code:"
  print finalCode
  putStrLn "Final Stack:"
  print finalStack
  putStrLn "Final State:"
  print finalState




-- To help you test your assembler
--testAssembler :: Code -> (String, String)
--testAssembler code = (stack2Str stack, state2Str state)
  --where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
--compA = undefined -- TODO
--
---- compB :: Bexp -> Code
--compB = undefined -- TODO
--
---- compile :: Program -> Code
--compile = undefined -- TODO
--
---- parse :: String -> Program
--parse = undefined -- TODO
--
---- To help you test your parser
--testParser :: String -> (String, String)
--testParser programCode = (stack2Str stack, store2Str store)
 -- --where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyStore)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
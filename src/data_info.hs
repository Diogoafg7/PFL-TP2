-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

-- Part 1

-- Do not modify our definition of Inst and Code
module Data_info where

import Prelude
import Data.List (intercalate)
import Data.List (sort)

data Inst =
    Push Integer
    | Add 
    | Mult 
    | Sub 
    | Tru 
    | Fals 
    | Equ 
    | Le 
    | And 
    | Neg 
    | Fetch String 
    | Store String 
    | Noop 
    | Branch Code Code 
    | Loop Code Code
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

---------------PART2

--------- COMPILER --------------

-- Arithmetic expressions
data Aexp
    = Num Integer         -- a number
    | Var String          -- a variable
    | AddExp Aexp Aexp       -- addition
    | SubExp Aexp Aexp       -- subtraction
    | MultExp Aexp Aexp       -- multiplication
    deriving Show

-- Boolean expressions
data Bexp
    = Tru                 -- true
    | Fals                 -- false
    | Not Bexp            -- negation
    | AndExp Bexp Bexp       -- logical and
    | EquExp Aexp Aexp       -- equality
    | LeExp Aexp Aexp        -- less than or equal to
    deriving Show

-- Statements
data Stm
    = Assign String Aexp  -- assignment
    | NoopStm                -- do nothing
    | If Bexp Stm Stm     -- if statement
    | While Bexp Stm      -- while loop
    deriving Show

type Program = [Stm]
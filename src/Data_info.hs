-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

-- Part 1

-- Do not modify our definition of Inst and Code
module Data_info where

import Prelude
import qualified Data.Map.Strict as HashMap
import Data.List (intercalate, sort)
import Data.Map (toList)

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
state2Str state = intercalate "," [var ++ "=" ++ stackElementValue val | (var, val) <- sort state]  --after sorting the state list, iterate through the list and make the var = val with , separating them

--------------- PART2 -------------

-- Arithmetic expressions

data Aexp
    = Num Integer        -- a number
    | Var String         -- a variable
    | AddA Aexp Aexp      -- addition
    | SubA Aexp Aexp      -- subtraction
    | MultA Aexp Aexp     -- multiplication
    deriving Show

-- Boolean expressions

data Bexp
    = TrueB               -- true
    | FalseB              -- false
    | NotB Bexp           -- negation
    | AndB Bexp Bexp      -- logical AND
    | EqualBB Bexp Bexp   -- boolean equality
    | LessB Aexp Aexp     -- less than or equal to
    | EqualAB Aexp Aexp   -- arithmetic equality
    deriving Show


-- Statements

data Stm
    = Assign String Aexp          -- assignment
    | If Bexp Program Program     -- if statement
    | While Bexp Program          -- while loop
    deriving Show

type Program = [Stm]
type Parser a = [String] -> (a)
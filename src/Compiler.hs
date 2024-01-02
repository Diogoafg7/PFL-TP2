module Compiler where

import Data_info
import Data.Char (isDigit, isAlpha)

-- Compiles an arithmetic expression into a list of instructions
compA :: Aexp -> Code
compA (Num i) = [Push i]
compA (Var var) = [Fetch var]
compA (AddA a1 a2) = compA a2 ++ compA a1 ++ [Add]
compA (SubA a1 a2) = compA a2 ++ compA a1 ++ [Sub]
compA (MultA a1 a2) = compA a2 ++ compA a1 ++ [Mult]

-- Compiles a boolean expression into a list of instructions
compB :: Bexp -> Code
compB TrueB = [Tru]
compB FalseB = [Fals]
compB (NotB bexp) = compB bexp ++ [Neg]
compB (AndB b1 b2) = compB b2 ++ compB b1 ++ [And]
compB (LessB a1 a2) = compA a2 ++ compA a1 ++ [Le]
compB (EqualBB a1 a2) = compB a2 ++ compB a1 ++ [Equ]
compB (EqualAB a1 a2) = compA a2 ++ compA a1 ++ [Equ]

-- Main function for compiling a program
compile :: Program -> Code
compile = concatMap compileStm
  where
    compileStm (Assign var aexp) = compA aexp ++ [Store var]
    compileStm (If bexp stm1 stm2) = compB bexp ++ [Branch (compile stm1) (compile stm2)]
    compileStm (While bexp program) = [Loop (compB bexp) (compile program)]

-- Parses an arithmetic expression
parseAexp :: Parser Aexp
parseAexp tokens = case nextValidAexpToken (tokens, []) "+" of
    (firstSegment, "+":secondSegment) -> AddA (parseAexp firstSegment) (parseAexp secondSegment)
    _ -> case nextValidAexpToken (tokens, []) "-" of
        (firstSegment, "-":secondSegment) -> SubA (parseAexp firstSegment) (parseAexp secondSegment)
        _ -> case nextValidAexpToken (tokens, []) "*" of
            (firstSegment, "*":secondSegment) -> MultA (parseAexp firstSegment) (parseAexp secondSegment)
            _ -> case break allNumbers (reverse tokens) of
                (_, number:firstSegment) | checkParenthesis (reverse firstSegment) -> Num (read number)
                _ -> case break allLetters (reverse tokens) of
                    (_, name:firstSegment) | checkParenthesis (reverse firstSegment) -> Var name
                    _ -> case break (== "(") (reverse tokens) of
                        (middleAfter, "(":_) -> case break (== ")") (reverse middleAfter) of
                            (middle, ")":_) -> parseAexp middle

-- Parses a boolean expression
parseBexp :: Parser Bexp
parseBexp tokens = case nextValidInTokens (tokens, []) "and" of
    (firstSegment, "and":secondSegment) -> AndB (parseBexp firstSegment) (parseBexp secondSegment)
    _ -> case nextValidInTokens (tokens, []) "=" of
        (firstSegment, "=":secondSegment) -> EqualBB (parseBexp firstSegment) (parseBexp secondSegment)
        _ -> case nextValidInTokens (tokens, []) "not" of
            (_, "not":secondSegment) -> NotB (parseBexp secondSegment)
            _ -> case nextValidInTokens (tokens, []) "==" of
                (firstSegment, "==":secondSegment) -> EqualAB (parseAexp firstSegment) (parseAexp secondSegment)
                _ -> case nextValidInTokens (tokens, []) "<=" of
                    (firstSegment, "<=":secondSegment) -> LessB (parseAexp firstSegment) (parseAexp secondSegment)
                    _ -> case break (== "True") (reverse tokens) of
                        (_, "True":firstSegment) | checkParenthesis (reverse firstSegment) -> TrueB
                        _ -> case break (== "False") (reverse tokens) of
                            (_, "False":firstSegment) | checkParenthesis (reverse firstSegment) -> FalseB
                            _ -> case break (== "(") (reverse tokens) of
                                (middleAfter, "(":_) -> case break (== ")") (reverse middleAfter) of
                                    (middle, ")":_) -> parseBexp middle

-- Parses an if statement
parseIfStatement :: Parser Program
parseIfStatement ("if":tokens) = case break (=="then") tokens of
    (ifStm , rest) -> case splitAtElse ([],rest) 0 of
        ("then":"(":thenStm', "else":"(":elseStm') ->
            let ")":thenStm = reverse thenStm'
                (elseStm,")":";":next) = splitAtParenthesis ([],elseStm') 0
            in If (parseBexp ifStm) (parseProgramStructure (reverse thenStm)) (parseProgramStructure elseStm):parseProgramStructure next
        ("then":"(":thenStm', "else":elseStm) ->
            let ")":thenStm = reverse thenStm'
            in [If (parseBexp ifStm) (parseProgramStructure (reverse thenStm)) [parseAssignmentStatement elseStm]]
        ("then":thenStm, "else":"(":elseStm') ->
            let (elseStm,")":";":next) = splitAtParenthesis ([],elseStm') 0
            in [If (parseBexp ifStm) [parseAssignmentStatement thenStm] (parseProgramStructure elseStm)] ++ parseProgramStructure next
        ("then":thenStm, "else":elseStm) -> case break (==";") elseStm of
            (elseStm, ";":next) -> [If (parseBexp ifStm) [parseAssignmentStatement thenStm] [parseAssignmentStatement elseStm]] ++ parseProgramStructure next
            _ -> [If (parseBexp ifStm) [parseAssignmentStatement thenStm] [parseAssignmentStatement elseStm]]

-- Parses a while statement
parseWhileStatement :: Parser Program
parseWhileStatement ("while":tokens) = case break (== "do") tokens of
    (whileStm, "do":"(":after) ->
        let (doStm,")":";":next) = splitAtParenthesis ([], after) 0
        in (While (parseBexp whileStm) (parseProgramStructure doStm) : parseProgramStructure next)
    (whileStm, "do":doStm) -> [While (parseBexp whileStm) [parseAssignmentStatement doStm]]

-- Parses an assignment statement
parseAssignmentStatement :: Parser Stm
parseAssignmentStatement (token:":=":tokens) = Assign token (parseAexp tokens)
parseAssignmentStatement _ = error "Invalid assignment statement"


-- Main helper function for parsing a program
parseProgramStructure :: Parser Program
parseProgramStructure [] = []
parseProgramStructure (firstToken:tokens)
    | firstToken == "if" = parseIfStatement (firstToken:tokens)
    | firstToken == "while" = parseWhileStatement (firstToken:tokens)
    | otherwise = case break (== ";") (firstToken:tokens) of
        (firstStm, ";":second) -> parseAssignmentStatement firstStm : parseProgramStructure second

-- Parses a string with code into a program
parse :: String -> Program
parse = parseProgramStructure . lexer

-- Lexes a string into a list of tokens
lexer :: String -> [String]
lexer [] = []
lexer (':':'=':cs) = ":=" : lexer cs
lexer ('<':'=':cs) = "<=" : lexer cs
lexer ('=':'=':cs) = "==" : lexer cs
lexer ('>':'=':cs) = ">=" : lexer cs
lexer (c:cs)
    | c `elem` " +-*;()=" = if c == ' ' then lexer cs else [c] : lexer cs
    | otherwise = let (word, rest) = span (`notElem` " +-*;():=<") (c:cs)
                  in case rest of
                       (':':'=':rs) -> word : ":=" : lexer rs
                       _ -> word : lexer rest

-- Auxiliary functions

allLetters :: String -> Bool
allLetters = all isAlpha

allNumbers :: String -> Bool
allNumbers = all isDigit

splitAtElse :: ([String], [String]) -> Int -> ([String], [String])
splitAtElse (_, []) _ = ([], [])
splitAtElse (left, x:right) count
    | x == "if" = splitAtElse (left ++ [x], right) (count+1)
    | x == "else" && count /= 0 = splitAtElse (left ++ [x], right) (count-1)
    | x == "else" = (left, x:right)
    | otherwise = splitAtElse (left ++ [x], right) count

splitAtParenthesis :: ([String], [String]) -> Int -> ([String], [String])
splitAtParenthesis (left, []) _ = (left, [])
splitAtParenthesis (left, x:right) count
    | x == "(" = splitAtParenthesis (left ++ [x], right) (count+1)
    | x == ")" && count /= 0 = splitAtParenthesis (left ++ [x], right) (count-1)
    | x == ")" = (left, x:right)
    | otherwise = splitAtParenthesis (left ++ [x], right) count

-- Used to find the next valid token in the list of tokens
nextValidInTokens :: ([String], [String]) -> String -> ([String], [String])
nextValidInTokens ([], x:right) token
    | x == token = ([], x:right)
nextValidInTokens ([], right) _ = ([], right)
nextValidInTokens ("(":left, []) token = nextValidInTokens (init left, [last left]) token
nextValidInTokens (left, []) token = nextValidInTokens (init left, [last left]) token
nextValidInTokens (left, x:right) token
    | x == token && checkParenthesis left = (left, x:right)
    | otherwise = nextValidInTokens (init left, last left:x:right) token

-- Similar to nextValidInTokens, but used strictly for parsing arithmetic expressions
nextValidAexpToken :: ([String], [String]) -> String -> ([String], [String])
nextValidAexpToken ([], x:right) token
    | x == token = ([], x:right)
nextValidAexpToken ([], right) _ = ([], right)
nextValidAexpToken (left, []) token = nextValidAexpToken (init left, [last left]) token
nextValidAexpToken (left, x:right) token
    | x == token && checkParenthesis left = (left, x:right)
    | otherwise = nextValidAexpToken (init left, last left:x:right) token

-- Checks if the parenthesis are balanced
checkParenthesis :: [String] -> Bool
checkParenthesis = (== 0) . foldl updateCount 0
  where
    updateCount count "(" = count + 1
    updateCount count ")" = max 0 (count - 1)
    updateCount count _   = count

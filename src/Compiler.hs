module Compiler where

import Data_info
import Assembler
import Data.Char (isDigit, isLower)

-- Lexer (divide a string em palavras (tokens))
lexer :: String -> [String]
lexer [] = []
lexer (c:cs)
    | c `elem` " +-*;()=" = if c == ' ' then lexer cs else [c] : lexer cs
    | otherwise = let (word, rest) = span (`notElem` " +-*;()=") (c:cs)
                    in word : lexer rest

-- parsing
parse :: String -> [Stm]
parse input = case parseProgram (lexer input) of
    Just (program, []) -> program
    _ -> error "Erro de sintaxe no programa."

-- parsing for programs
parseProgram :: [String] -> Maybe ([Stm], [String])
parseProgram [] = Just ([], [])
parseProgram tokens = case parseStm tokens of
    Just (stm, ";" : tokens') -> case parseProgram tokens' of
        Just (program, tokens'') -> Just (stm : program, tokens'')
        _ -> Just ([stm], tokens')
    Just (stm, tokens') -> Just ([stm], tokens')
    _ -> Nothing

-- parsing for statements
parseStm :: [String] -> Maybe (Stm, [String])
parseStm (token : ":=" : tokens) = case parseAexp tokens of
    Just (aexp, tokens') -> Just (Assign token aexp, tokens')
    _ -> Nothing
parseStm ("if" : tokens) = case parseBexp tokens of
    Just (bexp, "then" : tokens') -> case parseProgram tokens' of
        Just (stm1, "else" : tokens'') -> case parseProgram tokens'' of
            Just (stm2, tokens''') -> Just (If bexp (head stm1) (head stm2), tokens''')
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing
parseStm ("while" : tokens) = case parseBexp tokens of
    Just (bexp, "do" : tokens') -> case parseProgram tokens' of
        Just (stm, tokens'') -> Just (While bexp (head stm), tokens'')
        _ -> Nothing
    _ -> Nothing
parseStm (";" : tokens) = Just (NoopStm, tokens)
parseStm _ = Nothing

-- parsing for Arithmetic expressions
parseAexp :: [String] -> Maybe (Aexp, [String])
parseAexp (token : tokens)
    | all isDigit token = Just (Num (read token), tokens)
    | isLower (head token) = Just (Var token, tokens)
    | token == "(" = case parseAexp tokens of
        Just (aexp, ")" : tokens') -> Just (aexp, tokens')
        _ -> Nothing
    | otherwise = Nothing

-- parsing for Boolean expressions
parseBexp :: [String] -> Maybe (Bexp, [String])
parseBexp ("not" : tokens) = case parseBexp tokens of
    Just (bexp, tokens') -> Just (Not bexp, tokens')
    _ -> Nothing
parseBexp tokens = case parseAexp tokens of
    Just (aexp1, op : tokens') -> case parseAexp tokens' of
        Just (aexp2, tokens'') -> case op of
            "<=" -> Just (LeExp aexp1 aexp2, tokens'')
            "==" -> Just (EquExp aexp1 aexp2, tokens'')
            "and" -> Just (AndExp (EquExp aexp1 (Num 1)) (EquExp aexp2 (Num 1)), tokens'')
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

type Code = [Inst]  -- Defina Inst conforme sua implementação anterior

-- Compiler Functions
compile :: [Stm] -> Data_info.Code
compile [] = []
compile (stm:stmts) = compileStm stm ++ compile stmts
    where
        compileStm :: Stm -> Data_info.Code
        compileStm (Assign var aexp) = compA aexp ++ [Store var]
        compileStm (If bexp stm1 stm2) = compB bexp ++ [Branch (compileStm stm1) (compileStm stm2)]
        compileStm (While bexp stm) = [Loop (compB bexp) (compileStm stm)]
        compileStm NoopStm = []

-- Arithmetic expressions
compA :: Aexp -> Data_info.Code
compA (Num i) = [Push i]
compA (Var var) = [Fetch var]
compA (AddExp a1 a2) = compA a1 ++ compA a2 ++ [Add]
compA (SubExp a1 a2) = compA a1 ++ compA a2 ++ [Sub]
compA (MultExp a1 a2) = compA a1 ++ compA a2 ++ [Mult]

-- Boolean expressions
compB :: Bexp -> Data_info.Code
compB Tr = [Tru]
compB Fls = [Fals]
compB (Not bexp) = compB bexp ++ [Neg]
compB (AndExp b1 b2) = compB b1 ++ compB b2 ++ [And]
compB (LeExp a1 a2) = compA a1 ++ compA a2 ++ [Le]
compB (EquExp a1 a2) = compA a1 ++ compA a2 ++ [Equ]

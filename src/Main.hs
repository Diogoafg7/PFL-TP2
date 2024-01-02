module Main where

import Compiler
import Data_info
import Assembler
import Tests( runAllAssemblerTests, runAllParserTests)

-- main will just run all the tests, the function that processes the Code, Stack and State is the run function
main :: IO ()
main = do
    runAllAssemblerTests
    runAllParserTests
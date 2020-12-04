module Main where

import IntCode (runComputation, flashMemory, setInput, continue)
import qualified Inputs.Day5_2019 (input)

runDiagnostic program n = runComputation $ do
    flashMemory program
    setInput $ return n
    continue

main :: IO ()
main = do
    program <- Inputs.Day5_2019.input
    runDiagnostic program 1
    runDiagnostic program 5
    return ()

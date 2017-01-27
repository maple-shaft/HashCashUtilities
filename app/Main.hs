module Main where

import System.Environment
import Data.List
import HashCash

dispatch :: [(String, [String] -> IO ())]
dispatch = [
    ("generate", generate)
  , ("validate", validate)
  ]
  
generate :: [String] -> IO ()
generate args = do
  header <- generateHeader defaultHashCashSpec
  putStrLn header
  return ()
  
validate :: [String] -> IO ()
validate args = do
  let (arg:others) = args
  if (validateHeader arg) then putStrLn "Valid"
    else putStrLn "Not Valid"  
  return ()

main :: IO ()
main = do 
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args
  

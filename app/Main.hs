module Main where

import System.Environment
import System.Exit
import Data.List
import HashCash

dispatch :: [(String, [String] -> IO ())]
dispatch = [
    ("generate", generate)
  , ("validate", validate)
  , ("help", help)
  ]
  
help :: [String] -> IO ()
help args = do
  putStrLn "HashCash Command Line Interface Tool:"
  putStrLn "Copyright 2017 Dustin Briscoe"
  putStrLn " "
  putStrLn "  Commands: "
  putStrLn "    generate - Create a valid X-Hashcash mail header given various options"
  putStrLn "      -v [option] = Use a specific email address or other value"
  putStrLn "      -d [option] = Use a specific valid timestamp such that timestamp of mail header can be validated"
  putStrLn "      -p [option] = Modify the difficulty setting of the proof of work (NOTE: Higher numbers require more work)"
  putStrLn "    validate - Validate an X-Hashcash mail header and return 'Valid' or 'Not Valid'"

  
generate :: [String] -> IO ()
generate args = do
  header <- case (generateOpts args Nothing (Just defaultHashCashSpec)) of
    (Just s) -> generateHeader s
    Nothing -> do return "Please run the help command to see valid arguments."
  putStrLn header
  return ()
  
generateOpts :: [String] -> Maybe String -> Maybe HashCashSpec -> Maybe HashCashSpec
generateOpts _ _ Nothing = Nothing
generateOpts [] _ (Just spec) = Just spec
generateOpts (x:xs) Nothing (Just spec) = generateOpts xs (Just x) (Just spec)
generateOpts (x:xs) y (Just spec) =
  let specV = spec { value = x }
      specD = spec { timestamp = x }
      specP = spec { difficulty=(read x) }
  in case y of
    (Just "-v") -> generateOpts xs Nothing (Just specV)
    (Just "-d") -> generateOpts xs Nothing (Just specD)
    (Just "-p") -> generateOpts xs Nothing (Just specP)
    _ -> Nothing
  
validate :: [String] -> IO ()
validate args = do
  let (arg:others) = args
  if (validateHeader arg) then putStrLn "Valid"
    else putStrLn "Not Valid"  
  return ()

main :: IO ()
main = do
  a <- getArgs
  let arf = case a of
             [] -> ("help":[])
             _ -> a
  let (command:args) = arf
  let (Just action) = lookup command dispatch
  action args
  

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
  putStrLn "HashCash Command Line Interface Tool: Copyright 2017 Dustin Briscoe"
  putStrLn " "
  putStrLn "This command line tool will allow you to mint a valid HashCash header, or validate it.  The following tool is to be "
  putStrLn "used at your own risk.  It is intended to be a proof of concept in implementing a proof of work scheme in a "
  putStrLn "purely functional language like Haskell."
  putStrLn " "
  putStrLn "Features not yet implemented:"
  putStrLn " - Only version 1 of HashCash is implemented"
  putStrLn " - Timestamp validation is not currently occurring"
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
      specT = spec { threads=(read x) }
  in case y of
    (Just "-v") -> generateOpts xs Nothing (Just specV)
    (Just "-d") -> generateOpts xs Nothing (Just specD)
    (Just "-p") -> generateOpts xs Nothing (Just specP)
    (Just "-t") -> generateOpts xs Nothing (Just specT)
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
  

module Main where

import HashCash

main :: IO ()
main = do 
  header <- generateHeader
  putStrLn header
  return ()

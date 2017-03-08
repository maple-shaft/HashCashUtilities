module Testy where

import           Crypto.Random
import           Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as BU
import           Data.Binary.Strict.Get
import           Data.Word (Word32)
import           Data.List (intercalate)

getRandomBytes'' :: (DRG g) => g -> Int -> (ByteString, g)
getRandomBytes'' g num = randomBytesGenerate num g

convertToStr :: ByteString -> String
convertToStr = BU.toString

toInt :: Word32 -> Int
toInt = fromIntegral

getWords :: Get [Int]
getWords = do
  empty <- isEmpty
  if empty
    then return []
    else do
      w <- getWord32be
      ws <- getWords
      return $ (toInt w):ws

printMe = putStrLn . intercalate ("\n")

mainly :: IO ()
mainly = do
  g <- getSystemDRG
  let threads = 8
  let (bs, _) = getRandomBytes'' g (threads * 4)
  let res = runGet getWords bs
  let words = case res of
             (Left arr, _) -> []
             (Right arr, _) -> arr
  putStrLn $ show $ length words
  printMe $ map (show) words
  
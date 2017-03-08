{-# LANGUAGE BangPatterns #-}

module HashCash where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad
import           Crypto.Hash
import           Crypto.Random
import           Data.Binary.Strict.Get
import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Conversion as BCON
import           Data.Int (Int32)
import           Data.List (find, intercalate, sort)
import           Data.List.Split (splitOn)
import           Data.Maybe (fromJust)
import           Data.Word (Word32)
import           System.Random

data HashCashSpec = HashCashSpec { version :: Int
                                 , difficulty :: Int
                                 , value :: String
                                 , timestamp :: String
                                 , threads :: Int
                                 } deriving (Show)

defaultHashCashSpec = HashCashSpec {version=1, difficulty=20, value="a@a", timestamp="150320112233", threads=1}
startingCounter :: Int32
startingCounter = 1
headerPrefix = "X-Hashcash: "
template = "1:{:{:{::{:{"

convertToString :: ByteString -> String
convertToString = BU.toString

convertFromString :: String -> ByteString
convertFromString = BU.fromString

convertIntToString :: Int -> String
convertIntToString = convertToString . BCON.toByteString'

toInt :: Word32 -> Int
toInt = fromIntegral

encodeInt32 :: Int32 -> ByteString
encodeInt32 = B64.encode . BCON.toByteString'

--OLD DECODER
mahDecoder :: Get Word32
mahDecoder = do
  first32Bits <- getWord32be
  return first32Bits
  
--multi decoder
getWords :: Get [Int32]
getWords = do
  empty <- isEmpty
  if empty
    then return []
    else do
      w <- getWord32be
      ws <- getWords
      return $ (fromIntegral (toInt w)):ws


-- OLD firstBitsZero, unoptimized
--firstBitsZeroOLD :: (Bits a) => a -> Bool
--firstBitsZeroOLD val = foldr (\x acc -> ((not $ testBit val x) && acc)) True [0..(difficulty - 1)]

bitMaskZero :: (Bits a) => Int -> a
bitMaskZero diff = foldr (\x acc -> setBit acc x) zeroBits [0..(diff - 1)]

firstBitsZero :: (Bits a) => a -> Int -> Bool
firstBitsZero val diff = (val .&. (bitMaskZero diff)) == zeroBits 

formatTemplate :: String -> [String] -> String
formatTemplate base [] = base
formatTemplate base (x:xs) = 
   let splix = (splitOn "{" base) :: [String]
       (splixHead:splixTail) = splix
       concatSplitTail = tail $ concatMap ('{' :) splixTail
   in formatTemplate (splixHead ++ (x ++ concatSplitTail)) xs

getRandomBytes'' :: (DRG g) => g -> Int -> (ByteString, g)
getRandomBytes'' g num = randomBytesGenerate num g

getBaseTemplate :: ByteString -> HashCashSpec -> String
getBaseTemplate bs spec =
  let encodedVal = B64.encode bs
      baseParams = [(convertIntToString $ difficulty spec),
        (timestamp spec),
        (value spec),
        (convertToString encodedVal)]
  in formatTemplate template baseParams
  
getBaseString :: String -> Int32 -> String
getBaseString s counter = 
  let encodedCounter = encodeInt32 counter
      baseParams = [(convertToString encodedCounter)]
  in formatTemplate s baseParams
  
hashSHA1Encoded :: ByteString -> ByteString
hashSHA1Encoded bs = B.pack . BA.unpack $ hashDigest
  where hashDigest = hash bs :: Digest SHA1

testCounterBool :: HashCashSpec -> String -> Int32 -> Bool
testCounterBool spec s counter =
  let baseString = getBaseString s counter
      hashedString = hashSHA1Encoded $ convertFromString baseString
      eitherFirst32 = runGet mahDecoder hashedString
  in case eitherFirst32 of
    (Left first32, _) -> False
    (Right first32, _) -> firstBitsZero first32 (difficulty spec)

generateHeaderAsync :: HashCashSpec -> IO String
generateHeaderAsync spec = do
  g <- getSystemDRG
  -- Get 16 random bytes for the random value
  let (ran, g2) = getRandomBytes'' g 16
  -- get 4 * threads additional bytes for random starting counters for each thread
  let (rand, _) = getRandomBytes'' g2 ((threads spec) * 4)
  let rints = case (runGet getWords rand) of
             (Left arr, _) -> []
             (Right arr, _) -> arr
  x <- newEmptyMVar :: IO (MVar Int32)
  let funcs = map (\y -> (generateHeaderThread ran spec x (abs y))) (sort rints)
  mapM_ (\y -> forkIO $ y) funcs
  validCounter <- takeMVar x
  let validBase = getBaseTemplate ran spec
  let validHeader = getBaseString validBase validCounter
  return $ headerPrefix ++ validHeader

generateHeaderThread :: ByteString -> HashCashSpec -> MVar (Int32) -> Int32 -> IO ()
generateHeaderThread ran spec validMVar start = do
  putStrLn $ "Starting a thread at " ++ (show start)
  let s = getBaseTemplate ran spec
  -- This is where the bulk of work occurs.  It will start at the random number and continue
  -- testing numbers for where the first p bits are zero.
  let validCounter = find (testCounterBool spec s) [start..]
  if validCounter == Nothing 
    then do
      putStrLn "Thread did not find anything"
        else do
          putMVar validMVar (fromJust validCounter)

validateHeader :: String -> Bool
validateHeader s = 
  let hashedString = hashSHA1Encoded $ convertFromString $ s
      eitherFirst32 = runGet mahDecoder hashedString
      (vers:diff:others) = (splitOn ":" s) :: [String]
  in case eitherFirst32 of
    (Left first32, _) -> False
    (Right first32, _) -> firstBitsZero first32 (read diff)

{-# LANGUAGE BangPatterns #-}

module HashCash where

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
import           Data.List (find)
import           Data.List.Split (splitOn)
import           Data.Maybe (fromJust)
import           Data.Word (Word32)
import           System.Random

data HashCashSpec = HashCashSpec { version :: Int
                                 , difficulty :: Int
                                 , value :: String
                                 , timestamp :: String
                                 } deriving (Show) 

defaultHashCashSpec = HashCashSpec {version=1, difficulty=20, value="a@a", timestamp="150320112233"}
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

encodeInt32 :: Int32 -> ByteString
encodeInt32 = B64.encode . BCON.toByteString'

--OLD DECODER
mahDecoder :: Get Word32
mahDecoder = do
  first32Bits <- getWord32be
  return first32Bits

-- New decoder using Monad syntax, TODO
decoder :: Get Word32
decoder = getWord32be

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
   
get16RandomBytes :: (DRG g) => g -> (ByteString, g)
get16RandomBytes = randomBytesGenerate 16

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

-- Keep taking incrementing counters from an infinite list and testing them until we find a counter 
-- that generates a valid header
findValidCounter :: ByteString -> HashCashSpec -> Int32
findValidCounter ran spec = fromJust $ find (testCounterBool spec s) [1..]
  where s = getBaseTemplate ran spec

generateHeader :: HashCashSpec -> IO String
generateHeader spec = do
  g <- getSystemDRG
  let (ran, _) = get16RandomBytes g
  let validCounter = findValidCounter ran spec
  let validBase = getBaseTemplate ran spec
  let validHeader = getBaseString validBase validCounter
  return $ headerPrefix ++ validHeader
  
validateHeader :: String -> Bool
validateHeader s = 
  let hashedString = hashSHA1Encoded $ convertFromString $ s
      eitherFirst32 = runGet mahDecoder hashedString
      (vers:diff:others) = (splitOn ":" s) :: [String]
  in case eitherFirst32 of
    (Left first32, _) -> False
    (Right first32, _) -> firstBitsZero first32 (read diff)
  
      
 
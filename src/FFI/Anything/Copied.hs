-- | Contains some functions needed from the msgpack package
-- that are unfortunately not yet exposed.
--
-- See https://github.com/msgpack/msgpack-haskell/pull/34.
module FFI.Anything.Copied where

import           Blaze.ByteString.Builder
import qualified Data.Attoparsec.ByteString as A
import           Data.Bits
import           Data.Monoid
import           Data.Word
import           Text.Printf


-- Copied from Data.MessagePack.Pack
fromArray :: (a -> Int) -> (a -> Builder) -> a -> Builder
fromArray lf pf arr = do
  case lf arr of
    len | len <= 15 ->
      fromWord8 $ 0x90 .|. fromIntegral len
    len | len < 0x10000 ->
      fromWord8 0xDC <>
      fromWord16be (fromIntegral len)
    len ->
      fromWord8 0xDD <>
      fromWord32be (fromIntegral len)
  <> pf arr


-- Copied from Data.MessagePack.Unpack
parseArray :: (Int -> A.Parser a) -> A.Parser a
parseArray aget = do
  c <- A.anyWord8
  case c of
    _ | c .&. 0xF0 == 0x90 ->
      aget . fromIntegral $ c .&. 0x0F
    0xDC ->
      aget . fromIntegral =<< parseUint16
    0xDD ->
      aget . fromIntegral =<< parseUint32
    _ ->
      fail $ printf "invalid array tag: 0x%02X" c


parseUint16 :: A.Parser Word16
parseUint16 = do
  b0 <- A.anyWord8
  b1 <- A.anyWord8
  return $ (fromIntegral b0 `shiftL` 8) .|. fromIntegral b1


parseUint32 :: A.Parser Word32
parseUint32 = do
  b0 <- A.anyWord8
  b1 <- A.anyWord8
  b2 <- A.anyWord8
  b3 <- A.anyWord8
  return $ (fromIntegral b0 `shiftL` 24) .|.
           (fromIntegral b1 `shiftL` 16) .|.
           (fromIntegral b2 `shiftL` 8) .|.
           fromIntegral b3

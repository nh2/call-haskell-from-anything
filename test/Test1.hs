{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, GADTs, TypeOperators #-}

module FFI.Python where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Monoid
import qualified Data.MessagePack as MSG
import           Blaze.ByteString.Builder
import           Data.Int

import Foreign.C

import FFI.Python.TH (deriveCallable)
import FFI.Python.Copied

infixr :::
data ParamList l where
  Nil :: ParamList '[]
  (:::) :: (MSG.Packable a) => a -> ParamList l -> ParamList (a ': l)

paramLength :: ParamList l -> Int
paramLength Nil = 0
paramLength (_ ::: ls) = 1 + paramLength ls

pack :: ParamList l -> Builder
pack Nil = mempty
pack (a ::: as) = MSG.from a <> pack as

instance MSG.Packable (ParamList l) where
  from ls = fromArray (const $ paramLength ls) pack ls

x :: ParamList '[String, String]
x = "a" ::: "b" ::: Nil


-- unpack ::

-- | Example function to be called from Python.
f1 :: Int -> Double -> String
f1 i f = "Called with params: " ++ show i ++ ", " ++ show f


-- To be translated to:
f1' :: ByteString -> ByteString
f1' bs = mconcat . BSL.toChunks $ MSG.pack (uncurry f1 $ msg)
  where
    msg = case MSG.tryUnpack bs of
      Left e  -> error $ "tryUnpack: " ++ e
      Right r -> r


foreign export ccall f1_hs :: CString -> IO CString
f1_hs :: CString -> IO CString
f1_hs cs = do
    cs_bs <- BS.packCString cs
    let res_bs = f1' cs_bs
    res_cs <- BS.useAsCString res_bs return
    return res_cs

$(deriveCallable 'f1 "f1_hs")

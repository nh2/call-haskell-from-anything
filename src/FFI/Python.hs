{-# LANGUAGE ForeignFunctionInterface #-}

module FFI.Python where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Foreign.C
import Foreign.StablePtr

import Debug.Trace


-- | Example function to be called from Python.
f1 :: Int -> Float -> String
f1 i f = show i ++ " " ++ show f


-- To be translated to:
f1' :: ByteString -> ByteString
f1' bs = trace (show bs) bs


foreign export ccall f1_hs :: CString -> IO CString
f1_hs :: CString -> IO CString
f1_hs cs = do
    cs_bs <- BS.packCString cs
    let res_bs = f1' cs_bs
    res_cs <- BS.useAsCString res_bs return
    return res_cs

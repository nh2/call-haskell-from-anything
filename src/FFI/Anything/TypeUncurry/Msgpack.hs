{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeOperators, ScopedTypeVariables, FlexibleContexts #-}

-- | Easy FFI via MessagePack.
--
-- You can use this module to expose any Haskell function to other Programming languages.
--
-- It allows to convert functions that take multiple arguments
-- into functions that take one argument:
-- A 'ByteString' which contains all arguments encoded as a MessagePack array.
--
-- Common use cases:
--
-- * Write functions in fast native Haskell code, compile them into a dynamic.
--   library (@.so@ \/ @.dll@) and call them via C\/Python\/Ruby\/whatever via @dlopen()@ or equivalents.
--
-- * Expose Haskell functions via a socket / the web
module FFI.Anything.TypeUncurry.Msgpack (
  MessagePackRec (..)
, getTypeListFromMsgpackArray
, uncurryMsgpack
, tryUncurryMsgpack
, tryUncurryMsgpackIO
, byteStringToCStringFun
, byteStringToCStringFunIO
, export
, exportIO
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe (fromMaybe)
import qualified Data.MessagePack as MSG
import           Data.Proxy
import           Foreign.C

import FFI.Anything.TypeUncurry


-- | Helper to allow writing a 'MSG.MessagePack' instance for 'TypeList's.
--
-- We need this because we have to call 'parseArray' at the top-level
-- 'MSG.MessagePack' instance, but not at each function argument step.
class MessagePackRec l where
  fromObjectRec :: (Monad m) => [MSG.Object] -> m (TypeList l)

-- | When no more types need to be unpacked, we are done.
instance MessagePackRec '[] where
  fromObjectRec v | null v = pure Nil
  fromObjectRec _          = fail "fromObjectRec: passed object is not expected []"

-- | Unpack one type by just parsing the next element.
instance (MSG.MessagePack a, MessagePackRec l) => MessagePackRec (a ': l) where
  fromObjectRec (x:xs) = (:::) <$> MSG.fromObject x <*> fromObjectRec xs
  fromObjectRec _      = fail "fromObjectRec: passed object is not expected (x:xs)"

-- | Parses a tuple of arbitrary size ('TypeList's) from a MessagePack array.
getTypeListFromMsgpackArray :: forall m l . (MessagePackRec l, ParamLength l, Monad m) => MSG.Object -> m (TypeList l)
getTypeListFromMsgpackArray obj = case obj of
    MSG.ObjectArray v | length v == len -> fromObjectRec v
    _                                   -> fail "getTypeListFromMsgpackArray: wrong object length"
  where
    len = paramLength (Proxy :: Proxy l)

instance (MessagePackRec l, ParamLength l) => MSG.MessagePack (TypeList l) where
  fromObject = getTypeListFromMsgpackArray
  toObject = error "call-haskell-from-anything: Serialising a TypeList is not supported (and not needed)!"


-- | Standard error message when unpacking failed.
errorMsg :: String -> String
errorMsg locationStr = "call-haskell-from-anything: " ++ locationStr ++ ": got wrong number of function arguments or non-array"


-- | Translates a function of type @a -> b -> ... -> r@ to
-- a function that:
--
-- * takes as a single argument a 'ByteString' containing all arguments serialized in a MessagePack array
--
-- * returns its result serialized in a 'ByteString' via MessagePack 'MSG.pack'
--
-- This function throws an 'error' if the de-serialization of the arguments fails!
-- It is recommended to use 'tryUncurryMsgpack' instead.
uncurryMsgpack :: (MSG.MessagePack (TypeList l), ToTypeList f l r, MSG.MessagePack r) => f -> (ByteString -> ByteString)
uncurryMsgpack f = \bs -> BSL.toStrict . MSG.pack $ (translate f $ fromMaybe (error (errorMsg "uncurryMsgpack")) $ MSG.unpack $ BSL.fromStrict bs)


-- | Like 'uncurryMsgpack', but for 'IO' functions.
--
-- This function throws an 'error' if the de-serialization of the arguments fails!
-- It is recommended to use 'tryUncurryMsgpackIO' instead.
uncurryMsgpackIO :: (MSG.MessagePack (TypeList l), ToTypeList f l (IO r), MSG.MessagePack r) => f -> (ByteString -> IO ByteString)
uncurryMsgpackIO f = \bs -> BSL.toStrict . MSG.pack <$> (translate f $ fromMaybe (error (errorMsg "uncurryMsgpackIO")) $ MSG.unpack $ BSL.fromStrict bs)


-- | Like 'uncurryMsgpack', but makes it clear when the 'ByteString' containing
-- the function arguments does not contain the right number/types of arguments.
tryUncurryMsgpack :: (MSG.MessagePack (TypeList l), ToTypeList f l r, MSG.MessagePack r) => f -> (ByteString -> Maybe ByteString)
tryUncurryMsgpack f = \bs -> case MSG.unpack $ BSL.fromStrict bs of
  Nothing   -> Nothing
  Just args -> Just . BSL.toStrict . MSG.pack $ (translate f $ args)


-- | Like 'uncurryMsgpack', but makes it clear when the 'ByteString' containing
-- the function arguments does not contain the right number/types of arguments.
tryUncurryMsgpackIO :: (MSG.MessagePack (TypeList l), ToTypeList f l (IO r), MSG.MessagePack r) => f -> (ByteString -> Maybe (IO ByteString))
tryUncurryMsgpackIO f = \bs -> case MSG.unpack $ BSL.fromStrict bs of
  Nothing   -> Nothing
  Just args -> Just $ BSL.toStrict . MSG.pack <$> (translate f $ args)


-- * Exporting

-- TODO implement via byteStringToCStringFunIO?
-- | Transforms a 'ByteString'-mapping function to 'CString'-mapping function
-- for use in the FFI.
byteStringToCStringFun :: (ByteString -> ByteString) -> CString -> IO CString
byteStringToCStringFun f cs = do
  cs_bs <- BS.packCString cs
  let res_bs = f cs_bs
  res_cs <- BS.useAsCString res_bs return
  return res_cs


-- | Transforms a 'ByteString'-mapping 'IO' function to 'CString'-mapping function
-- for use in the FFI.
byteStringToCStringFunIO :: (ByteString -> IO ByteString) -> CString -> IO CString
byteStringToCStringFunIO f cs = do
  cs_bs <- BS.packCString cs
  res_bs <- f cs_bs
  res_cs <- BS.useAsCString res_bs return
  return res_cs


-- | Exports a "pure" function
-- to an FFI function that takes its arguments as a serialized MessagePack message.
--
-- Calling this function throws an 'error' if the de-serialization of the arguments fails!
-- Use 'tryExport' if you want to handle this case.
export :: (MSG.MessagePack (TypeList l), ToTypeList f l r, MSG.MessagePack r) => f -> CString -> IO CString
export = byteStringToCStringFun . uncurryMsgpack


-- | Exports an 'IO' function to an FFI function that takes its arguments as a serialized MessagePack message.
--
-- Calling this function throws an 'error' if the de-serialization of the arguments fails!
-- Use 'tryExportIO' if you want to handle this case.
exportIO :: (MSG.MessagePack (TypeList l), ToTypeList f l (IO r), MSG.MessagePack r) => f -> CString -> IO CString
exportIO = byteStringToCStringFunIO . uncurryMsgpackIO


-- TODO make equivalent using tryUncurryMsgpack (tryExport)
-- TODO make equivalent using tryUncurryMsgpackIO (tryExport)

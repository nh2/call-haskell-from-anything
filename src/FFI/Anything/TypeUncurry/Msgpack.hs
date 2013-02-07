{-# OPTIONS_GHC -cpp -fno-warn-orphans #-}

#if MIN_VERSION_base(4,6,0)
#define USE_DATA_KINDS 1
#endif

#if USE_DATA_KINDS
{-# LANGUAGE DataKinds, TypeOperators #-}
#endif
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
  UnpackableRec (..)
, getTypeListFromMsgpackArray
, uncurryMsgpack
, tryUncurryMsgpack
, tryUncurryMsgpackIO
, byteStringToCStringFun
, byteStringToCStringFunIO
, export
, exportIO
, module FFI.Anything.TypeUncurry.ReturnResult
) where

import           Control.Applicative
import qualified Data.Attoparsec as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.MessagePack as MSG
import           Foreign.C
import           Text.Printf

import FFI.Anything.Copied
import FFI.Anything.Util
import FFI.Anything.TypeUncurry.ReturnResult

-- For GHC 7.6 or newer, we import TypeUncurry which uses DataKinds for TypeList to be kind-safe.
-- For all other versions, import TypeUncurryLegacy which uses a simpler model of TypeList.
import FFI.Anything.TypeUncurry


-- | Helper to allow writing a 'MSG.Unpackable' instance for 'TypeList's.
--
-- We need this because we have to call 'parseArray' at the top-level
-- 'MSG.Unpackable' instance, but not at each function argument step.
class UnpackableRec l where
  getRec :: A.Parser (TypeList l)

-- | When no more types need to be unpacked, we are done.
#if USE_DATA_KINDS
instance UnpackableRec '[] where -- For GHC 7.6 or newer, use DataKinds.
#else
instance UnpackableRec Nil where
#endif
  getRec = return Nil

-- | Unpack one type by just parsing the next element.
#if USE_DATA_KINDS
instance (MSG.Unpackable a, UnpackableRec l) => UnpackableRec (a ': l) where  -- For GHC 7.6 or newer, use DataKinds.
#else
instance (MSG.Unpackable a, UnpackableRec l) => UnpackableRec (a ::: l) where
#endif
  getRec = (:::) <$> MSG.get <*> getRec



-- | Parses a tuple of arbitrary size ('TypeList's) from a MessagePack array.
getTypeListFromMsgpackArray :: forall l . (UnpackableRec l, ParamLength l) => A.Parser (TypeList l)
getTypeListFromMsgpackArray = parseArray f
  where
    len = paramLength (Proxy :: Proxy l)
    f n | n == len  = getRec
        -- TODO also print function name
        | otherwise = fail $ printf "getTypeListFromMsgpackArray: wrong number of function arguments: expected %d but got %d" len n


instance (UnpackableRec l, ParamLength l) => MSG.Unpackable (TypeList l) where
  get = getTypeListFromMsgpackArray



-- | Translates a function of type @a -> b -> ... -> Identity r@ to
-- a function that:
--
-- * takes as a single argument a 'ByteString' containing all arguments serialized in a MessagePack array
--
-- * returns its result serialized in a 'ByteString' via MessagePack 'MSG.pack'
--
-- This function throws an 'error' if the de-serialization of the arguments fails!
-- It is recommended to use 'tryUncurryMsgpack' instead.
uncurryMsgpack :: (MSG.Unpackable (TypeList l), ToTypeList f l r, MSG.Packable r) => f -> (ByteString -> ByteString)
uncurryMsgpack f = \bs -> lazyToStrictBS . MSG.pack $ (translate f $ MSG.unpack bs)


-- | Like 'uncurryMsgpack', but for 'IO' functions.
--
-- This function throws an 'error' if the de-serialization of the arguments fails!
-- It is recommended to use 'tryUncurryMsgpackIO' instead.
uncurryMsgpackIO :: (MSG.Unpackable (TypeList l), ToTypeList f l (IO r), MSG.Packable r) => f -> (ByteString -> IO ByteString)
uncurryMsgpackIO f = \bs -> lazyToStrictBS . MSG.pack <$> (translate f $ MSG.unpack bs)


-- | Like 'uncurryMsgpack', but makes it clear when the 'ByteString' containing
-- the function arguments does not contain the right number/types of arguments.
tryUncurryMsgpack :: (MSG.Unpackable (TypeList l), ToTypeList f l r, MSG.Packable r) => f -> (ByteString -> Either String ByteString)
tryUncurryMsgpack f = \bs -> case MSG.tryUnpack bs of
  Left e     -> Left e
  Right args -> Right . lazyToStrictBS . MSG.pack $ (translate f $ args)


-- | Like 'uncurryMsgpack', but makes it clear when the 'ByteString' containing
-- the function arguments does not contain the right number/types of arguments.
tryUncurryMsgpackIO :: (MSG.Unpackable (TypeList l), ToTypeList f l (IO r), MSG.Packable r) => f -> (ByteString -> Either String (IO ByteString))
tryUncurryMsgpackIO f = \bs -> case MSG.tryUnpack bs of
  Left e     -> Left e
  Right args -> Right $ lazyToStrictBS . MSG.pack <$> (translate f $ args)


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


-- | Exports a "pure" function (usually it has to be wrapped in the Identity monad)
-- to an FFI function that takes its arguments as a serialized MessagePack message.
--
-- Calling this function throws an 'error' if the de-serialization of the arguments fails!
-- Use 'tryExport' if you want to handle this case.
export :: (MSG.Unpackable (TypeList l), ToTypeList f l r, MSG.Packable r) => f -> CString -> IO CString
export = byteStringToCStringFun . uncurryMsgpack


-- | Exports an 'IO' function to an FFI function that takes its arguments as a serialized MessagePack message.
--
-- Calling this function throws an 'error' if the de-serialization of the arguments fails!
-- Use 'tryExportIO' if you want to handle this case.
exportIO :: (MSG.Unpackable (TypeList l), ToTypeList f l (IO r), MSG.Packable r) => f -> CString -> IO CString
exportIO = byteStringToCStringFunIO . uncurryMsgpackIO


-- TODO make equivalent using tryUncurryMsgpack (tryExport)
-- TODO make equivalent using tryUncurryMsgpackIO (tryExport)

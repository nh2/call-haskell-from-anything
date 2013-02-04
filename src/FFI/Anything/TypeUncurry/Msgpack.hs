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
module FFI.Anything.TypeUncurry.Msgpack where

import           Control.Applicative
import qualified Data.Attoparsec as A
import           Data.ByteString (ByteString)
import qualified Data.MessagePack as MSG
import           Text.Printf

import FFI.Anything.Copied
import FFI.Anything.Util

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
uncurryMsgpack f = \bs -> lazyToStrictBS $ MSG.pack (translate f $ MSG.unpack bs)


-- | Like 'uncurryMsgpack', but makes it clear when the 'ByteString' containing
-- the function arguments does not contain the right number/types of arguments.
tryUncurryMsgpack :: (MSG.Unpackable (TypeList l), ToTypeList f l r, MSG.Packable r) => f -> (ByteString -> Either String ByteString)
tryUncurryMsgpack f = \bs -> case MSG.tryUnpack bs of
  Left e     -> Left e
  Right args -> Right $ lazyToStrictBS $ MSG.pack (translate f $ args)

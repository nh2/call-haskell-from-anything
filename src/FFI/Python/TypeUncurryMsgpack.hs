{-# OPTIONS_GHC -cpp -fno-warn-orphans #-}

#if MIN_VERSION_base(4,6,0)
#define USE_DATA_KINDS 1
#endif

#if USE_DATA_KINDS
{-# LANGUAGE DataKinds, TypeOperators #-}
#endif
{-# LANGUAGE TypeOperators, ScopedTypeVariables, FlexibleContexts #-}

module FFI.Python.TypeUncurryMsgpack where

import           Control.Applicative
import qualified Data.Attoparsec as A
import           Data.ByteString (ByteString)
import qualified Data.MessagePack as MSG
import           Text.Printf

import FFI.Python.Copied
import FFI.Python.Util

-- For GHC 7.6 or newer, we import TypeUncurry which uses DataKinds for TypeList to be kind-safe.
-- For all other versions, import TypeUncurryLegacy which uses a simpler model of TypeList.
#if USE_DATA_KINDS
import FFI.Python.TypeUncurry
#else
import FFI.Python.TypeUncurryLegacy
#endif


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
getTypeListFromArray :: forall l . (UnpackableRec l, ParamLength l) => A.Parser (TypeList l)
getTypeListFromArray = parseArray f
  where
    len = paramLength (undefined :: Proxy l)
    f n | n == len  = getRec
        -- TODO also print function name
        | otherwise = fail $ printf "getTypeListFromArray: wrong number of function arguments: expected %d but got %d" len n


instance (UnpackableRec l, ParamLength l) => MSG.Unpackable (TypeList l) where
  get = getTypeListFromArray



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

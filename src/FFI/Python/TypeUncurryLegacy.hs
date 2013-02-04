{-# LANGUAGE GADTs, TypeOperators, FunctionalDependencies, FlexibleInstances, TypeFamilies, PolyKinds, ScopedTypeVariables, FlexibleContexts, EmptyDataDecls #-}

module FFI.Python.TypeUncurryLegacy where

import           Control.Applicative
import           Control.Monad.Identity
import qualified Data.Attoparsec as A
import           Data.ByteString (ByteString)
import qualified Data.MessagePack as MSG
import           Text.Printf

import FFI.Python.Copied
import FFI.Python.Util


-- OLDSTYLE for GHC < 7.6


data Proxy k = Proxy


data LNil -- '[]
data a :~ b -- ':


data PList l where
  LNil :: PList LNil
  (:~) :: a -> PList l -> PList (a :~ l)


infixr :~


class ParamLengthOld l where
  paramLengthOld :: Proxy l -> Int

instance ParamLengthOld LNil where
  paramLengthOld _ = 0

instance (ParamLengthOld l) => ParamLengthOld (a :~ l) where
  paramLengthOld _ = succ $ paramLengthOld (undefined :: Proxy l)



type family Param2 f
type instance Param2 (Identity r) = LNil
type instance Param2 (a -> f) = a :~ Param2 f

type family Result2 f
type instance Result2 (Identity r) = r
type instance Result2 (a -> f) = Result2 f


class (Param2 f ~ l, Result2 f ~ r) => ToParamList2 f l r where
  translate2 :: f -> PList l -> r

instance (ToParamList2 f l r) => ToParamList2 (a -> f) (a :~ l) r where
  translate2 f (a :~ l) = translate2 (f a) l

instance ToParamList2 (Identity r) LNil r where
  translate2 (Identity r) LNil = r



instance (Unpackable2 l, ParamLengthOld l) => MSG.Unpackable (PList l) where
  get = parseArray f
    where
      len = paramLengthOld (undefined :: Proxy l)
      f n | n == len = get2
          -- TODO also print function name
          | otherwise = fail $ printf "wrong number of function arguments: expected %d but got %d" len n


-- TODO rename this
class Unpackable2 l where
  get2 :: A.Parser (PList l)

instance Unpackable2 LNil where
  get2 = return LNil

instance (MSG.Unpackable a, Unpackable2 l) => Unpackable2 (a :~ l) where
  get2 = (:~) <$> MSG.get <*> get2


-- TODO tryUnpack
translateCall2 :: (MSG.Unpackable (PList l), ToParamList2 f l r, MSG.Packable r) => f -> (ByteString -> ByteString)
translateCall2 f = \bs -> lazyToStrictBS $ MSG.pack (transed_f (MSG.unpack bs))
  where
    transed_f = translate2 f

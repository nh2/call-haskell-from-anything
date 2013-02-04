{-# LANGUAGE GADTs, TypeOperators, MultiParamTypeClasses, FlexibleInstances, TypeFamilies, PolyKinds, ScopedTypeVariables, FlexibleContexts #-}

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


data Nil -- '[]
data a ::: b -- ':


data TypeList l where
  Nil :: TypeList Nil
  (:::) :: a -> TypeList l -> TypeList (a ::: l)


infixr :::


class ParamLength l where
  paramLength :: Proxy l -> Int

instance ParamLength Nil where
  paramLength _ = 0

instance (ParamLength l) => ParamLength (a ::: l) where
  paramLength _ = succ $ paramLength (undefined :: Proxy l)



type family Param f
type instance Param (Identity r) = Nil
type instance Param (a -> f) = a ::: Param f

type family Result f
type instance Result (Identity r) = r
type instance Result (a -> f) = Result f


class (Param f ~ l, Result f ~ r) => ToTypeList f l r where
  translate :: f -> TypeList l -> r

instance (ToTypeList f l r) => ToTypeList (a -> f) (a ::: l) r where
  translate f (a ::: l) = translate (f a) l

instance ToTypeList (Identity r) Nil r where
  translate (Identity r) Nil = r



class UnpackableRec l where
  getRec :: A.Parser (TypeList l)

instance UnpackableRec Nil where
  getRec = return Nil

instance (MSG.Unpackable a, UnpackableRec l) => UnpackableRec (a ::: l) where
  getRec = (:::) <$> MSG.get <*> getRec


instance (UnpackableRec l, ParamLength l) => MSG.Unpackable (TypeList l) where
  get = parseArray f
    where
      len = paramLength (undefined :: Proxy l)
      f n | n == len = getRec
          -- TODO also print function name
          | otherwise = fail $ printf "wrong number of function arguments: expected %d but got %d" len n


-- TODO tryUnpack
uncurryMsgpack :: (MSG.Unpackable (TypeList l), ToTypeList f l r, MSG.Packable r) => f -> (ByteString -> ByteString)
uncurryMsgpack f = \bs -> lazyToStrictBS $ MSG.pack (translate f $ MSG.unpack bs)

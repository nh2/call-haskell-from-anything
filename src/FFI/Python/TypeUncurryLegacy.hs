{-# LANGUAGE GADTs, TypeOperators, MultiParamTypeClasses, FlexibleInstances, TypeFamilies, ScopedTypeVariables, FlexibleContexts #-}

module FFI.Python.TypeUncurryLegacy where

import           Control.Monad.Identity


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

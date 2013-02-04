{-# LANGUAGE GADTs, TypeOperators, MultiParamTypeClasses, FlexibleInstances, TypeFamilies, ScopedTypeVariables, FlexibleContexts #-}

-- | Converts function arguments to tuple-like types.
--
-- This module is the LEGACY module not using DataKinds.
-- Compilers supporting DataKinds (e.g. GHC >= 7.6) should use "FFI.Anything.TypeUncurry".
--
-- For example, take @f :: a -> b -> c -> r@.
-- This module can convert it to @f' :: (a, b, c) -> r@, at compile time.
--
-- This is especially useful for (de)serialization.
-- Suppose you have a function that takes multiple arguments
-- and you want to obtain all of its arguments from some serialized data.
-- The serialization library will make it very easy to unpack types
-- like tuples/lists, but de-serializing *fuction arguments* is not that simple.
--
-- Using this module, you can write an instance how to unpack the 'TypeList' type,
-- and then use 'translateCall' to make any function take such a single 'TypeList'
-- instead of multiple function arguments.
--
-- There is currently a technical limitation:
-- The result type must be wrapped in the 'Identity' monad.
--
-- Example:
--
-- >-- Assume your library provides some unpack function, e.g. it allows you to write:
-- >unpack someBytestring :: (Int, String, Double)
-- >
-- >-- and you have a function
-- >f :: Int -> String -> Double -> Identity Char
-- >
-- >-- then you can use:
-- >f' :: (Int, String, Double) -> Identity Char
-- >f' = translateCall f
-- >
-- >result = f' (unpack someBytestring)
module FFI.Anything.TypeUncurry.Legacy where

import           Control.Monad.Identity


-- * Type-level lists (containing types)

-- | The empty type-level type list
data Nil
-- | A type attached to a type-level type list
data a ::: b

-- | Type-level list that can contain arbitrarily mixed types.
--
-- Example:
--
-- >1 ::: "hello" ::: 2.3 :: TypeList (Int ::: String ::: Double ::: Nil)
data TypeList l where
  Nil :: TypeList Nil
  (:::) :: a -> TypeList l -> TypeList (a ::: l)

-- Right-associativity, like (->)
infixr :::


-- * \"Uncurrying\" functions

{- In the following, we try to not use Template Haskell,
   using an instance of (a -> ...) to convert functions to TypeLists automatically
   (similar to how you make variadic functions).
-}

-- | Arguments to a function, e.g. @[String, Int]@ for @String -> Int -> r@.
type family Param f
-- | We need an 'Identity' moad wrapper here to not conflict with @a -> f@.
type instance Param (Identity r) = Nil
type instance Param (a -> f) = a ::: Param f

-- | The result of a function, e.g. @r@ for @String -> Int -> r@.
type family Result f
-- | We need an 'Identity' monad wrapper here to not conflict with @a -> f@.
type instance Result (Identity r) = r
type instance Result (a -> f) = Result f


-- | Function f can be translated to 'TypeList' l with result type r.
class (Param f ~ l, Result f ~ r) => ToTypeList f l r where
  -- | Translates a function taking multiple arguments to a function
  -- taking a single 'TypeList' containing the types of all arguments.
  --
  -- Example: @t1 -> ... -> tn -> r@ becomes @TypeList (t1 ::: ... ::: tn ::: Nil) -> r@.
  translate :: f -> TypeList l -> r

-- | Base case: A function without arguments (just @Identity r@)
-- can be translated to @TypeList Nil -> r@.
instance (ToTypeList f l r) => ToTypeList (a -> f) (a ::: l) r where
  translate f (a ::: l) = translate (f a) l

-- | Recursive case: A function of type @a -> ... -> r@
-- can be translated to @TypeList (a ::: ...) -> r@.
instance ToTypeList (Identity r) Nil r where
  translate (Identity r) Nil = r


-- * Length of type-level lists

-- | A proxy type that can contain an arbitrary type.
--
-- Needed for some type-level computations, like 'paramLength'.
data Proxy k = Proxy


-- | Allows to calculate the length of a 'TypeList', at compile time.
--
-- We need to use a 'Proxy' for this.
class ParamLength l where
  -- | Calculates the length of a type list, put into a proxy. Usage:
  --
  -- >paramLength (undefined :: Proxy l)
  paramLength :: Proxy l -> Int

instance ParamLength Nil where
  paramLength _ = 0

instance (ParamLength l) => ParamLength (a ::: l) where
  paramLength _ = succ $ paramLength (undefined :: Proxy l)

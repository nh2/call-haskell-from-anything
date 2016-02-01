{-# LANGUAGE DataKinds, GADTs, TypeOperators, MultiParamTypeClasses, FlexibleInstances, TypeFamilies, PolyKinds, ScopedTypeVariables #-}

-- | Converts function arguments to tuple-like types.
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
-- and then use 'translate' to make any function take such a single 'TypeList'
-- instead of multiple function arguments.
module FFI.Anything.TypeUncurry.DataKinds where

import           Data.Proxy


-- * Type-level lists (containing types)

-- NOTE: GHC 7.4 cannot deal with DataKinds
-- (see http://hackage.haskell.org/trac/ghc/ticket/5881)
--
-- This is why we have two separate implementations:
-- - one with DataKinds for GHC >= 7.6
-- - one with a standard Nil / Cons type-level list for older
--   compilers which is not kind-safe

-- | Type-level list that can contain arbitrarily mixed types.
--
-- Example:
--
-- >1 ::: "hello" ::: 2.3 :: TypeList '[Int, String, Double]
data TypeList l where
  Nil :: TypeList '[] -- TODO make singleton list, not empty list, base type?
  (:::) :: a -> TypeList l -> TypeList (a ': l)

-- Right-associativity, like (->)
infixr :::

-- Example: You can write:
--
-- exampleTypeList :: TypeList '[String, Int]
-- exampleTypeList = "a" ::: 3 ::: Nil


-- * \"Uncurrying\" functions

{- In the following, we try to not use Template Haskell,
   using an instance of (a -> ...) to convert functions to TypeLists automatically
   (similar to how you make variadic functions).
-}

-- | Arguments to a function, e.g. @[String, Int]@ for @String -> Int -> r@.
type family Param f :: [*] where
  Param (a -> f) = a ': Param f
  Param r = '[]

-- | The result of a function, e.g. @r@ for @String -> Int -> r@.
type family Result f :: * where
  Result (a -> f) = Result f
  Result r = r


-- | Function f can be translated to 'TypeList' l with result type r.
class (Param f ~ l, Result f ~ r) => ToTypeList f l r where
  -- | Translates a function taking multiple arguments to a function
  -- taking a single 'TypeList' containing the types of all arguments.
  --
  -- Example: @t1 -> ... -> tn -> r@ becomes @TypeList [t1, ..., tn] -> r@.
  translate :: f -> TypeList l -> r


-- | Base case: A "pure" function without arguments
-- can be translated to @TypeList Nil -> r@.
instance (ToTypeList f l r) => ToTypeList (a -> f) (a ': l) r where
  translate f (a ::: l) = translate (f a) l

-- | Base case: A value @r@ can be translated to @TypeList Nil -> r@.
instance (Param f ~ '[], Result f ~ r, f ~ r) => ToTypeList f '[] r where
  -- Could also be written as
  --   (Param r ~ '[], Result r ~ r) => ToTypeList r '[] r
  -- but I find the other way clearer.
  translate r Nil = r


-- Now an example:
--
-- someFunction :: Int -> Double -> String
-- someFunction _i _d = return "asdf"
--
-- exampleAutoTranslate = translate someFunction
--
-- -- ghci would give as type for this: TypeList ((:) * Int ((:) * Double ([] *))) -> [Char]


-- * Length of type-level lists

-- | Allows to calculate the length of a 'TypeList', at compile time.
--
-- We need to use a 'Proxy' for this.
class ParamLength (l :: [*]) where
  -- | Calculates the length of a type list, put into a proxy. Usage:
  --
  -- >paramLength (Proxy :: Proxy l)
  paramLength :: Proxy l -> Int

instance ParamLength '[] where
  paramLength _ = 0

instance (ParamLength l) => ParamLength (a ': l) where
  paramLength _ = succ $ paramLength (Proxy :: Proxy l)

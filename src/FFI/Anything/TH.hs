module FFI.Anything.TH where

import Language.Haskell.TH

import Debug.Trace




parameters :: Type -> [Type]  -- Result list are "ground" types
parameters t = case t of
  AppT t1 t2 -> parameters t1 ++ parameters t2
  ArrowT     -> []
  ConT name  -> [ConT name]
  -- TODO handle ListT, TupleT and so on
  _          -> error $ "parameters: unhandled Type " ++ show t


-- TODO better use custom data type, tuples are quite finite
argTypesToTuple :: [Type] -> Type
argTypesToTuple types = foldl f (TupleT n) types
  where
    f a next = AppT a next
    n = length types


debug :: (Show a, Monad m) => a -> m ()
debug x = trace ("\n" ++ show x ++ "\n") $ return ()


deriveCallable :: Name -> String -> Q [Dec]
deriveCallable funName exportedName = do
  info <- reify funName
  case info of
    VarI name typ _mDec -> do
      let _nameString   = nameBase name
          signatureList = parameters typ
          paramTypes    = init signatureList
          returnType    = last signatureList

          typ' = [ SigD
                     (mkName exportedName)
                     (AppT
                       (AppT
                         ArrowT
                         (argTypesToTuple paramTypes)
                       )
                       returnType
                     )
                 ]

      debug typ'
      debug $ pprint typ'
      return []

    _ -> error "deriveCallable: can only derive functions"


-- Example:
--
--   VarI
--     -- Name
--     FFI.Anything.f
--     -- Type
--     (AppT (AppT ArrowT (ConT GHC.Types.Int)) (AppT (AppT ArrowT (ConT GHC.Types.Double)) (ConT GHC.Base.String)))
--     -- Maybe Dec
--     Nothing
--     -- Fixity
--     (Fixity 9 InfixL)
--
-- Where the type "f :: Int -> Double -> String" is:
--
--   AppT
--     (AppT ArrowT (ConT GHC.Types.Int))
--     (AppT
--       (AppT ArrowT (ConT GHC.Types.Double))
--       (ConT GHC.Base.String)
--     )
--
--
-- The target is: runQ f_hs :: (Int, Double) -> String
-- so e.g.:
--
--   runQ [d| f_hs :: (Int, Double) -> String; f_hs = f_hs |]
--
-- which is:
--
--   [ SigD  -- This is the type
--       f_hs
--       (AppT
--         (AppT
--           ArrowT
--           (AppT
--             (AppT (TupleT 2) (ConT GHC.Types.Int))
--             (ConT GHC.Types.Double)
--           )
--         )
--         (ConT GHC.Base.String)
--       )
--   , ValD  -- This is the unimportant `f_hs = f_hs` part needed for the quasiquoter to complile
--       (VarP f_hs_2)
--       (NormalB (VarE f_hs_2))
--       []
--   ]

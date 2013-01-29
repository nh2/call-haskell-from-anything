module FFI.Python.TH where

import Language.Haskell.TH

import Debug.Trace


parameters :: Type -> [Name]
parameters t = case t of
  AppT t1 t2 -> parameters t1 ++ parameters t2
  ArrowT     -> []
  ConT name  -> [name]
  -- TODO handle ListT, TupleT and so on
  _          -> error $ "parameters: unhandled Type " ++ show t


deriveCallable :: Name -> Q [Dec]
deriveCallable funName = do
  info <- reify funName
  case info of
    VarI name typ mDec fixity -> do
      let nameString    = nameBase name
          signatureList = parameters typ
          paramTypes    = init signatureList
          returnType    = last signatureList

          name' = mkName (nameString ++ "_hs")

      trace (show signatureList) $ return ()
      return $ [ SigD
                   name'
                   (AppT
                     (AppT
                       ArrowT
                       (AppT
                         (AppT (TupleT 2) (ConT (mkName "Int")))
                         (ConT (mkName "Double"))
                       )
                     )
                     (ConT (mkName "String"))
                   )
               ]
    x -> error "deriveCallable: can only derive functions"


-- Example:
--
--   VarI
--     -- Name
--     FFI.Python.f
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

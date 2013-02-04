{-# OPTIONS_GHC -cpp #-}

#if MIN_VERSION_base(4,6,0)
#define USE_DATA_KINDS 1
#endif

#if USE_DATA_KINDS
{-# LANGUAGE DataKinds, TypeOperators #-}
#endif
{-# LANGUAGE TypeOperators, ScopedTypeVariables, FlexibleContexts #-}

-- Oh man, CPP for comments. Getting fastidious ...
#if USE_DATA_KINDS
-- | For documentation, see the exported module "FFI.Anything.TypeUncurryDataKinds".
#else
-- | For documentation, see the exported module "FFI.Anything.TypeUncurryLegacy".
#endif
module FFI.Anything.TypeUncurry (
  module TypeUncurryImplementation
) where


-- For GHC 7.6 or newer, we import TypeUncurry which uses DataKinds for TypeList to be kind-safe.
-- For all other versions, import TypeUncurryLegacy which uses a simpler model of TypeList.
#if USE_DATA_KINDS
import FFI.Anything.TypeUncurry.DataKinds as TypeUncurryImplementation
#else
import FFI.Anything.TypeUncurry.Legacy as TypeUncurryImplementation
#endif

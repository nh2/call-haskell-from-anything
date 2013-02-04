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
-- | For documentation, see the exported module "FFI.Python.TypeUncurryDataKinds".
#else
-- | For documentation, see the exported module "FFI.Python.TypeUncurryLegacy".
#endif
module FFI.Python.TypeUncurry (
  module TypeUncurryImplementation
) where


-- For GHC 7.6 or newer, we import TypeUncurry which uses DataKinds for TypeList to be kind-safe.
-- For all other versions, import TypeUncurryLegacy which uses a simpler model of TypeList.
#if USE_DATA_KINDS
import FFI.Python.TypeUncurryDataKinds as TypeUncurryImplementation
#else
import FFI.Python.TypeUncurryLegacy as TypeUncurryImplementation
#endif

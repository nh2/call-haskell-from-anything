{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4,6,0)
#define USE_DATA_KINDS 1
#endif

#if USE_DATA_KINDS
{-# LANGUAGE DataKinds, TypeOperators #-}
#endif
{-# LANGUAGE TypeOperators, ScopedTypeVariables, FlexibleContexts #-}

-- | For GHC 7.6 or newer, we use (and re-export) "FFI.Anything.TypeUncurry.DataKinds" which uses DataKinds for TypeList to be kind-safe.
--
-- For all other versions, use (and re-export) "FFI.Anything.TypeUncurry.Legacy" which uses a simpler model of TypeList.
module FFI.Anything.TypeUncurry (
#if USE_DATA_KINDS
  -- | You see this because your compiler supports DataKinds.
  module FFI.Anything.TypeUncurry.DataKinds
#else
  -- | You see this because your compiler does not support DataKinds.
  module FFI.Anything.TypeUncurry.Legacy
#endif
) where


#if USE_DATA_KINDS
import FFI.Anything.TypeUncurry.DataKinds
#else
import FFI.Anything.TypeUncurry.Legacy
#endif

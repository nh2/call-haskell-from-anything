-- | Contains some helper functions.
module FFI.Python.Util (
  lazyToStrictBS
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Monoid


-- Could use Data.ByteString.Lazy.toStrict starting from bytestring 0.10.0.0
lazyToStrictBS :: BSL.ByteString -> ByteString
lazyToStrictBS = mconcat . BSL.toChunks

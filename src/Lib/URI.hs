module Lib.URI (relativeToM) where

import Lib.Base (maybeToM)
import Network.URI

relativeToM :: (MonadFail m) => String -> String -> m URI
relativeToM uri1 uri2 =
  maybeToM
    (unwords ["Cannot calculate uri", show uri1, "relative to", show uri2])
    (liftA2 relativeTo (parseURI uri1) (parseURI uri2))

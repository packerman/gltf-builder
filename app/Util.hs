module Util (module Util) where

import Core.Model
import Lib.Base64 (DataUrl (..))

dataUrlToImage :: DataUrl -> Image
dataUrlToImage (DataUrl {mimeType, getData}) =
  Image
    { name = Nothing,
      mimeType,
      imageData = getData
    }

module Lib.Validity (module Lib.Validity) where

import Data.Validity (Validation, prettyValidation)
import Lib.Base (maybeToLeft)

validationToLeft :: a -> Validation -> Either String a
validationToLeft val = maybeToLeft val . prettyValidation

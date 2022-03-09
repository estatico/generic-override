-- | The public, stable generic-override API.
module Data.Override
  ( Override(Override)
  , As
  , At
  , With
  ) where

import Data.Override.Internal (Override(Override), As, At, With)
import Data.Override.Instances ()

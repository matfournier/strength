module Stress where

import Prelude
import Data.Int(toNumber)
import Data.Array(range)

normalStressRange :: Number -> Number -> Int -> Array Number 
normalStressRange min max granularity =
  let rangeStep = (max - min) / toNumber(granularity)
      in map (\v -> min + (toNumber(v) * rangeStep)) $ range 1 granularity

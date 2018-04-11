module Degree where

import Prelude
import Math (Radians, pi)

newtype Deg = Deg Number

instance showDegree :: Show Deg where
  show (Deg d) = show d 

deg2rad :: Deg -> Radians
deg2rad (Deg d) = (d * pi / 180.0) 

rad2deg :: Radians -> Deg
rad2deg r = Deg (r * 180.0 / pi)

num2deg :: Number -> Deg
num2deg n = Deg n


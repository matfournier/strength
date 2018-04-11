module HoekBrown( HoekBrown
                , HoekBrownModel (..)
                , createHoekBrownModel
                , hbGenerateShearNormal) where

import Prelude
import Math (pow, sqrt, exp, e)
import Data.Tuple (Tuple(Tuple))

type HoekBrown = { name :: String 
                 , ucs :: Number
                 , gsi :: Number
                 , mi :: Number
                 , d :: Number
                 , ei :: Number }

newtype HoekBrownModel = HoekBrownModel { inputs :: HoekBrown
                                        , mb :: Number
                                        , s :: Number
                                        , a :: Number
                                        , ucs' :: Number
                                        , t' :: Number }

createHoekBrownModel :: HoekBrown -> HoekBrownModel
createHoekBrownModel hb =
  let mb_ = mb hb
      s_ = s hb 
      a_ = a hb
      ucs_ = ucs' hb s_ a_
      t_ = t' hb s_ mb_
  in
   HoekBrownModel{ inputs: hb
                 , mb: mb_
                 , s: s_
                 , a: a_
                 , ucs': ucs_
                 , t': t_
                  }

-- calculate the reduced value of the material constant mi 
mb :: HoekBrown -> Number
mb hb = hb.mi * exp ((hb.gsi - 100.0) / (28.0 - 14.0 * hb.d))

-- calculate s rock mass constant
s :: HoekBrown -> Number
s hb = exp ((hb.gsi - 100.0) / (9.0 - 3.0 * hb.d))

-- calculate a rock mass constant
a :: HoekBrown -> Number
a hb = 0.5 + (1.0 / 6.0) * (pow e (-hb.gsi / 15.0) ) - (pow e (-20.0 / 3.0))

-- calculate uniaxial compressive strength
ucs' :: HoekBrown -> Number -> Number -> Number
ucs' hb s a = hb.ucs * pow s a 

-- calculate tensile strength

t' :: HoekBrown -> Number -> Number -> Number
t' hb s mb = s * hb.ucs / mb

-- calculate sigma1' 
hbSigma1' :: HoekBrownModel -> Number -> Number
hbSigma1' (HoekBrownModel hb) sig3' =
      sig3' + hb.inputs.ucs * pow((hb.mb * sig3' / hb.inputs.ucs) + hb.s)(hb.a)

-- calculate d'sigma1' / d'sigma3'
ds1ds3 :: HoekBrownModel -> Number -> Number -> Number
ds1ds3 (HoekBrownModel hb) s1 s3 = 1.0 +
                                   (hb.a * hb.mb * pow ((hb.mb * s3)/(hb.ucs' + hb.s)) (hb.a - 1.0))

-- calculate  sigma normal'
hbNormal' :: Number -> Number -> Number -> Number
hbNormal' s1' s3' ds1ds3 = ((s1' + s3') / 2.0) + ((s1' - s3') / 2.0) * ((ds1ds3 - 1.0) / (ds1ds3 + 1.0))

-- calculate tau shear stress
hbTau :: Number -> Number -> Number -> Number
hbTau s1' s3' ds1ds3 = (s1' - s3') * sqrt(ds1ds3) / (ds1ds3 + 1.0)

-- calculate a shear normal stress pair for a given model and sigma 3 value

hbGenerateShearNormal :: HoekBrownModel -> Number -> Tuple Number Number 
hbGenerateShearNormal hb sig3' = let sig1' = hbSigma1' hb sig3'
                                     ds1ds3' = ds1ds3 hb sig1' sig3'
                                     normalStress = hbNormal' sig1' sig3' ds1ds3'
                                     shearStress = hbTau sig1' sig3' ds1ds3'
                                 in Tuple normalStress shearStress 

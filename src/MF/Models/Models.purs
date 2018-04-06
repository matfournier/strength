module Models where

import Prelude

import Data.Array (range)
import Data.Int (toNumber)
import Degree (Deg, deg2rad, rad2deg, num2deg)
import Math (tan, asin, pow, sqrt, exp, e)

-- | Generic aliasing

type UCS = Number -- Unconfined Compressive Strength 
type GSI = Number -- GS 
type ModelName = String

-- | Models
-- | In the form Paramters, the Model itself, and a Sum type of Models

newtype MohrColoumbModel = MohrColoumbModel {phi :: Deg, cohesion :: Number}

newtype FrictionalStrengthModel = FrictionalStrength {phi :: Deg}

newtype ShearNormalModel = ShearNormalModel { shearStress :: Array Number, normalStress :: Array Number }

instance showShearNormalModel :: Show ShearNormalModel where
  show (ShearNormalModel m) = "shear stress: " <> show m.shearStress <> "\n" 
    <> "normal stress: " <> show m.normalStress 

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
                                        , rockmassUCS :: Number
                                        , rockMassTensileStrength :: Number }

hoekBrownToMohrColoumb :: HoekBrownModel -> Number -> MohrColoumbModel
hoekBrownToMohrColoumb (HoekBrownModel hb) sig3max = MohrColoumbModel {phi: rad2deg p, cohesion: c}
  where
    sig3n = sig3max / hb.inputs.ucs
    p = asin (6.0 * hb.a * hb.mb * pow (hb.s + hb.mb * sig3n)(hb.a - 1.0) /
        ((2.0 * (1.0 + hb.a) * (2.0 + hb.a)) + (6.0 * hb.a * hb.mb * pow (hb.s + hb.mb * sig3n)(hb.a - 1.0))))
    c =  (hb.inputs.ucs * (((1.0 + 2.0 * hb.a) * hb.s) + ((1.0 - hb.a) * hb.mb * sig3n)) *
          pow (hb.s + hb.mb * sig3n)(hb.a - 1.0)) / (((1.0 + hb.a) * (2.0 + hb.a)) *
          sqrt(1.0 + (((6.0 * hb.a * hb.mb) * pow(hb.s + hb.mb * sig3n)(hb.a - 1.0))) /
               ((1.0 + hb.a) * (2.0 + hb.a))))


-- | Sum type of all models 
data Model = MC ModelName MohrColoumbModel
            | Frictional ModelName FrictionalStrengthModel
            | HB ModelName HoekBrownModel
            | SN ModelName ShearNormalModel 

-- | Prebuild static Leps (1970) soil models models

lepsPoor :: Model 
lepsPoor = SN "Leps - Poor" leps
  where
    leps :: ShearNormalModel
    leps = ShearNormalModel {
      shearStress: [3.4, 24.1, 41.4, 82.7, 275.8, 482.6, 689.5, 1103.20, 4826.30, 6894.80],
      normalStress: [4.4, 25.2, 40.9, 82.7, 224.1, 369.6, 508.3, 773.0, 2863.4, 3921.7]
      }


lepsAvg :: Model
lepsAvg = SN "Leps - Average" leps
  where
    leps :: ShearNormalModel
    leps = ShearNormalModel {
      shearStress: [3.4, 24.1, 41.4, 82.7, 275.8, 482.6, 689.5, 1103.20, 4826.30, 6894.80],
      normalStress: [5.4, 30.3, 49.1, 91.4, 269.3, 445.0, 612.8, 933.8, 3494.9, 4802.8]
      }

lepsGood :: Model
lepsGood = SN "Leps - Good" leps
  where
    leps :: ShearNormalModel
    leps = ShearNormalModel {
      shearStress: [3.4, 24.1, 41.4, 82.7, 275.8, 482.6, 689.5, 1103.20, 4826.30, 6894.80],
      normalStress: [6.5, 36.4, 58.9, 109.3, 321.1, 530.5, 730.6, 1114.1, 4189.7, 5763.8] 
      }

-- | Model Parameter Types

newtype ShearNormalParameters = ShearNormalParameters {min :: Number, max :: Number, granularity :: Int}


-- | Typeclasses 

-- | Convert from model (a) with range b c to a ShearNormalModel
-- |   so all models can be plotted as shear vs normal stress

class ShearNormal model settings  where
  shearnormal :: model -> settings -> ShearNormalModel

instance mcShearNormal :: ShearNormal MohrColoumbModel ShearNormalParameters where
  shearnormal (MohrColoumbModel mc) (ShearNormalParameters params) =
    ShearNormalModel {shearStress: ss, normalStress: ns} 
    where
      ss :: Array Number 
      ss = normalStressRange params.min params.max params.granularity
      ns :: Array Number 
      ns = map (\s -> tan(deg2rad(mc.phi)) + mc.cohesion) ss  


-- | todo move to util functions
normalStressRange :: Number -> Number -> Int -> Array Number 
normalStressRange min max granularity =
  let rangeStep =(max - min) / toNumber(granularity)
      in map (\v -> min + (toNumber(v) * rangeStep)) $ range 1 granularity

-- instance hbShearNormal :: ShearNormal HoekBrownModel ShearNormalParameters where
--   shearnormal (HoekBrownModel hb) (ShearNormalParameters params) = ShearNormalModel {shearStress: ss, nomralStress: ns}
--     where
--       tensileRange :: Array Number
--       tensileRange = normalStressRange (-hb.rockMassTensileStrength) 0 30
--       s3range = normalStressRange params.min params.max params.granularity
--       s1 = map (\s3 -> s3 + hb.ucs * pow((hb.mb * s3 / hb.inputs.ucs) + hb.s)(hb.a)) s3range 
--       ds1s3 = map (\s3 -> 1 + (hb.a * hb.mb * pow((hb.mb * hb * s3 / hb.inputs.ucs) + hb.s)(hb.a - 1.0))) s3range
--      -- I need a zipWith3 here but it's not defined on arrays?


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

-- calculate sneile strength

t' :: HoekBrown -> Number -> Number -> Number
t' hb s mb = s * hb.ucs / mb


-- calculated sigma1' 
hbSigma1' :: HoekBrownModel -> Number -> Number
hbSigma1' (HoekBrownModel hb) sig3' =
      sig3' + hb.inputs.ucs * pow((hb.mb * sig3' / hb.inputs.ucs) + hb.s)(hb.a)


-- need to rethink this one, it doesn't have all the inputs? where do I get rockmass compressive strength
-- from? 
-- calculate d'sigma1' / d'sigma3'
-- ds1ds3 :: HoekBrownModel -> Number -> Number -> Number
-- ds1ds3 hb s1 s3 = 1 + (hb.a * hb.mb * pow ((hb.mb * s3)/(hb.)))
    

module Models where

import Prelude
import Stress

import Data.Array (range, unzip)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..), fst, snd)
import Degree (Deg, deg2rad, rad2deg)
import HoekBrown (HoekBrown(..), HoekBrownModel(..), hbGenerateShearNormal)
import Math (tan, asin, pow, sqrt, exp, e)

-- | Generic aliasing

type ModelName = String

-- | Models
-- | In the form Paramters, the Model itself, and a Sum type of Models

newtype MohrColoumbModel = MohrColoumbModel {phi :: Deg, cohesion :: Number}

newtype FrictionalStrengthModel = FrictionalStrength {phi :: Deg}

newtype ShearNormalModel = ShearNormalModel { normalStress :: Array Number, shearStress :: Array Number }

instance showShearNormalModel :: Show ShearNormalModel where
  show (ShearNormalModel m) = "normal stress: " <> show m.normalStress <> "\n" 
    <> "shear stress: " <> show m.shearStress 


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
      normalStress: [3.4, 24.1, 41.4, 82.7, 275.8, 482.6, 689.5, 1103.20, 4826.30, 6894.80],
      shearStress: [4.4, 25.2, 40.9, 82.7, 224.1, 369.6, 508.3, 773.0, 2863.4, 3921.7]
      }


lepsAvg :: Model
lepsAvg = SN "Leps - Average" leps
  where
    leps :: ShearNormalModel
    leps = ShearNormalModel {
      normalStress: [3.4, 24.1, 41.4, 82.7, 275.8, 482.6, 689.5, 1103.20, 4826.30, 6894.80],
      shearStress: [5.4, 30.3, 49.1, 91.4, 269.3, 445.0, 612.8, 933.8, 3494.9, 4802.8]
      }

lepsGood :: Model
lepsGood = SN "Leps - Good" leps
  where
    leps :: ShearNormalModel
    leps = ShearNormalModel {
      normalStress: [3.4, 24.1, 41.4, 82.7, 275.8, 482.6, 689.5, 1103.20, 4826.30, 6894.80],
      shearStress: [6.5, 36.4, 58.9, 109.3, 321.1, 530.5, 730.6, 1114.1, 4189.7, 5763.8] 
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
    ShearNormalModel {normalStress: ns, shearStress: ss} 
    where
      ns :: Array Number 
      ns = normalStressRange params.min params.max params.granularity
      ss :: Array Number 
      ss = ns # map (\s -> tan(deg2rad(mc.phi)) + mc.cohesion)   



-- | todo add in the tensile strength (-ve normal stress) rang 
instance hbShearNormal :: ShearNormal HoekBrownModel ShearNormalParameters where
  shearnormal hb (ShearNormalParameters params) = ShearNormalModel {normalStress: fst stresses,
                                                                    shearStress: snd stresses}
    where
      s3range :: Array Number 
      s3range = normalStressRange params.min params.max params.granularity
      stresses :: Tuple (Array Number) (Array Number)
      stresses = s3range # map (\s3 -> hbGenerateShearNormal hb s3) # unzip 



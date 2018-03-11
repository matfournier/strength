module Models where

import Prelude
import Data.Array (range)
import Data.Int (toNumber)
import Math (tan)
import Degree (Deg, deg2rad) 

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

-- | Sum type of all models 
data Model = MC ModelName MohrColoumbModel
            | Frictional ModelName FrictionalStrengthModel
            | HB ModelName HoekBrownModel
            | SN ModelName ShearNormalModel 

-- | Prebuild models

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
    
-- | Typeclasses 

-- | Convert from model (a) with range b c to a ShearNormalModel
-- |   so all models can be plotted as shear vs normal stress

class ShearNormal a b c where
  shearnormal :: a -> b -> c -> ShearNormalModel

instance mcShearNormal :: ShearNormal MohrColoumbModel Number Number where
  shearnormal (MohrColoumbModel mc) min max = ShearNormalModel {shearStress: ss, normalStress: ns} 
    where
      ss :: Array Number 
      ss = normalStressRange min max 40
      ns :: Array Number 
      ns = map (\s -> tan(deg2rad(mc.phi)) + mc.cohesion) ss  


-- | todo move to util functions
normalStressRange :: Number -> Number -> Int -> Array Number 
normalStressRange min max granularity =
  let rangeStep =(max - min) / toNumber(granularity)
      in map (\v -> min + (toNumber(v) * rangeStep)) $ range 1 granularity

module MF.Strength where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head)
import Data.List as S
import Data.Maybe (Maybe)
import Math (sqrt)



type Angle = Number
type UCS = Number
type GSI = Number
type ModelName = String

type HoekBrown = { name :: String 
                 , ucs :: Number
                 , gsi :: Number
                 , mi :: Number
                 , d :: Number
                 , ei :: Number }


newtype MohrColoumbModel = MohrColoumbModel {phi :: Angle, cohesion :: Number}

newtype FrictionalStrengthModel = FrictionalStrength {phi :: Angle}

newtype ShearNormalModel = ShearNormalModel { shearStress :: Array Number, normalStress :: Array Number }

newtype HoekBrownModel = HoekBrownModel { inputs :: HoekBrown
                                        , mb :: Number
                                        , s :: Number
                                        , a :: Number
                                        , rockmassUCS :: Number
                                        , rockMassTensileStrength :: Number }

data Model = MC ModelName MohrColoumbModel
            | Frictional ModelName FrictionalStrengthModel
            | HB ModelName HoekBrownModel
            | ShearNormal ModelName ShearNormalModel 


lepsFine :: Model 
lepsFine = ShearNormal "lepsFine" leps
  where
    leps :: ShearNormalModel
    leps = ShearNormalModel {shearStress: [1.0,2.0,3.0], normalStress: [1.0,2.0,3.0]}
    

-- examples of making/showing these 

blah :: MohrColoumbModel 
blah = MohrColoumbModel {phi: 10.0, cohesion: 1000.0}

showMC :: MohrColoumbModel -> String
showMC (MohrColoumbModel mc) = show mc.phi <> " " <> show mc.cohesion

showSN :: ShearNormalModel -> String
showSN (ShearNormalModel sn) = show sn.shearStress <> " " <> show sn.normalStress 

blahModel :: Model
blahModel = MC "testMC" blah 

showBlahModel :: Model -> String 
showBlahModel (MC name mc) = name <> " " <> showMC mc
showBlahModel (ShearNormal name shear) = name <> " " <> showSN shear
showBlahModel _ = "not here yet!"


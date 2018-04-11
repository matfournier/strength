module MF.Strength where

import Prelude

import Data.Array (range)
import Data.Int (toNumber)
import Math (sqrt, pi, sin, tan, Radians)
import Degree (Deg, deg2rad, num2deg)
import Models


-- examples of making/showing these 

exMC :: MohrColoumbModel 
exMC = MohrColoumbModel {phi: num2deg 10.0 :: Deg, cohesion: 1000.0}

showMC :: MohrColoumbModel -> String
showMC (MohrColoumbModel mc) = show mc.phi <> " " <> show mc.cohesion

showSN :: ShearNormalModel -> String
showSN (ShearNormalModel sn) = show sn.normalStress <> " " <> show sn.shearStress 

exMCModel :: Model
exMCModel = MC "testMC" exMC 

showBlahModel :: Model -> String 
showBlahModel (MC name mc) = name <> " " <> showMC mc
showBlahModel (SN name shear) = name <> " " <> showSN shear
showBlahModel _ = "not here yet!"


-- need to update w/ the new params methods 
-- generateAxis :: Model -> ShearNormalModel
-- generateAxis (MC _ mc) = shearnormal mc ShearNormalParameters {min: 0.0, max: 1000.0, granularity: 40}
-- generateAxis _ =  ShearNormalModel {shearStress: [1.0,2.0], normalStress:[1.0, 2.0]} -- todo generate the rest here

module Animation.State
( State(..)
, posX
, posY
, velX
, velY
, dirX
, dirY
, velIncX
, velIncY
, maxVelX
, maxVelY
, initState
)
where

import Graphics.Gloss

type XY = (Float, Float) -- private type
data State
    = State
    { ballID :: Int
    , pos :: XY
    , vel :: XY
    , dir :: XY
    , velInc :: XY
    , maxVel :: XY
    , ballColor :: Color
    }

posX :: State -> Float
posX = fst . pos

posY :: State -> Float
posY = snd . pos

velX :: State -> Float
velX = fst . vel

velY :: State -> Float
velY = snd . vel

dirX :: State -> Float
dirX = fst . dir

dirY :: State -> Float
dirY = snd . dir

velIncX :: State -> Float
velIncX = fst . velInc

velIncY :: State -> Float
velIncY = snd . velInc

maxVelX :: State -> Float
maxVelX = fst . maxVel

maxVelY :: State -> Float
maxVelY = snd . maxVel

type Pos = XY
type Vel = XY
type Dir = XY
type VelInc = XY
type MaxVel = XY
initState :: (Int, Pos, Vel, Dir, VelInc, MaxVel, Color) -> State
initState (ballID, pos, vel, dir, vInc, maxV, color) =
    State ballID pos vel dir vInc maxV color

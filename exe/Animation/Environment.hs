module Animation.Environment where

data Environment
    = Environment
    { framePerSecond :: Int
    , frameDimension :: (Int, Int)
    , ballRadius :: Float
    , ballStartV :: Float
    , ballCharge :: Float
    , maxBallVel :: Float
    , ballCounts :: Int
    , saveLogTxt :: Bool
    , logTxtPath :: String
    }

getDefaultEnv :: Environment
getDefaultEnv = Environment
    { framePerSecond = 360
    , frameDimension = (1500, 750)
    , ballRadius = 5
    , ballStartV = 5
    , ballCharge = 50
    , maxBallVel = 500
    , ballCounts = 500
    , saveLogTxt = False
    , logTxtPath = "./log.txt"
    }

frameW :: (Num n) => Environment -> n
frameW = fromIntegral . fst . frameDimension

frameH :: (Num n) => Environment -> n
frameH = fromIntegral . snd . frameDimension

wBound :: Environment -> Float
wBound = (/2) . frameW

hBound :: Environment -> Float
hBound = (/2) . frameH

xBound :: Environment -> Float
xBound env = wBound env - ballRadius env

yBound :: Environment -> Float
yBound env = hBound env - ballRadius env

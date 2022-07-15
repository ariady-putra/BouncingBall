module Animation.Init
( initBalls
, initEnv
)
where

import Animation.Environment
import Animation.Input
import qualified Animation.State as Ball

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Reader

import Data.List

import Graphics.Gloss

import System.Directory
import System.Exit
import System.Random

initBalls :: ReaderT Environment IO [Ball.State]
initBalls = do -- need IO due to randomRIO
    env <- ask
    let c = ballCounts env
        x = xBound env
        y = yBound env
    pX <- replicateM c . randomRIO $ (-x, x)
    pY <- replicateM c . randomRIO $ (-y, y)
    let pos = zip pX pY
    
    let v   = ballStartV env -- flexibility to have individual
        vel = repeat (v, v)  -- init.vel in the future
    
    dX <- replicateM c $ signum <$> randomRIO (-1, 1)
    dY <- replicateM c $ signum <$> randomRIO (-1, 1)
    let dir = zip dX dY
    
    let charge = ballCharge env -- flexibility to have individual
        vInc   = repeat (charge, charge) -- charges in the future
    
    let maxVel = maxBallVel env -- flexibility to have individual
        maxV   = repeat (maxVel, maxVel) -- max.vel in the future
    
    let colors = cycle [red, green, blue, cyan, magenta, yellow]
    
    return . map Ball.initState $ zip7 [1..c] pos vel dir vInc maxV colors

initEnv :: [String] -> IO Environment
initEnv cliArgs@[_, _, _, _, _, _, _, _, _] =
    tryProcessArgs cliArgs
    `catch`
    showException
initEnv [] = do
    w <- getFrameW -- max ball radius would be 1/10th of width or height
    h <- getFrameH -- whichever one is lesser
    r <- getBallSize $ min (w `div` 10) (h `div` 10)
    v <- getBallInitVel
    m <- getMaxVelocity
    c <- getWallCharges
    l <- getFileLogging
    
    let env = getDefaultEnv
    let filePath = logTxtPath env
    
    let wndwSize = w * h
    let ballSize = ceiling $ 4 * r * r
    logFileExist <- doesFileExist $ filePath
    unless logFileExist . writeFile filePath $ "" -- mark app run
    b <- getBallCount (wndwSize `div` ballSize) (not logFileExist)
    
    return env
        { frameDimension = (w, h)
        , ballRadius = r
        , ballStartV = v
        , ballCharge = c
        , maxBallVel = m
        , saveLogTxt = l
        , ballCounts = b
        }
initEnv _ = showHelp "user error (Invalid number of arguments)"

-- private func
tryProcessArgs :: [String] -> IO Environment
tryProcessArgs [f, w, h, r, v, m, c, l, b] = do
    fps         <- readIO f
    frameW      <- readIO w
    frameH      <- readIO h
    ballSize    <- readIO r
    ballInitVel <- readIO v
    maxVelocity <- readIO m
    wallCharges <- readIO c
    ballCount   <- readIO b
    return getDefaultEnv
        { framePerSecond = fps
        , frameDimension = (frameW, frameH)
        , ballRadius = ballSize
        , ballStartV = ballInitVel
        , ballCharge = maxVelocity
        , maxBallVel = wallCharges
        , saveLogTxt = (l == "Y" || l == "y")
        , ballCounts = ballCount
        }
    

-- private func
showHelp :: String -> IO x
showHelp x = do
    putStrLn "Unable to process arguments!"
    putStrLn ""
    putStrLn "Valid arguments are:"
    putStrLn "f - frame per second"
    putStrLn "w - frame width"
    putStrLn "h - frame height"
    putStrLn "r - ball radius"
    putStrLn "v - initial velocity"
    putStrLn "m - maximum velocity"
    putStrLn "c - wall charges"
    putStrLn "l - file logging [Y / N]"
    putStrLn "b - ball count"
    putStrLn ""
    putStrLn "Example:"
    putStrLn "./BouncingBall 60 500 400 3 2 1000 90 N 800"
    putStrLn ""
    die x

-- private func
showException :: IOError -> IO x
showException = showHelp . show

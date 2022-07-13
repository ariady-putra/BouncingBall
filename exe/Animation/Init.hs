module Animation.Init
( initBalls
, initEnv
)
where

import Animation.Environment
import Animation.Input
import qualified Animation.State as Ball

import Control.Exception
import Control.Monad.Trans.Reader

import Data.List

import System.Directory
import System.Exit
import System.Random

initBalls :: ReaderT Environment IO [Ball.State]
initBalls = do -- need IO due to randomRIO
    env <- ask
    let balls = [1 .. ballCounts env]
    let ballR = ballRadius env
    
    let xRange = (ballR - wBound env, wBound env - ballR)
    let yRange = (ballR - hBound env, hBound env - ballR)
    pX <- mapM (\ _ -> randomRIO xRange) balls
    pY <- mapM (\ _ -> randomRIO yRange) balls
    let pos = zip pX pY
    
    let v = ballStartV env -- flexibility to have individual
    let vel = repeat (v,v) -- init.vel in the future
    
    dX <- mapM (\ _ -> signum <$> randomRIO (-1, 1)) balls
    dY <- mapM (\ _ -> signum <$> randomRIO (-1, 1)) balls
    let dir = zip dX dY
    
    let charge = ballCharge env -- flexibility to have individual
    let vInc = repeat (charge,charge) -- charge in the future
    
    let maxVel = maxBallVel env -- flexibility to have individual
    let maxV = repeat (maxVel,maxVel) -- max.vel in the future
    
    let color = map Ball.intToColor balls -- rotate colors
    
    let z = zipWith7 (,,,,,,) balls pos vel dir vInc maxV color
    return [Ball.initState b | b <- z]

initEnv :: [String] -> IO Environment
initEnv cliArgs@[_, _, _, _, _, _, _, _, _] =
    processCliArgs cliArgs
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
    writeFile filePath "" -- to mark that this app has been run
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
processCliArgs :: [String] -> IO Environment
processCliArgs [f, w, h, r, v, m, c, l, b] = do
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

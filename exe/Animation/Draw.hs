{-# LANGUAGE BlockArguments #-}

module Animation.Draw
( window
, drawBalls
, stepBalls
)
where

import Animation.Environment
import qualified Animation.State as Ball

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Writer.Lazy

import qualified Data.Text as Dtxt
import Data.Time

import Graphics.Gloss

import Text.Printf

type PosVelDir  = (Float, Float, Float)

type ReaderIO   = ReaderT Environment IO
type StateRIO   = StateT Ball.State ReaderIO
type WriterSRIO = WriterT Dtxt.Text StateRIO

window :: Reader Environment Display
window = do
    env <- ask
    let d = frameDimension env
        c = ballCounts env
        b = case c of
            1 -> "BouncingBall"
            _ -> printf "Bouncing %d Balls" c
    return $ InWindow b d (0, 0)

drawBalls :: [Ball.State] -> ReaderIO Picture -- used by simulateIO,
drawBalls balls = do                          -- must return IO Picture
    env <- ask
    let w = frameW env
        h = frameH env
    let bg = Color white $ rectangleSolid w h -- anticipate window resize
        bs = map toPicture balls where toPicture = flip runReader env . draw
    return . pictures $ bg : bs

stepBalls :: Float -> [Ball.State] -> ReaderIO [Ball.State]
stepBalls = mapM . execStateT . step

-- private func
step :: Float -> StateRIO Ball.State
step sec = do
    currState <- get
    let pvdX = (Ball.posX currState, Ball.velX currState, Ball.dirX currState)
        pvdY = (Ball.posY currState, Ball.velY currState, Ball.dirY currState)
    let velX = (Ball.velIncX currState, Ball.maxVelX currState)
        velY = (Ball.velIncY currState, Ball.maxVelY currState)
    
    -- get max coordinate
    env <- lift ask
    let maxX = xBound env
        maxY = yBound env
    
    -- step pos, vel, dir
    (nextX, xLog) <- runWriterT $ stepPVD pvdX velX maxX sec
    (nextY, yLog) <- runWriterT $ stepPVD pvdY velY maxY sec
    
    -- update state
    let (pX, vX, dX) = nextX
        (pY, vY, dY) = nextY
    let nextState = currState
            { Ball.pos = (pX, pY)
            , Ball.vel = (vX, vY)
            , Ball.dir = (dX, dY)
            }
    put nextState
    
    -- log debug messages
    logDbgMsg xLog
    logDbgMsg yLog
    
    return nextState

-- private func
stepPVD :: PosVelDir -> (Float, Float) -> Float -> Float -> WriterSRIO PosVelDir
stepPVD (pos, vel, dir) (vInc, maxV) bound sec = do
    let pos' = pos + vel * dir * sec -- calc next pos,
    if pos' < -bound || pos' > bound -- if it's going to be out of bound
    then do                          -- then recalculate pos:
        let d = -dir -- change direction
            v = min maxV $ vInc + vel
            p = pos + v * d * sec
        ball <- lift . lift $ length . show . ballCounts <$> ask
        time <- liftIO $ show <$> getCurrentTime
        tell . Dtxt.pack $ printf "[Ball#%%0%dd] %s" ball time
        return (p, v, d)
    else
        return (pos', vel, dir)
    

-- private func
logDbgMsg :: Dtxt.Text -> StateRIO ()
logDbgMsg dbgTxt = unless (Dtxt.null dbgTxt) do
    b <- get
    let ball = Ball.ballID b
    let pvdX = (Ball.posX b, Ball.velX b, Ball.dirX b)
        pvdY = (Ball.posY b, Ball.velY b, Ball.dirY b)
    let dbgStr = printf (Dtxt.unpack dbgTxt) ball
            ++ " X:" ++ show pvdX
            ++ " Y:" ++ show pvdY
            ++ "\n"
    liftIO . putStr $ dbgStr
    
    env <- lift ask
    when (saveLogTxt env) . liftIO . appendFile (logTxtPath env) $ dbgStr

-- private func
draw :: Ball.State -> Reader Environment Picture
draw ball = do
    let (x, y) = Ball.pos ball
    
    env <- ask
    let r = ballRadius env
        c = Ball.ballColor ball
    return . pictures $
        [ translate x y . Color c $ circleSolid r
        , translate x y $ circle r
        ]
    

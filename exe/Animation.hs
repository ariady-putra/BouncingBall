module Main where

import Animation.Draw
import Animation.Init
import Animation.Environment

import Control.Monad.Trans.Reader

import Graphics.Gloss.Interface.IO.Simulate

import System.Environment
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- auto-flush all putStr
    
    env <- initEnv =<< getArgs
    let fps = framePerSecond env
    
    initialState <- runReaderT initBalls env
    let wndw = runReader window env
        draw = flip runReaderT env . drawBalls
        step = (\ _ sec balls -> runReaderT (stepBalls sec balls) env)
    simulateIO wndw black fps initialState draw step

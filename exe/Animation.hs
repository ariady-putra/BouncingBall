module Main where

import Animation.Draw
import Animation.Environment
import Animation.Init

import Control.Monad.Trans.Reader

import Graphics.Gloss.Interface.IO.Simulate

import System.Environment
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- auto-flush all putStr
    
    env <- initEnv =<< getArgs
    let fps = framePerSecond env
    
    let fRunReader  = flip runReader  env
        fRunReaderT = flip runReaderT env
    initialState <- fRunReaderT initBalls
    let wndw = fRunReader window
        draw = fRunReaderT . drawBalls
        step _ sec = fRunReaderT . stepBalls sec
    simulateIO wndw black fps initialState draw step

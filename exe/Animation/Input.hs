{-# LANGUAGE BlockArguments #-}

module Animation.Input
( getFrameW
, getFrameH
, getBallSize
, getBallInitVel
, getMaxVelocity
, getWallCharges
, getFileLogging
, getBallCount
)
where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Data.Char

getFrameW :: IO Int -- initial window width
getFrameW = runReaderT getWH ("Width [min.250, default 1500]: ", 1500)

getFrameH :: IO Int -- initial window height
getFrameH = runReaderT getWH ("Height [min.250, default 750]: ", 750)

getBallSize :: Int -> IO Float -- ball radius
getBallSize = runReaderT getSize

getBallInitVel :: IO Float -- initial velocity
getBallInitVel = runReaderT getSpeed ("Initial speed [default 5]: ", 5)

getMaxVelocity :: IO Float -- maximum velocity
getMaxVelocity = runReaderT getSpeed ("Max. speed [default 500]: ", 500)

getWallCharges :: IO Float -- velocity increment
getWallCharges = runReaderT getSpeed ("Wall charge [default 50]: ", 50)

getFileLogging :: IO Bool -- enable file logging
getFileLogging = do
    putStr "Enable logging? [Y / N, default N]: "
    input <- getLine
    if null input -- if null then print and return default value
    then putStrLn "N" >> return False
    else if input == "y" || input == "Y"
    then return True
    else if input == "n" || input == "N"
    then return False
    else putStr "Only Y or N. " >> getFileLogging

getBallCount :: Int -> Bool -> IO Int
getBallCount = curry $ runReaderT getCount

-- private func
getWH :: ReaderT (String, Int) IO Int
getWH = do
    (label, defaultValue) <- ask
    liftIO . putStr $ label
    input <- liftIO getLine
    if null input -- if null then print and return default value
    then (liftIO . print $ defaultValue) >> return defaultValue
    else case isInputAllDigit input of
        Left no -> (liftIO . putStr $ no) >> getWH
        Right i -> if i < 250
            then (liftIO . putStr $ "Min.250, ") >> getWH
            else return i
        
    

-- private func
getSize :: ReaderT Int IO Float -- ball radius
getSize = do
    maxRadius <- ask
    liftIO . putStr $
        "Ball radius [between 1.." ++ show maxRadius ++ ", default 5]: "
    input <- liftIO getLine
    if null input -- if null then print and return default value
    then (liftIO . print $ 5) >> return 5
    else case isInputAllDigit input >>= isInputBetween (1, maxRadius) of
        Left error -> (liftIO . putStr $ error) >> getSize
        Right size -> return size
    

-- private func
getSpeed :: ReaderT (String, Float) IO Float
getSpeed = do 
    (label, defaultValue) <- ask
    liftIO . putStr $ label
    input <- liftIO getLine
    if null input -- if null then print and return default value
    then (liftIO . print . truncate $ defaultValue) >> return defaultValue
    else case isInputAllDigit input of
        Left reject -> (liftIO . putStr $ reject) >> getSpeed
        Right speed -> return . fromIntegral $ speed
    

-- private func
getCount :: ReaderT (Int, Bool) IO Int
getCount = do
    (maxCount, firstRun) <- ask
    let defC = min maxCount 500 -- default value cannot be greater than max value
    let maxC = show maxCount
    
    when firstRun do
        liftIO . putStr $ "Surprise! "
        liftIO . threadDelay $ 1000000
        liftIO . putStr $ "One last thing... "
        liftIO . threadDelay $ 1000000
    liftIO . putStr $
        "How many balls [max." ++ maxC ++ ", default " ++ show defC ++ "]: "
    input <- liftIO getLine
    if null input -- if null then print and return default value
    then (liftIO . print $ defC) >> return defC
    else case isInputAllDigit input >>= isInputBetween (1, maxCount) of
        Left reject -> (liftIO . putStr $ reject) >> getCount
        Right count -> return count
    

-- private func
isInputAllDigit :: String -> Either String Int
isInputAllDigit input = if all isDigit input
    then Right . read $ input
    else Left "Input must be a non-negative number. "

-- private func
isInputBetween :: (Num n) => (Int, Int) -> Int -> Either String n
isInputBetween (from, to) input = if input >= from && input <= to
    then Right . fromIntegral $ input
    else Left $ "Between " ++ show from ++ ".." ++ show to ++ ", "

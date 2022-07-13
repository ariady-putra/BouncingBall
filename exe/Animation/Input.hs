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

import Data.Char

getFrameW :: IO Int -- initial window width
getFrameW = getWH "Width [min.250, default 1500]: " 1500

getFrameH :: IO Int -- initial window height
getFrameH = getWH "Height [min.250, default 750]: " 750

getBallSize :: Int -> IO Float -- ball radius
getBallSize maxRadius = do
    putStr $ "Ball radius [between 1.." ++ show maxRadius ++ ", default 5]: "
    input <- getLine
    if null input -- if null then print and return default value
    then print 5 >> return 5
    else case isInputAllDigit input >>= isInputBetween (1, maxRadius) of
        Left error -> putStr error >> getBallSize maxRadius
        Right size -> return size
    

getBallInitVel :: IO Float -- initial velocity
getBallInitVel = getSpeed "Initial speed [default 5]: " 5

getMaxVelocity :: IO Float -- maximum velocity
getMaxVelocity = getSpeed "Max. speed [default 500]: " 500

getWallCharges :: IO Float -- velocity increment
getWallCharges = getSpeed "Wall charge [default 50]: " 50

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
getBallCount maxCount firstRun = do
    let defC = min maxCount 500 -- default value cannot be greater than max value
    let maxC = show maxCount
    
    when firstRun do
        putStr "Surprise! "
        threadDelay 1000000
        putStr "One last thing... "
        threadDelay 1000000
    putStr $ "How many balls [max." ++ maxC ++ ", default " ++ show defC ++ "]: "
    input <- getLine
    if null input -- if null then print and return default value
    then print defC >> return defC
    else case isInputAllDigit input >>= isInputBetween (1, maxCount) of
        Left reject -> putStr reject >> getBallCount maxCount False
        Right count -> return count
    

-- private func
getWH :: String -> Int -> IO Int
getWH label defaultValue = do
    putStr label
    input <- getLine
    if null input -- if null then print and return default value
    then print defaultValue >> return defaultValue
    else case isInputAllDigit input of
        Left no -> putStr no >> getWH label defaultValue
        Right i -> if i < 250
            then putStr "Min.250, " >> getWH label defaultValue
            else return i
        
    

-- private func
getSpeed :: String -> Float -> IO Float
getSpeed label defaultValue = do
    putStr label
    input <- getLine
    if null input -- if null then print and return default value
    then print (truncate defaultValue) >> return defaultValue
    else case isInputAllDigit input of
        Left reject -> putStr reject >> getSpeed label defaultValue
        Right speed -> return . fromIntegral $ speed
    

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

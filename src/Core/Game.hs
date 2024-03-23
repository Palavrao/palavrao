module Core.Game where

import Controllers.MatchesController
import Data.Time.Clock (getCurrentTime, NominalDiffTime, UTCTime, diffUTCTime)
import Interface.DrawBoard
import Utils.Validator
import Control.Concurrent (threadDelay)
import System.Console.ANSI
import Controllers.BoardController

fluxo :: Match -> String -> IO Bool

fluxo _ ":S" = do
                putStrLn "S"
                return True
fluxo _ ":?" = do
                putStrLn "?" 
                return True
fluxo _ ":!" = do
                putStrLn "!" 
                return True
fluxo _ ":*x" = do
                putStrLn "" 
                return True
fluxo match input = return (initialValidation match input)

gameLoop :: Match -> UTCTime -> IO ()
gameLoop match lastUpdate = do
    printBoard match
    input <- getLine
    v <- (fluxo match input)
    putStrLn $ show v
{-     if (initialValidation match input)
        then putStrLn "Okay"
        else putStrLn "False!!!!" 
        
        
        fluxo _ (':':t) = do
                    putStrLn "COMANDO"
                    return True-}

    currentTime <- getCurrentTime
    let elapsed = realToFrac (currentTime `diffUTCTime` lastUpdate) :: NominalDiffTime
        updatedTimer = mTimer match - realToFrac elapsed

    if updatedTimer <= 0
        then do
            putStrLn "Your turn is over!"
            let updatedMatch = updateMatchTimer match 300
            let updatedMatch' = toggleMatchTurn updatedMatch
            saveMatchJson updatedMatch
            gameLoop updatedMatch' currentTime
        else do
            let updatedMatch = updateMatchTimer match updatedTimer
            threadDelay 100000
            gameLoop updatedMatch currentTime
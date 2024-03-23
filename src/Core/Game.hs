module Core.Game where

import Controllers.MatchesController
import Data.Time.Clock (getCurrentTime, NominalDiffTime, UTCTime, diffUTCTime)
import Interface.DrawBoard
import Utils.Validator
import Control.Concurrent (threadDelay)
import System.Console.ANSI
import Controllers.BoardController

gameLoop :: Match -> UTCTime -> IO ()
gameLoop match lastUpdate = do
    clearScreen
    printBoard match
    input <- getLine
    if (initialValidation match input)
        then putStrLn "Okay"
        else putStrLn "False!!!!"

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
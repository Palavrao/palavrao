module Main where

import Utils as UT
import MatchesController
import BoardController
import DrawBoard
import System.Console.ANSI
import Menu
import DrawMenu
import AccountsController
import Control.Monad (forever)
import Data.Time.Clock (getCurrentTime, NominalDiffTime, UTCTime, diffUTCTime)
import Control.Concurrent (threadDelay)
import System.Console.ANSI
import MatchesController (updateMatchTimer, saveMatchJson, toggleMatchTurn)
import Validator

gameLoop :: Match -> UTCTime -> IO ()
gameLoop match lastUpdate = do
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

main :: IO ()
main = do
    clearScreen
    menuLoop beginGame
--    putStrLn (unlines [ "                 ┌─────────────────────────────┐",
--                        "                 │                             │",
--                        "                 │    Redimensione para que    │",
--                        "                 │  a linha caiba no terminal! │",
--                        "                 │                             │",
--                        "<-------------------------------------------------------------->",
--                        "                 │           > Enter           │",
--                        "                 │                             │",
--                        "                 └──────────────────────────── ┘"])
--
--    ctd <- getLine
    startPersistence

    startTime <- getCurrentTime

    acc1 <- createAcc "Fulano"
    acc2 <- createAcc "Sicrano"
    match <- createMatch "Match" acc1 acc2

    gameLoop match startTime

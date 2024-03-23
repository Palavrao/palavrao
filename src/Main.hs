module Main where

import Utils.Utils as UT
import Controllers.MatchesController
import Core.Menu
import Controllers.AccountsController
import Data.Time.Clock (getCurrentTime)
import System.Console.ANSI
import Interface.BoxesMenu
import Core.Game

main :: IO ()
main = do
    clearScreen
    
    menuLoop beginGame

    startPersistence

    startTime <- getCurrentTime

    acc1 <- createAcc "Fulano"
    acc2 <- createAcc "Sicrano"
    match <- createMatch "Match" acc1 acc2

    gameLoop match startTime

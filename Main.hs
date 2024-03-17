module Main where

import Utils
import AccountsController
import MatchesController

main :: IO ()
main = do
    startPersistence
    p1 <- createAcc "Fulano"
    p2 <- createAcc "Sicrano"
    match <- createMatch "Fulano x Sicrano" p1 p2
    print(matchName match)
    print(board match)
    print(p1Name match)
    print(p1Score match)
    print(p2Name match)
    print(p2Score match)
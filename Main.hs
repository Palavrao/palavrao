module Main where

import Utils
import AccountsController
import MatchesController

main :: IO ()
main = do
    startPersistence
    
    acc1 <- createAcc "Fulano"
    acc2 <- createAcc "Sicrano"
    match <- createMatch "Fulano x Sicrano" acc1 acc2
    print(matchName match)
    print(matchBoard match)
    print(matchP1 match)
    print(matchP2 match)
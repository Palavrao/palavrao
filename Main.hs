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

    fulanoExiste <- accExists "Fulano"
    if (fulanoExiste)
        then putStrLn "Fulano existe"
        else putStrLn "Fulano não existe"
    
    kaikeExiste <- accExists "Kaike"
    if (kaikeExiste)
        then putStrLn "Kaike existe"
        else putStrLn "Kaike não existe"

    fulanoXSicranoExiste <- matchExists "Fulano x Sicrano"
    if (fulanoXSicranoExiste)
        then putStrLn "Fulano x Sicrano existe"
        else putStrLn "Fulano x Sicrano não existe"
    
    fulanoXKaikeExiste <- matchExists "Fulano x Kaike"
    if (fulanoXKaikeExiste)
        then putStrLn "Fulano x Kaike existe"
        else putStrLn "Fulano x Kaike não existe"
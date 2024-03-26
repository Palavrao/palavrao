{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant return" #-}
module Core.Game where

import Controllers.MatchesController
import Data.Time.Clock (getCurrentTime, NominalDiffTime, UTCTime, diffUTCTime)
import Interface.DrawBoard
import Utils.Validator
import Control.Concurrent (threadDelay)
import System.Console.ANSI
import System.IO
import Controllers.BoardController
import Utils.Utils as UT
import Controllers.AccountsController
import Controllers.PlayerController
import Data.Char


valida :: Match -> [String] -> String -> IO (Match, String)
valida match _ ":C" = return (match, "") -- TODO
valida match _ ":!" = do
                        return (skipPlayerTurn match, ">> " ++ (map toUpper (accName (pAcc (_getPlayerOnTurn match)))) ++ " pulou o turno!\n") -- TODO
valida match wordlist ":?" = do
                        UT.manual
                        UT.__colorText ("Turno de: " ++ (map toUpper (accName (pAcc (_getPlayerOnTurn match))))) Blue
                        putStr "\nDigite sua palavra no formato X00 V/H PALAVRA:\n > "
                        hFlush stdout
                        i <- getLine
                        (m, msg)  <- (valida match wordlist i)
                        return (m, msg) -- TODO

valida match w (':':'*':[]) = do
                    UT.__colorText ("Escolha um caracter válido \n > ") Blue
                    c <- getLine
                    valida match w (":*" ++ c)

valida match w (':':'*':t) = 
                    let lttr = UT.getLetterObject (head t)
                    in case lttr of
                    Just letter -> do
                        switched <- switchPlayerLetter match letter
                        return (skipPlayerTurn switched, ((map toUpper (accName (pAcc (_getPlayerOnTurn match)))) ++ " trocou uma letra.\n")) -- TODO
                    Nothing -> do
                        UT.__colorText ("Escolha um caracter válido \n > ") Blue
                        c <- getLine
                        valida match w (":*" ++ c)
                        

valida match wordlist input 
    |(res && ((length listaDeRes) == 0)) = do
        let m = toggleMatchTurn (incPlayerScore (resetMatchSkipsQtd (updateMatchBoard match boardAtualizado)) points)
        return (m, ("Palavra válida! Pontos: " ++ (show points) ++ "\n"))

    |(res && ((length listaDeRes) /= 0)) = do
        UT.__colorText ("\nPalavras inválidas: " ++ (show listaDeRes) ++ " \nTente novamente: \n") Red
        putStrLn "Digite sua palavra no formato X00 V/H PALAVRA:\n > "
        hFlush stdout
        i <- getLine
        (m, msg)  <- (valida match wordlist i)
        return (m, msg)
    | otherwise = do
        UT.__colorText "\nCoordenada ou Formatação inválidas, tente novamente: \n" Red
        putStr "Digite sua palavra no formato X00 V/H PALAVRA:\n > "
        hFlush stdout
        i <- getLine
        (m, msg) <- (valida match wordlist i)
        return (m, msg)
    where 
        (res, listaDeRes, points) = (initialValidation match wordlist input)
        boardAtualizado = (updateBoard (placeWord (readWordInput input match) (mBoard match)))


gameLoop :: Match -> [String] -> UTCTime -> String -> IO (Match)
gameLoop match wordList lastUpdate lastMessage = do
    printBoard match
    UT.__colorText lastMessage Green
    UT.__colorText ("Turno de: " ++ (map toUpper (accName (pAcc (_getPlayerOnTurn match))))) Blue
    putStr "\nDigite sua palavra no formato X00 V/H PALAVRA:\n > "
    hFlush stdout
    input <- getLine
    (m, msg) <- valida match wordList input
    if mSkips m == 4 then do
        finishedMatch <- finishMatch m
        return finishedMatch
    else do
        currentTime <- getCurrentTime
        let elapsed = realToFrac (currentTime `diffUTCTime` lastUpdate) :: NominalDiffTime
            updatedTimer = mTimer match - realToFrac elapsed

        if updatedTimer <= 0 then do
            putStrLn "Your turn is over!"
            let updatedMatch = updateMatchTimer m 300
            let updatedMatch' = toggleMatchTurn updatedMatch
            updateMatchJson updatedMatch
            gameLoop updatedMatch' wordList currentTime ""
        else do
            let updatedMatch = updateMatchTimer m updatedTimer
            threadDelay 100000
            updateMatchJson updatedMatch
            gameLoop updatedMatch wordList currentTime msg
        
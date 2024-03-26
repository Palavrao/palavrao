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


valida :: Match -> [String] -> String -> IO (Match, String)
valida match _ ":C" = return (match, "") -- TODO
valida match _ ":!" = do
                        return (skipPlayerTurn match, ">> Pulou o turno") -- TODO
valida match wordlist ":?" = do
                        UT.manual
                        putStr "\nDigite sua palavra no formato X00 V/H PALAVRA:\n > "
                        hFlush stdout
                        i <- getLine
                        (m, msg)  <- (valida match wordlist i)
                        return (m, msg) -- TODO

valida match _ (':':'*':t) = do
                        putStrLn "TODO SWITCH LETTERS"
                        return (match, ("Trocou a letra " ++ t)) -- TODO

valida match wordlist input 
    |(res && ((length listaDeRes) == 0)) = do
        let m = (resetMatchSkipsQtd (updateMatchBoard match (updateBoard (placeWord (readWordInput input match) (mBoard match)))))
        return (m, ("Palavra válida! Pontos: " ++ (show points)))
    |(res && ((length listaDeRes) /= 0)) = do
        UT.__colorText ("\nPalavras inválidas: " ++ (show listaDeRes) ++ " \nTente novamente: \n") Red
        putStr "Digite sua palavra no formato X00 V/H PALAVRA:\n > "
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


gameLoop :: Match -> [String] -> UTCTime -> String -> IO ()
gameLoop match wordList lastUpdate lastMessage = do
    printBoard match
    UT.__colorText lastMessage Green
    putStr "\nDigite sua palavra no formato X00 V/H PALAVRA:\n > "
    hFlush stdout
    input <- getLine
    (m, msg) <- valida match wordList input
    if mSkips m == 4 then do
        finishMatch m
    else do    

        currentTime <- getCurrentTime
        let elapsed = realToFrac (currentTime `diffUTCTime` lastUpdate) :: NominalDiffTime
            updatedTimer = mTimer match - realToFrac elapsed

        if updatedTimer <= 0
            then do
                putStrLn "Your turn is over!"
                let updatedMatch = updateMatchTimer m 300
                let updatedMatch' = toggleMatchTurn updatedMatch
                saveMatchJson updatedMatch
                gameLoop updatedMatch' wordList currentTime ""
            else do
                let updatedMatch = updateMatchTimer m updatedTimer
                threadDelay 100000
                gameLoop updatedMatch wordList currentTime msg
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


valida :: Match -> [String] -> String -> IO Match
valida match _ ":C" = return (match) -- TODO
valida match _ ":!" = do
                        putStrLn ">> Pulou o turno"
                        return (match) -- TODO
valida match wordlist ":?" = do
                        UT.manual
                        putStr "\nDigite sua palavra no formato X00 V/H PALAVRA:\n > "
                        hFlush stdout
                        i <- getLine
                        m <- (valida match wordlist i)
                        return m -- TODO

valida match _ (':':'*':t) = do
                        putStrLn "TODO SWITCH LETTERS"
                        return (match) -- TODO

valida match wordlist input 
    |(res && ((length listaDeRes) == 0)) = return (updateMatchBoard match (placeWord (readWordInput input match) (mBoard match)))
    |(res && ((length listaDeRes) /= 0)) = do
        UT.__colorText ("\nPalavras inválidas: " ++ (show listaDeRes) ++ " \nTente novamente: \nDigite sua palavra no formato X00 V/H PALAVRA:\n > ") Red
        hFlush stdout
        i <- getLine
        m <- (valida match wordlist i)
        return m
    | otherwise = do
        UT.__colorText "\nCoordenada ou Formatação inválidas, tente novamente: \nDigite sua palavra no formato X00 V/H PALAVRA:\n > " Red
        hFlush stdout
        i <- getLine
        m <- (valida match wordlist i)
        return m
    where 
        (res, listaDeRes) = (initialValidation match wordlist input)


gameLoop :: Match -> [String] -> UTCTime -> IO ()
gameLoop match wordList lastUpdate = do
    printBoard match
    putStr "\nDigite sua palavra no formato X00 V/H PALAVRA:\n > "
    hFlush stdout
    input <- getLine
    m <- valida match wordList input

    currentTime <- getCurrentTime
    let elapsed = realToFrac (currentTime `diffUTCTime` lastUpdate) :: NominalDiffTime
        updatedTimer = mTimer match - realToFrac elapsed

    if updatedTimer <= 0
        then do
            putStrLn "Your turn is over!"
            let updatedMatch = updateMatchTimer m 300
            let updatedMatch' = toggleMatchTurn updatedMatch
            saveMatchJson updatedMatch
            gameLoop updatedMatch' wordList currentTime
        else do
            let updatedMatch = updateMatchTimer m updatedTimer
            threadDelay 100000
            gameLoop updatedMatch wordList currentTime
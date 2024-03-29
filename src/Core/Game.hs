{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Use list literal pattern" #-}
{-# HLINT ignore "Use null" #-}
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
import Controllers.LettersController


fluxHandler :: Match -> [String] -> String -> IO (Match, String)
-- CASOS ESPECIAIS : :C PAUSAR PARTIDA
fluxHandler match wl ":c" = fluxHandler match wl ":C"
fluxHandler match _ ":C" = return (match, (map toUpper (accName (pAcc (getPlayerOnTurn match)))) ++ " pausou o jogo!")

-- CASOS ESPECIAIS: :! PULAR TURNO
fluxHandler match _ ":!" = do
                        return (skipPlayerTurn match, ">> " ++ (map toUpper (accName (pAcc (getPlayerOnTurn match)))) ++ " pulou o turno!\n") 

-- CASOS ESPECIAIS: :! VER MANUAL
fluxHandler match wordlist ":?" = do
                        UT.manual
                        UT.__colorText ("Turno de: " ++ (map toUpper (accName (pAcc (getPlayerOnTurn match))))) Blue
                        putStr "\nDigite sua palavra no formato X00 V/H PALAVRA:\n > "
                        hFlush stdout
                        i <- getLine
                        (m, msg)  <- (fluxHandler match wordlist i)
                        return (m, msg) -- TODO
-- CASOS ESPECIAIS: :* trocar letra : sem letra
fluxHandler match w (':':'*':[]) = do
                    UT.__colorText ("Escolha um caracter válido \n > ") Blue
                    c <- getLine
                    fluxHandler match w (":*" ++ c)
-- CASOS ESPECIAIS: :* trocar letra : letras válidas e inválidas
fluxHandler match w (':':'*':t) = 
                    -- obtém o objeto letter a partir de um char
                    let lttr = UT.getLetterObject (toUpper (head t))
                    in case lttr of
                    Just letter -> do --se é um caracter válido
                        let hasLttr = playerHasLetter match letter
                        if hasLttr then do -- se o jogador tem o caracter
                            switched <- switchPlayerLetter match letter
                            return (skipPlayerTurn switched, ((map toUpper (accName (pAcc (getPlayerOnTurn match)))) ++ " trocou uma letra.\n"))
                        else do
                            UT.__colorText (" > Escolha uma letra válida \n > ") Blue
                            c <- getLine
                            fluxHandler match w (":*" ++ c)
                    Nothing -> do -- o caracter é inválido
                        UT.__colorText (" > Escolha uma letra válida \n > ") Blue
                        c <- getLine
                        fluxHandler match w (":*" ++ c)

-- CASO COMUM: player tenta colocar uma palavra no tabuleiro                      
fluxHandler match wordlist input 
    -- coordenadas válidas, palavras existentes e letras válidas
    |(res && ((length palavrasInvalidas) == 0) && (length letrasInvalidas) == 0) = do
        updatedPlayer <- updatePlayerLetters (removePlayerLetters (incPlayerScore (resetMatchSkipsQtd (updateMatchBoard match boardAtualizado)) points) letrasUsadas)
        let m = toggleMatchTurn updatedPlayer
        return (m, ("Palavra válida! Pontos: " ++ (show points) ++ "\n"))
    -- coordenadas válidas mas letras inválidas (player não tem as letras)
    |(res && ((length letrasInvalidas) /= 0)) = do
        UT.__colorText ("\nVocê não tem as letras: " ++ (show letrasInvalidas) ++ " \nTente novamente: \n") Red
        putStrLn "Digite sua palavra no formato X00 V/H PALAVRA:\n > "
        hFlush stdout
        i <- getLine
        (m, msg)  <- (fluxHandler match wordlist i)
        return (m, msg)
    --coordenadas válidas mas forma palavras que não existem
    |(res && ((length palavrasInvalidas) /= 0)) = do
        UT.__colorText ("\nPalavras inválidas: " ++ (show palavrasInvalidas) ++ " \nTente novamente: \n") Red
        putStrLn "Digite sua palavra no formato X00 V/H PALAVRA:\n > "
        hFlush stdout
        i <- getLine
        (m, msg) <- (fluxHandler match wordlist i)
        return (m, msg)
    --coordenadas inválidas
    | otherwise = do
        UT.__colorText "\nCoordenada ou Formatação inválidas, tente novamente: \n" Red
        putStr "Digite sua palavra no formato X00 V/H PALAVRA:\n > "
        hFlush stdout
        i <- getLine
        (m, msg) <- (fluxHandler match wordlist i)
        return (m, msg)
    where 
        (res, palavrasInvalidas, points, letrasInvalidas, letrasUsadas) = (initialValidation match wordlist input)
        boardAtualizado = (updateBoard (placeWord (readWordInput input match) (mBoard match)))

-- Loop principal do jogo: recebe input do jogador, valida o input e informa resultados sobre a tentativa
gameLoop :: Match -> [String] -> UTCTime -> String -> IO (Match)
gameLoop match wordList lastUpdate lastMessage = do
    -- Mostra a tela de transição e aguarda continuação
    clearScreen
    UT.__colorText lastMessage Green
    hFlush stdout
    UT.__colorText "> Enter para seguir para a visão do próximo jogador!\n\n" Blue
    hFlush stdout
    c <- getLine

    -- Mostra a tela de jogo 
    printBoard match
    UT.__colorText ("Turno de: " ++ (map toUpper (accName (pAcc (getPlayerOnTurn match))))) Blue
    putStr "\nDigite sua palavra no formato X00 V/H PALAVRA:\n > "
    hFlush stdout
    
    -- Verifica condição de parada: 4 saltos de turno/trocas de letra seguidos
    if mSkips match == 4 then do
        finishedMatch <- finishMatch match
        return finishedMatch
    else do
        -- Recebe o input do jogador e valida
        input <- getLine

        -- Jogador pausou a partida e saiu para o menu
        if input == ":C" || input == ":c" then do 
            UT.__colorText "\n\n >> Pausando e saindo do jogo...\n" Green
            UT.__colorText " > Aperte enter...\n\n" Blue
            c <- getLine
            return match
        -- Jogador jogou uma palavra ou outra ação especial
        else do 
            currentTime <- getCurrentTime
            let elapsed = realToFrac (currentTime `diffUTCTime` lastUpdate) :: NominalDiffTime
            let updatedTimer = mTimer match - realToFrac elapsed

            -- Se tiver acabado o tempo não registra a palavra jogada
            if updatedTimer <= 0 then do
                let updatedMatch = toggleMatchTurn match
                updateMatchJson updatedMatch
                gameLoop updatedMatch wordList currentTime "\nTempo de rodada excedido!\n"
            
            -- Se estiver dentro do tempo recebe a palavra ou comando e os processa
            else do
                (m, msg) <- fluxHandler match wordList input
                if getPlayerOnTurn match /= getPlayerOnTurn m then do
                    updateMatchJson m
                    gameLoop m wordList currentTime msg
                else do 
                    let updatedMatch = updateMatchTimer m updatedTimer
                    threadDelay 100000
                    updateMatchJson updatedMatch
                    gameLoop updatedMatch wordList currentTime msg
            
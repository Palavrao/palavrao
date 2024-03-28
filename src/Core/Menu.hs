{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use print" #-}
{-# HLINT ignore "Use forM_" #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Core.Menu where

import Text.Printf
import System.Console.ANSI
import System.Exit
import Data.Char
import Data.Aeson
import Interface.BoxesMenu
import System.IO
import Controllers.AccountsController
import Controllers.MatchesController
import Data.Time.Clock (getCurrentTime)
import Data.Maybe (fromMaybe)
import Utils.Utils as UT
import Utils.Validator as VL
import Core.Game
import Controllers.PlayerController (Player(pScore))

_drawMenu :: Menu -> IO ()
_drawMenu menu = do
    clearScreen
    mapM_ putStrLn (box menu)

menuLoop :: Menu -> IO ()
menuLoop menu = do
    _drawMenu menu
    userInput <- getLine
    updatedMenu <- _menuFlux menu userInput
    case updatedMenu of
        Just updatedMenu -> menuLoop updatedMenu
        Nothing -> return ()

_menuFlux :: Menu -> String -> IO (Maybe Menu)
_menuFlux menu input = do
    case action menu of
        StartMenu -> case input of
            "1" -> do
                if _accsFull menu then do
                    return (Just (updateMenu RegisterMatch menu))
                else do
                    return (Just (updateMenu NewGame menu))
            "2" -> return (Just (updateMenu ContinueGame menu))
            "3" ->
                if _accsFull menu then do
                    return (Just (updateMenu (action menu) menu))
                else do
                    return (Just (updateMenu Register menu))
            "4" -> return (Just (updateMenu Rules menu))
            "5" -> do
                updatedMenu <- _getRank menu
                return (Just updatedMenu)
            "6" -> exitSuccess >> return Nothing
            _   -> return (Just (updateMenu (action menu) menu))
        NewGame -> case input of
            "1" -> return (Just (updateMenu Register menu))
            "2" -> return (Just (updateMenu Login menu))
            "3" -> return (Just (updateMenu (boxBefore menu) menu))
            _   -> return (Just (updateMenu (action menu) menu))
        Login -> case input of
            "1" -> do
                updatedMenu <- _login menu
                return (Just updatedMenu)
            "2" -> return (Just (updateMenu (boxBefore menu) menu))
            _   -> return (Just (updateMenu (action menu) menu))
        Register -> case input of
            "1" -> do
                updatedMenu <- _createAcc menu
                return (Just updatedMenu)
            "2" -> return (Just (updateMenu (boxBefore menu) menu))
            _   -> return (Just (updateMenu (action menu) menu))
        ContinueGame -> case input of
            "1" -> do
                matchMenu <- _continueGame menu
                _loadMatch matchMenu
            "2" -> do
                let updatedMenu = updateMatchesMenu menu 1
                updatedMenu
            "3" -> return (Just (updateMenu (boxBefore menu) menu))
            _   -> return (Just (updateMenu (action menu) menu))
        Rules -> return (Just (updateMenu (boxBefore menu) menu))
        BeforeGame -> case input of
            "1" -> _loadMatch menu
            "2" -> do
                let updatedMenu = menu {p1 = p2 menu}
                let updatedMenu' = _updateAccs updatedMenu Account{accName = ""}
                return (Just (updateMenu (boxBefore menu) updatedMenu'))
            "3" -> do
                let updatedMenu = _updateAccs menu Account{accName = ""}
                return (Just (updateMenu (boxBefore menu) updatedMenu))
            _   -> return (Just (updateMenu (action menu) menu))
        RegisterMatch -> case input of
            "1" -> do
                updatedMenu <- _createMatch menu
                Just <$> return (updatedMenu)
            "2" -> Just <$> return (updateMenu (boxBefore menu) menu)
            "3" -> Just <$> return (updateMenu (boxBefore menu) menu)
            _   -> Just <$> return (updateMenu (action menu) menu)
        Rank -> return (Just (updateMenu (boxBefore menu) menu))
        Matches -> case input of
            "1" -> do
                let idxMatch = (indexMatch menu) + 1
                let updatedMenu = updateMatchesMenu menu idxMatch
                updatedMenu
            "2" -> do
                let idxMatch = (indexMatch menu) - 1
                updatedMenu <- if idxMatch == 0
                               then Just <$> return (updateMenu (boxBefore menu) menu)
                               else updateMatchesMenu menu idxMatch
                return updatedMenu
            _ -> return $ Just menu
        FinishMatch -> return (Just (updateMenu (boxBefore menu) beginGame))
        _ -> return (Just (updateMenu (action menu) menu))


_loadMatch :: Menu -> IO (Maybe Menu)
_loadMatch menu = do
    wordList <- UT.getWordList
    startTime <- getCurrentTime
    updatedMatch <- gameLoop (currentMatch menu) wordList startTime ""
    updatedMenu <- _menuPauseOrEnd menu updatedMatch
    return (Just updatedMenu)

_menuPauseOrEnd :: Menu -> Match -> IO Menu
_menuPauseOrEnd menu updatedMatch = do
    match <- getMatchByName (mName updatedMatch)
    case match of
        Just match -> do
            return (updateMenu StartMenu beginGame)
        Nothing    -> do
            let updatedMenu = _updateCurrentMatch menu updatedMatch
            putStrLn (show (pScore (mP1 updatedMatch)))
            putStrLn (show (pScore (mP2 updatedMatch)))
            putStrLn (show (currentMatch updatedMenu))
            return (updateMenu FinishMatch updatedMenu)

_accsFull :: Menu -> Bool
_accsFull menu = accName (p2 menu) /= ""

_createMatch :: Menu -> IO Menu
_createMatch menu = do
    putStr "nome_da_partida> "
    hFlush stdout
    newMatchName <- getLine
    if newMatchName == "2" || newMatchName == "" then do
        return menu
    else do
        matchExists <- VL.matchExistsValidation newMatchName
        if matchExists then do
            putStrLn "partida ja registrada"
            _createMatch menu
        else do
            match <- createMatch newMatchName (p1 menu) (p2 menu)
            let updatedMatch = _updateCurrentMatch menu match
            return $ updateMenu BeforeGame updatedMatch

_login :: Menu -> IO Menu
_login menu = do
    putStr "nome_de_usuario> "
    hFlush stdout
    accName <- getLine
    if accName == "2" || accName == "" then do
        return menu
    else do
        accExists <- VL.accExistsValidation accName
        if not accExists then do
            putStrLn "nome não registrado"
            _login menu
        else do
            acc <- getAccByName accName
            case acc of
                Just acc -> do
                    if acc == p1 menu then do
                        putStrLn "usuario ja logado"
                        _login menu
                    else do
                        let updatedAccs = _updateAccs menu acc
                        if _accsFull updatedAccs then do
                            putStrLn "acc fulll"
                            return $ updateMenu StartMenu updatedAccs
                        else do
                            return $ updateMenu NewGame updatedAccs


-- Recebe input do usuário para criar uma conta dentro de um menu, evitando duplicatas
-- Recebe: um Menu
-- Retorna: um Menu com uma conta adicionada
_createAcc :: Menu -> IO Menu
_createAcc menu = do
    putStr "nome_de_usuario> "
    hFlush stdout
    newAccName <- getLine
    if newAccName == "2" || newAccName == "" then do
        return menu
    else do
        accExists <- VL.accExistsValidation newAccName
        if accExists then do
                putStrLn "nome já registrado"
                _createAcc menu
        else do
            acc <- createAcc newAccName
            let updatedAccs = _updateAccs menu acc
            if _accsFull updatedAccs then do
                return $ updateMenu StartMenu updatedAccs
            else do
                return $ updateMenu NewGame updatedAccs


-- Recebe: um menu
-- Recebe: uma match nova
-- Retorna: o menu com sua match substituída pela nova passada como argumento
_updateCurrentMatch :: Menu -> Match -> Menu
_updateCurrentMatch menu match = menu {currentMatch = match}

_continueGame :: Menu -> IO Menu
_continueGame menu = do
  putStr "nome_da_partida> "
  hFlush stdout
  matchName <- getLine
  maybeMatch <- getMatchByName matchName
  case maybeMatch of
    Just match -> return $ (_updateCurrentMatch menu match)
    Nothing    -> do
               putStrLn "partida não encontrada"
               _continueGame menu

-- Recebe: um Menu
-- Recebe: uma Account
-- Retorna: Um menu com a account do player adicionada como jogador
_updateAccs :: Menu -> Account -> Menu
_updateAccs menu acc
  | p1 menu == acc = menu
  | accName (p1 menu) == "" = menu {p1 = acc}
  | otherwise = menu {p2 = acc}


-- Recebe: um Menu
-- Retorna: o menu com o rank atualizado
_getRank :: Menu -> IO Menu
_getRank menu = do
    accs <- getAccRank
    let updatedRank = _updateRank menu accs
    return $ updateMenu Rank updatedRank


-- Recebe: um Menu e um array de Accounts
-- Retorna: o menu com o rank substituído pelo array
_updateRank :: Menu -> [Account] -> Menu
_updateRank menu accs = menu {accsRank = accs}

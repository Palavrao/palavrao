{-# LANGUAGE DeriveGeneric #-}

module Core.Menu where

import Text.Printf
import System.Console.ANSI
import System.Exit
import Data.Char
import GHC.Generics
import Data.Aeson
import Interface.BoxesMenu
import System.IO
import Controllers.AccountsController
import Controllers.MatchesController
import Data.Time.Clock (getCurrentTime)
import Utils.Utils as UT
import Utils.Validator as VL
import Core.Game

_drawMenu :: Menu -> IO ()
_drawMenu menu = do
    clearScreen
    mapM_ putStrLn (box menu)

menuLoop :: Menu -> Int -> IO ()
menuLoop menu = do
    _drawMenu menu
    userInput <- getLine
    updatedMenu <- _menuFlux menu userInput
    case updatedMenu of 
        Just updatedMenu -> menuLoop updatedMenu pageIndex
        Nothing -> return ()

_menuFlux :: Menu -> String -> IO(Maybe Menu)
_menuFlux menu input = do
    case action menu of
        StartMenu -> case input of
            "1" -> do
                if (_accsFull menu) then do
                    Just <$> return (updateMenu RegisterMatch menu)
                else do 
                    Just <$> return (updateMenu NewGame menu)
            "2" -> Just <$> return (updateMenu ContinueGame menu)
            "3" -> 
                if (_accsFull menu) then do 
                    Just <$> return (updateMenu (action menu) menu)
                else do    
                    Just <$> return (updateMenu Register menu)
            "4" -> Just <$> return (updateMenu Rules menu)
            "5" -> do
                updatedMenu <- _getRank menu
                Just <$> return (updatedMenu)
            "6" -> exitSuccess >> return Nothing
            _   -> Just <$> return (updateMenu (action menu) menu)
        NewGame -> case input of 
            "1" -> Just <$> return (updateMenu Register menu)
            "2" -> Just <$> return (updateMenu Login menu)
            "3" -> Just <$> return (updateMenu (boxBefore menu) menu)
            _   -> Just <$> return (updateMenu (action menu) menu)
        Login -> case input of
            "1" -> do
                updatedMenu <- _login menu
                Just <$> return (updatedMenu)
            "2" -> Just <$> return (updateMenu (boxBefore menu) menu)
            _   -> Just <$> return (updateMenu (action menu) menu)
        Register -> case input of
            "1" -> do
                updatedMenu <- _createAcc menu
                Just <$> return (updatedMenu)
            "2" -> Just <$> return (updateMenu (boxBefore menu) menu)
            _   -> Just <$> return (updateMenu (action menu) menu)
        ContinueGame -> case input of 
            "1" -> Just <$> return (updateMenu BeforeGame menu)
            "2" -> Just <$> return (updateMenu Matches menu)
            "3" -> Just <$> return (updateMenu (boxBefore menu) menu)
            _   -> Just <$> return (updateMenu (action menu) menu)
        Rules -> case input of 
            _   -> Just <$> return (updateMenu (boxBefore menu) menu)
        BeforeGame -> case input of
            "1" -> do
                wordList <- UT.getWordList
                startTime <- getCurrentTime
                finishedMatch <- gameLoop (currentMatch menu) wordList startTime ""
                updatedMenu <- _menuPauseOrEnd menu finishedMatch
                Just <$> return updatedMenu
            "2" -> do
                let updatedMenu = menu {p1 = p2 menu}
                let updatedMenu' = _updateAccs updatedMenu Account{accName = ""}
                Just <$> return (updateMenu (boxBefore menu) updatedMenu')
            "3" -> do
                let updatedMenu = _updateAccs menu Account{accName = ""}
                Just <$> return (updateMenu (boxBefore menu) updatedMenu)
            _   -> Just <$> return (updateMenu (action menu) menu)
        RegisterMatch -> case input of
            "1" -> do
                updatedMenu <- _createMatch menu
                Just <$> return (updatedMenu)
            "2" -> Just <$> return (updateMenu (boxBefore menu) menu)
            "3" -> Just <$> return (updateMenu (boxBefore menu) menu)
            _   -> Just <$> return (updateMenu (action menu) menu)
        Rank -> case input of
            _   -> Just <$> return (updateMenu (boxBefore menu) menu)
        Matches -> case input of
            "1" -> do
                let updatedPageIndex = max 0 (pageIndex - 1)
                    matchesPerPage = 5
                    menuBox = box menu ++ geraMatchLines matches updatedPageIndex matchesPerPage
                menuLoop menu updatedPageIndex
                return Nothing -- página anterior
            "2" -> do
                let updatedPageIndex = pageIndex + 1
                    matchesPerPage = 5
                    menuBox = box menu ++ geraMatchLines matches updatedPageIndex matchesPerPage
                menuLoop menu updatedPageIndex
                return Nothing --  próxima página
            _   -> Just <$> return (updateMenu (action menu) menu)
        FinishMatch -> case input of
            _   -> Just <$> return (updateMenu (boxBefore menu) beginGame)
        _ -> Just <$> return (updateMenu (action menu) menu)

_menuPauseOrEnd :: Menu -> Match -> IO(Menu)
_menuPauseOrEnd menu updatedMatch = do
    match <- getMatchByName (mName updatedMatch)
    case match of 
        Just match -> do
            return (updateMenu StartMenu beginGame)
        Nothing    -> do
            let updatedMenu = _updateCurrentMatch menu updatedMatch
            return (updateMenu FinishMatch updatedMenu)

_accsFull :: Menu -> Bool
_accsFull menu = not (accName (p2 menu) == "")

_createMatch :: Menu -> IO(Menu)
_createMatch menu = do
    putStr "nome_da_partida> "
    hFlush stdout
    newMatchName <- getLine 
    if (newMatchName == "2" || newMatchName == "") then do 
        return menu
    else do
        matchExists <- VL.matchExistsValidation newMatchName
        if (matchExists) then do
            putStrLn "partida ja registrada"
            _createMatch menu
        else do
            match <- createMatch newMatchName (p1 menu) (p2 menu)
            let updatedMatch = _updateCurrentMatch menu match
            return $ updateMenu BeforeGame updatedMatch

_login :: Menu -> IO(Menu)
_login menu = do
    putStr "nome_de_usuario> "
    hFlush stdout
    accName <- getLine
    if (accName == "2" || accName == "") then do 
        return menu
    else do
        accExists <- VL.accExistsValidation accName
        if (not accExists) then do
            putStrLn "nome não registrado"
            _login menu
        else do
            acc <- getAccByName accName
            case acc of
                Just acc -> do
                    if acc == (p1 menu) then do
                        putStrLn "usuario ja logado"
                        _login menu
                    else do
                        let updatedAccs = _updateAccs menu acc
                        if (_accsFull updatedAccs) then do
                            putStrLn "acc fulll"
                            return $ updateMenu StartMenu updatedAccs
                        else do
                            return $ updateMenu NewGame updatedAccs

_createAcc :: Menu -> IO(Menu)
_createAcc menu = do
    putStr "nome_de_usuario> "
    hFlush stdout
    newAccName <- getLine
    if (newAccName == "2" || newAccName == "") then do
        return menu
    else do
        accExists <- VL.accExistsValidation newAccName
        if (accExists) then do 
                putStrLn "nome já registrado"
                _createAcc menu
        else do
            acc <- createAcc newAccName
            let updatedAccs = _updateAccs menu acc
            if (_accsFull updatedAccs) then do
                return $ updateMenu StartMenu updatedAccs
            else do
                return $ updateMenu NewGame updatedAccs

_updateCurrentMatch :: Menu -> Match -> Menu
_updateCurrentMatch menu match = menu {currentMatch = match}

_updateAccs :: Menu -> Account -> Menu
_updateAccs menu acc = 
    if (p1 menu) == acc then
        menu
    else
        if (accName (p1 menu) == "") then
            menu {p1 = acc}
        else
            menu {p2 = acc}

_getRank :: Menu -> IO(Menu)
_getRank menu = do
    accs <- getAccRank
    let updatedRank = _updateRank menu accs
    return $ updateMenu Rank updatedRank


_updateRank :: Menu -> [Account] -> Menu
_updateRank menu accs = menu {accsRank = accs}

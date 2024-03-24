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
import Core.Game

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

_menuFlux :: Menu -> String -> IO(Maybe Menu)
_menuFlux menu input = do
    case action menu of
        StartMenu -> case input of
            "1" -> do
                let updatedMenu = _verifyIfAccs menu
                Just <$> return (updatedMenu)
            "2" -> Just <$> return (updateMenu ContinueGame menu)
            "3" -> Just <$> return (updateMenu Register menu)
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
            "2" -> Just <$> return (updateMenu (boxBefore menu) menu)
            _   -> Just <$> return (updateMenu (action menu) menu)
        Rules -> case input of 
            "1" -> Just <$> return (updateMenu (boxBefore menu) menu)
            _   -> Just <$> return (updateMenu (action menu) menu)
        BeforeGame -> case input of
            "1" -> do
                wordList <- UT.getWordList
                startTime <- getCurrentTime
                gameLoop (currentMatch menu) wordList startTime
                Just <$> return (updateMenu StartMenu menu)
            "2" -> Just <$> return (updateMenu (boxBefore menu) menu)
            "3" -> Just <$> return (updateMenu (boxBefore menu) menu)
            _   -> Just <$> return (updateMenu (action menu) menu)
        RegisterMatch -> case input of
            "1" -> do
                updatedMenu <- _createMatch menu
                Just <$> return (updatedMenu)
            "2" -> Just <$> return (updateMenu (boxBefore menu) menu)
            "3" -> Just <$> return (updateMenu (boxBefore menu) menu)
            _   -> Just <$> return (updateMenu (action menu) menu)
        _ -> Just <$> return (updateMenu (action menu) menu)

_getRank :: Menu -> IO(Menu)
_getRank menu = do
    accs <- getAccRank
    let updatedRank = _updateRank menu accs
    return $ updateMenu Rank updatedRank

_verifyIfAccs :: Menu -> Menu
_verifyIfAccs menu = 
    if (accName (p2 menu) == "") 
        then updateMenu NewGame menu
        else updateMenu RegisterMatch menu

_createMatch :: Menu -> IO(Menu)
_createMatch menu = do
    putStr "nome_da_partida> "
    hFlush stdout
    newMatchName <- getLine 
    case newMatchName of 
        "2" -> return menu
        ""  -> do
            putStrLn "nome invalido"
            _createMatch menu
        _   -> do
            match <- createMatch newMatchName (p1 menu) (p2 menu)
            let updatedMatch = _updateCurrentMatch menu match
            return $ updateMenu BeforeGame updatedMatch

_login :: Menu -> IO(Menu)
_login menu = do
    putStr "nome_de_usuario> "
    hFlush stdout
    accName <- getLine
    if (accName /= "2") 
        then do 
            acc <- getAccByName accName
            case acc of
                Just acc -> do
                    if acc == (p1 menu) 
                        then do
                            putStrLn "usuario ja logado"
                            _login menu
                    else do
                        let updatedAccs = _updateAccs menu acc
                        return $ updateMenu NewGame updatedAccs
                Nothing -> do 
                    putStrLn "nome nao cadastrado"
                    _login menu
        else do
            return $ updateMenu NewGame menu

_createAcc :: Menu -> IO(Menu)
_createAcc menu = do
    putStr "nome_de_usuario> "
    hFlush stdout
    newAccName <- getLine
    case newAccName of 
        "2" -> return menu
        ""  -> do
            putStrLn "nome invalido"
            _createAcc menu
        _   -> do
            acc <- createAcc newAccName
            let updatedAccs = _updateAccs menu acc
            if (accName (p2 updatedAccs) == "")
                then do
                    return $ updateMenu NewGame updatedAccs
                else do
                    return $ updateMenu StartMenu updatedAccs
            

_updateRank :: Menu -> [Account] -> Menu
_updateRank menu accs = menu {accsRank = accs}

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

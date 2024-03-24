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
            "5" -> Just <$> return (updateMenu Rank menu)
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

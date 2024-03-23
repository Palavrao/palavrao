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

_drawMenu :: Menu -> IO ()
_drawMenu menu = do
    clearScreen
    mapM_ putStrLn (box menu)

menuLoop :: Menu -> IO ()
menuLoop menu = do
    _drawMenu menu
    userInput <- getLine
    if (action menu == Login && userInput == "1") 
        then do
            updatedMenu <- _createAcc menu
            menuLoop updatedMenu
        else do
            action <- _inputToAction userInput menu
            let updatedMenu = updateMenu action menu
            menuLoop updatedMenu

_createAcc :: Menu -> IO(Menu)
_createAcc menu = do
    putStr "> "
    hFlush stdout
    newAccName <- getLine
    acc <- createAcc newAccName
    let updatedAccs = _updateAccs menu acc
    if (accName (p2 updatedAccs) == "")
        then do
            return $ updateMenu NewGame updatedAccs
        else do
            return $ updateMenu BeforeGame updatedAccs

_updateAccs :: Menu -> Account -> Menu
_updateAccs menu acc = 
    if (accName (p1 menu) == "") then
        menu {p1 = acc}
    else
        menu {p2 = acc}

_inputToAction :: String -> Menu -> IO(Action)
_inputToAction userInput menu = case (action menu) of
    StartMenu -> case userInput of
        "1" -> return NewGame
        "2" -> return ContinueGame
        "3" -> return Login
        "4" -> return Rules
        "5" -> exitSuccess >> return StartMenu
        _   -> return (action menu)
    NewGame -> case userInput of 
        "1" -> return Login
        "2" -> return Login
        "3" -> return (boxBefore menu)
        _   -> return (action menu)
    ContinueGame -> case userInput of 
        "1" -> return BeforeGame
        "2" -> return (boxBefore menu)
        _   -> return (action menu)
    Rules -> case userInput of 
        "1" -> return (boxBefore menu)
        _   -> return (action menu)
    Login -> case userInput of 
        "2" -> return (boxBefore menu)
    BeforeGame -> case userInput of
        -- "1" -> return 
        "2" -> return (boxBefore menu)
        "3" -> return (boxBefore menu)
        _   -> return (action menu)
    _ -> return (action menu)
{-# LANGUAGE DeriveGeneric #-}

module Core.Menu where

import Text.Printf
import System.Console.ANSI
import System.Exit
import Data.Char
import GHC.Generics
import Data.Aeson
import Interface.BoxesMenu

_drawMenu :: Menu -> IO ()
_drawMenu menu = do
    clearScreen
    setCursorPosition 0 0
    mapM_ putStrLn (box menu)

menuLoop :: Menu -> IO ()
menuLoop menu = do
    _drawMenu menu
    userInput <- getChar
    if userInput == 'P' || userInput == 'J'
        then print userInput --logica salvar dados e inicar partida
    else do
      action <- _inputToAction userInput menu
      let updatedMenu = updateMenu action menu
      menuLoop updatedMenu

_inputToAction :: Char -> Menu -> IO Action
_inputToAction userInput actualMenu = case userInput of
    'A' -> return NewGame
    'B' -> return ContinueGame
    'C' -> return Login
    'D' -> return Rules
    'L' -> return Login
    'V' -> return (_goBack (boxBefore actualMenu))
    'S' -> exitSuccess >> return StartMenu
    _   -> return StartMenu

_goBack :: String -> Action
_goBack boxBefore = case boxBefore of
    "NG" -> NewGame
    "SM" -> StartMenu
    _   -> StartMenu

{-# LANGUAGE DeriveGeneric #-}

module Menu where

import Text.Printf
import System.Console.ANSI
import Data.Char
import GHC.Generics
import Data.Aeson
import BoxesMenu

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
      let action = _inputToAction userInput menu
          updatedMenu = updateMenu action menu
      menuLoop updatedMenu

_inputToAction :: Char -> Menu -> Action
_inputToAction userInput actualMenu = case userInput of
    'A' -> NewGame
    'B' -> ContinueGame
    'C' -> Login
    'D' -> Rules
    'L' -> Login
    'V' -> _goBack (boxBefore actualMenu)
    _   -> StartMenu

_goBack :: String -> Action
_goBack boxBefore = case boxBefore of
    "NG" -> NewGame
    "SM" -> StartMenu
    _   -> StartMenu

{-
main :: IO ()
main = do
    menuLoop beginGame
-}

{-# LANGUAGE DeriveGeneric #-}

module Menu where

import Text.Printf
import System.Console.ANSI
import Data.Char
import GHC.Generics
import Data.Aeson

data Action = NewGame | ContinueGame | CreateAccount | Rules | Login | StartMenu | Begin deriving (Show, Eq)

data Menu = Menu {
    box :: [[Char]],
    boxBefore :: String
} deriving (Show, Generic)

instance ToJSON Menu
instance FromJSON Menu

startMenu :: Menu
startMenu = Menu {
    box = [
        "    ┌───────────────────────────────┐   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │          PALAVRÃO             │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │      A  novo jogo             │   ",
        "    │      B  continuar jogo        │   ",
        "    │      C  criar conta           │   ",
        "    │      D  regras                │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    └───────────────────────────────┘   "
    ], boxBefore = "N"}

updateMenu :: Action -> Menu -> Menu
updateMenu action menu = case action of
        NewGame -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │          PALAVRÃO             │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │      C criar conta            │   ",
        "    │      L login                  │   ",
        "    │      V voltar                 │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    └───────────────────────────────┘   "
    ], boxBefore = "SM"}
        ContinueGame -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │          PALAVRÃO             │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │      P nome da partida        │   ",
        "    │      V voltar                 │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    └───────────────────────────────┘   "
    ], boxBefore = "NG"}
        CreateAccount -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │          PALAVRÃO             │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │     J digitar nome            │   ",
        "    │     V voltar                  │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    └───────────────────────────────┘   "
    ], boxBefore = "NG"}

captureInput :: IO (Maybe Action)
captureInput = do
        input <- getChar
        clearScreen
        return $ _getAction input

_getAction :: Char -> Menu -> Maybe Action
_getAction input menu
        | input == 'S'    = Just Begin
        | input == 'A'    = Just NewGame
        | input == 'B'    = Just ContinueGame
        | input == 'C'    = Just CreateAccount
        | input == 'D'    = Just Rules
        | input == 'L'    = Just Login
        | input == 'V'    = _goBack (boxBefore menu)
        | otherwise       = Nothing

_goBack :: String -> Maybe Action
_goBack boxBefore
      | boxBefore == "NG" = Just NewGame
      | boxBefore == "SM" = Just StartMenu
      | otherwise         = Nothing
{-# LANGUAGE DeriveGeneric #-}

module Menu where

import Text.Printf
import System.Console.ANSI
import Data.Char
import GHC.Generics
import Data.Aeson

data Action = NewGame | ContinueGame | CreateAccount deriving (Show, Eq)

data Menu = Menu {
    curTiles :: [[Char]]
} deriving (Show, Generic)

instance ToJSON Menu
instance FromJSON Menu

_startMenu :: Menu
_startMenu = Menu {
    curTiles = [
        "    ┌───────────────────────────────┐   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │          PALAVRÃO             │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │      >  novo jogo             │   ",
        "    │      =  continuar jogo        │   ",
        "    │      !  criar conta           │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    └───────────────────────────────┘   "
    ]

    }

_updateMenu :: Action -> Menu -> Menu
_updateMenu action menu = case action of
        NewGame -> menu { curTiles = [
        "    ┌───────────────────────────────┐   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │          PALAVRÃO             │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │      +  novo jogo             │   ",
        "    │      =  continuar jogo        │   ",
        "    │      !  criar conta           │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    └───────────────────────────────┘   "
    ]}
        ContinueGame -> menu { curTiles = [
        "    ┌───────────────────────────────┐   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │          PALAVRÃO             │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │! + digite o nome dos usuários │   ",
        "    │                               │   ",
        "    │       > voltar                │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    └───────────────────────────────┘   "
    ]}
        CreateAccount -> menu { curTiles = [
        "    ┌───────────────────────────────┐   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │          PALAVRÃO             │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │     ! para digitar o nome     │   ",
        "    │     > voltar                  │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    └───────────────────────────────┘   "
    ]}

_captureInput :: IO (Maybe Action)
_captureInput = do
        input <- getChar
        clearScreen
        return $ _getAction input

_getAction :: Char -> Maybe Action
_getAction input
        | input == '>'    = Just NewGame
        | input == '='    = Just ContinueGame
        | input == '!'    = Just CreateAccount
        | otherwise       = Nothing
 
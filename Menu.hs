{-# LANGUAGE DeriveGeneric #-}

module Menu where

import Text.Printf
import System.Console.ANSI
import Data.Char
import GHC.Generics
import Data.Aeson

data Action = NewGame | ContinueGame | CreateAccount | Rules | Login  deriving (Show, Eq)

data Menu = Menu {
    box :: [[Char]]
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
    ]

    }

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
    ]}
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
    ]}
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
    ]}

captureInput :: IO (Maybe Action)
captureInput = do
        input <- getChar
        clearScreen
        return $ _getAction input

_getAction :: Char -> Maybe Action
_getAction input
        | input == 'A'    = Just NewGame
        | input == 'B'    = Just ContinueGame
        | input == 'C'    = Just CreateAccount
        | input == 'D'    = Just Rules
        | input == 'L'    = Just Login
        | otherwise       = Nothing
{-# LANGUAGE DeriveGeneric #-}

module Menu where

import Text.Printf
import System.Console.ANSI
import Data.Char
import GHC.Generics
import Data.Aeson

data Action = NewGame | ContinueGame | CreateAccount deriving (Show, Eq)

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

updateMenu :: Action -> Menu -> Menu
updateMenu action menu = case action of
        NewGame -> menu { box = [
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
        ContinueGame -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │          PALAVRÃO             │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │  + digite o nome da partida   │   ",
        "    │    > voltar                   │   ",
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

captureInput :: IO (Maybe Action)
captureInput = do
        input <- getChar
        clearScreen
        return $ _getAction input

_getAction :: Char -> Maybe Action
_getAction input
        | input == '>'    = Just NewGame
        | input == '='    = Just ContinueGame
        | input == '!'    = Just CreateAccount
        | otherwise       = Nothing
 
{-# LANGUAGE DeriveGeneric #-}

module Interface.BoxesMenu where

import Text.Printf
import System.Console.ANSI
import Data.Char
import GHC.Generics
import Data.Aeson

data Action = NewGame | ContinueGame | Rules | Login | StartMenu deriving (Show, Eq)

data Menu = Menu {
    box :: [[Char]],
    boxBefore :: String
} deriving (Show, Generic)

instance ToJSON Menu
instance FromJSON Menu

beginGame :: Menu
beginGame = Menu {
    box = [
        "                 ┌─────────────────────────────┐                ",
        "                 │                             │                ",
        "                 │    Redimensione para que    │                ",
        "                 │  a linha caiba no terminal! │                ",
        "                 │                             │                ",
        "<-------------------------------------------------------------->",
        "                 │           > Enter           │                ",
        "                 │                             │                ",
        "                 └─────────────────────────────┘                "
    ], boxBefore = "N"}

updateMenu :: Action -> Menu -> Menu
updateMenu action menu = case action of
        StartMenu -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │           PALAVRÃO            │   ",
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
     ], boxBefore = "SM"}
        NewGame -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │           PALAVRÃO            │   ",
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
        "    │           PALAVRÃO            │   ",
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
    ], boxBefore = "SM"}
        Login -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │           PALAVRÃO            │   ",
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
        Rules -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │           PALAVRÃO            │   ",
        "    │            REGRAS             │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    └───────────────────────────────┘   "
    ], boxBefore = "SM"}

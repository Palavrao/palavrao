{-# LANGUAGE DeriveGeneric #-}

module Interface.BoxesMenu where

import Text.Printf
import System.Console.ANSI
import Data.Char
import GHC.Generics
import Controllers.AccountsController
import Controllers.MatchesController

data Action = NewGame | ContinueGame | Rules | Login | Register | RegisterMatch | StartMenu | Rank | InvalidAction | BeforeGame deriving (Show, Eq)

data Menu = Menu {
    box :: [[Char]],
    p1 :: Account,
    p2 :: Account,
    accsRank :: [Account],
    action :: Action,
    boxBefore :: Action,
    currentMatch :: Match
} deriving (Show, Eq)

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
    ], boxBefore = InvalidAction, action = StartMenu, p1 = Account{accName = ""}, p2 = Account{accName = ""}, currentMatch = Match{mName = ""}}

updateMenu :: Action -> Menu -> Menu
updateMenu action menu = case action of
        StartMenu -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        printf "    │  %-5s                 %-5s  │   " (take 5 $ (accName (p1 menu))) (take 5 $ (accName (p2 menu))),
        "    │                               │   ",
        "    │           PALAVRÃO            │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │      1.  novo jogo            │   ",
        "    │      2.  continuar jogo       │   ",
        "    │      3.  criar conta          │   ",
        "    │      4.  regras               │   ",
        "    │      5.  rank                 │   ",
        "    │      6.  sair                 │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    └───────────────────────────────┘   "
     ], boxBefore = StartMenu, action = StartMenu}
        NewGame -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        printf "    │  %-5s                 %-5s  │   " (take 5 $ (accName (p1 menu))) (take 5 $ (accName (p2 menu))),
        "    │                               │   ",
        "    │           PALAVRÃO            │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │      1. criar conta           │   ",
        "    │      2. login                 │   ",
        "    │      3. voltar                │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │           novo jogo           │   ",
        "    └───────────────────────────────┘   "
    ], boxBefore = StartMenu, action = NewGame}
        ContinueGame -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        printf "    │  %-5s                 %-5s  │   " (take 5 $ (accName (p1 menu))) (take 5 $ (accName (p2 menu))),
        "    │                               │   ",
        "    │           PALAVRÃO            │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │      1. nome da partida       │   ",
        "    │      2. voltar                │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │           continuar           │   ",
        "    └───────────────────────────────┘   "
    ], boxBefore = StartMenu, action = ContinueGame}
        Login -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        printf "    │  %-5s                 %-5s  │   " (take 5 $ (accName (p1 menu))) (take 5 $ (accName (p2 menu))),
        "    │                               │   ",
        "    │           PALAVRÃO            │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │     1. digitar nome           │   ",
        "    │     2. voltar                 │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │            login              │   ",
        "    └───────────────────────────────┘   "
    ], boxBefore = StartMenu, action = Login}
        Rules -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        printf "    │  %-5s                 %-5s  │   " (take 5 $ (accName (p1 menu))) (take 5 $ (accName (p2 menu))),
        "    │                               │   ",
        "    │           PALAVRÃO            │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │     1. voltar                 │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │            regras             │   ",
        "    └───────────────────────────────┘   "
    ], boxBefore = StartMenu, action = Rules}
        BeforeGame -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        printf "    │  %-5s                 %-5s  │   " (take 5 $ (accName (p1 menu))) (take 5 $ (accName (p2 menu))),
        "    │                               │   ",
        "    │           PALAVRÃO            │   ",
        "    │                               │   ",
        printf "    │        %-16s       │   " (take 16 $ (mName (currentMatch menu))),
        "    │                               │   ",
        "    │      1.  ir para jogo         │   ",
        "    │      2.  tirar player 1       │   ",
        "    │      3.  tirar player 2       │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │        opcoes de jogo         │   ",
        "    └───────────────────────────────┘   "
    ], boxBefore = StartMenu, action = BeforeGame}
        Register -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        printf "    │  %-5s                 %-5s  │   " (take 5 $ (accName (p1 menu))) (take 5 $ (accName (p2 menu))),
        "    │                               │   ",
        "    │           PALAVRÃO            │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │     1. digitar nome           │   ",
        "    │     2. voltar                 │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │          criar conta          │   ",
        "    └───────────────────────────────┘   "
    ], boxBefore = StartMenu, action = Register}
        RegisterMatch -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        printf "    │  %-5s                 %-5s  │   " (take 5 $ (accName (p1 menu))) (take 5 $ (accName (p2 menu))),
        "    │                               │   ",
        "    │           PALAVRÃO            │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │     1. digitar nome           │   ",
        "    │     2. voltar                 │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │         criar partida         │   ",
        "    └───────────────────────────────┘   "
    ], boxBefore = StartMenu, action = RegisterMatch}
        Rank -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        printf "    │  %-5s                 %-5s  │   " (take 5 $ (accName (p1 menu))) (take 5 $ (accName (p2 menu))),
        "    │                               │   ",
        "    │           PALAVRÃO            │   ",
        "    │                               │   ",
        "    │                               │   ",
        printf "    │     1. %-5s  -  %-4s         │   " (take 5 $ accName ((accsRank menu) !! 0)) (take 4 $ show (accScore ((accsRank menu) !! 0))),
        printf "    │     2. %-5s  -  %-4s         │   " (take 5 $ accName ((accsRank menu) !! 1)) (take 4 $ show (accScore ((accsRank menu) !! 1))),
        printf "    │     3. %-5s  -  %-4s         │   " (take 5 $ accName ((accsRank menu) !! 2)) (take 4 $ show (accScore ((accsRank menu) !! 2))),
        printf "    │     4. %-5s  -  %-4s         │   " (take 5 $ accName ((accsRank menu) !! 3)) (take 4 $ show (accScore ((accsRank menu) !! 3))),
        printf "    │     5. %-5s  -  %-4s         │   " (take 5 $ accName ((accsRank menu) !! 4)) (take 4 $ show (accScore ((accsRank menu) !! 4))),
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │             rank              │   ",
        "    └───────────────────────────────┘   "
    ], boxBefore = StartMenu, action = Rank}
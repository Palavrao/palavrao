module Main where

import AccountsController
import MatchesController
import GameLoop



main :: IO ()
main = do
    verTabuleiro
    createAcc Account {name="fulano", score=0}
    createAcc Account {name="beltrano", score=0}
    createMatch Match {p1Name="fulano", p1Score=0, p2Name="beltrano", p2Score=0}
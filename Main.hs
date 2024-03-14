module Main where

import AccountsController
import MatchesController
import GameLoop


main :: IO ()
main = do
    
    createAcc Account {accName="fulano", score=0}
    createAcc Account {accName="beltrano", score=0}
    let m = Match {matchName="match1", p1Name="fulano", p1Score=0, p2Name="beltrano", p2Score=0}
    createMatch m

    let t = startTabuleiro m
    verTabuleiro t
    
    --mapM (mapM replacements) (curTiles t) 
    putStr ""

    
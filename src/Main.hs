{- 
    Jogo de palavras baseado em Scrabble, com sistema de contas e partidas
    
    Produzido por: 
    - Samuel Lucas (https://github.com/SamuelLucasVM)
    - Helena Sátyro (https://github.com/helenasatyro)
    - Eliane Tamara (https://github.com/elianetamara)
    - Paulo Ricardo (https://github.com/paulorpn)
    - Rayanne Macêdo (https://github.com/raiaiaia)
-} 

module Main where

import Utils.Utils as UT
import Core.Menu
import Interface.BoxesMenu

main :: IO ()
main = do
    UT.startPersistence
    menuLoop beginGame
    
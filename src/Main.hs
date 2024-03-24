module Main where

import Utils.Utils as UT
import Core.Menu
import Interface.BoxesMenu

main :: IO ()
main = do
    UT.startPersistence
    menuLoop beginGame
    
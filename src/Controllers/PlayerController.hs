{-# LANGUAGE DeriveGeneric #-}

module Controllers.PlayerController where

import Utils.Utils as UT
import GHC.Generics
import Data.Aeson
import Controllers.AccountsController
import Controllers.LettersController

data Player = Player {
    pAcc :: Account,
    pLetters :: [Letter],
    pScore :: Int
} deriving (Show, Generic, Eq)

instance ToJSON Player
instance FromJSON Player

createPlayer :: Account -> Player
createPlayer acc = Player {
        pAcc = acc,
        pLetters = [],
        pScore = 0
    }

updateLetters :: Player -> [Letter] -> Player
updateLetters player newLetters = player {pLetters = newLetters}

addLetters :: Player -> [Letter] -> Player
addLetters player newLetters = player {pLetters = (pLetters player) ++ newLetters}

incScore :: Player -> Int -> Player
incScore player score = player {pScore = (pScore player + score)}
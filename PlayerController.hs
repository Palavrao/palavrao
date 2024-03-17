{-# LANGUAGE DeriveGeneric #-}

module PlayerController where

import Utils as UT
import GHC.Generics
import Data.Aeson
import AccountsController
import LettersController

data Player = Player {
    pAcc :: Account,
    pLetters :: [Letter],
    pScore :: Int
} deriving (Show, Generic)

instance ToJSON Player
instance FromJSON Player

createPlayer :: Account -> Player
createPlayer acc = Player {
        pAcc = acc,
        pLetters = [],
        pScore = 0
    }

updatePlayerLetters :: Player -> [Letter] -> Player
updatePlayerLetters player newLetters = player {pLetters = newLetters}

updatePlayerScore :: Player -> Int -> Player
updatePlayerScore player newScore = player {pScore = newScore}
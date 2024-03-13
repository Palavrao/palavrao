{-# LANGUAGE DeriveGeneric #-}

module MatchesController where

import GHC.Generics
import Data.Aeson

import Utils as UT

data Match = Match {
    p1Name :: String,
    p1Score :: Int,
    p2Name :: String,
    p2Score :: Int
} deriving (Show, Generic)

instance ToJSON Match
instance FromJSON Match

createMatch :: Match -> IO()
createMatch match = UT.updateJsonFile match "data/matches.json"
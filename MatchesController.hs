{-# LANGUAGE DeriveGeneric #-}

module MatchesController where

import GHC.Generics
import Data.Aeson
import BoardController;
import AccountsController

import Utils as UT

data Match = Match {
    matchName :: String,
    board :: Board,
    p1Name :: String,
    p1Score :: Int,
    p2Name :: String,
    p2Score :: Int
} deriving (Show, Generic)

instance ToJSON Match
instance FromJSON Match

saveMatch :: Match -> IO()
saveMatch match = UT.incJsonFile match "data/matches.json"

createMatch :: String -> Account -> Account -> IO(Match)
createMatch name p1 p2 = do
    let match = Match {matchName = name, board = startBoard, p1Name = accName p1, p1Score = 0, p2Name = accName p2, p2Score = 0}
    saveMatch match
    return match

getMatchByName :: String -> IO (Maybe Match)
getMatchByName targetName = do
    matches <- UT.readJsonFile "data/matches.json"
    return $ UT.getObjByField matches matchName targetName
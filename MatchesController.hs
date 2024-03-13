{-# LANGUAGE DeriveGeneric #-}

module MatchesController where

import GHC.Generics
import Data.Aeson

import Utils as UT

data Match = Match {
    matchName :: String,
    p1Name :: String,
    p1Score :: Int,
    p2Name :: String,
    p2Score :: Int
} deriving (Show, Generic)

instance ToJSON Match
instance FromJSON Match

createMatch :: Match -> IO()
createMatch match = UT.updateJsonFile match "data/matches.json"

getMatchByName :: String -> IO (Maybe Match)
getMatchByName targetName = do
    matches <- UT.readJsonFile "data/matches.json"
    return $ _getMatchByName matches targetName

_getMatchByName :: [Match] -> String -> Maybe Match
_getMatchByName [] _ = Nothing
_getMatchByName (match:matcht) targetName 
    | matchName match == targetName = Just match
    | otherwise = _getMatchByName matcht targetName
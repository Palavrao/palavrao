{-# LANGUAGE DeriveGeneric #-}

module MatchesController where

import GHC.Generics
import Data.Aeson
import BoardController;
import AccountsController

import Utils as UT

data Match = Match {
    matchName :: String,
    matchBoard :: Board,
    matchTurn :: Bool,
    matchP1 :: Player,
    matchP2 :: Player
} deriving (Show, Generic)

instance ToJSON Match
instance FromJSON Match

data Player = Player {
    pAcc :: Account,
    pLetters :: [Char],
    pScore :: Int
} deriving (Show, Generic)

instance ToJSON Player
instance FromJSON Player

saveMatch :: Match -> IO()
saveMatch match = UT.incJsonFile match "data/matches.json"

createPlayer :: Account -> Player
createPlayer acc = Player {pAcc = acc, pLetters = [], pScore = 0}

createMatch :: String -> Account -> Account -> IO(Match)
createMatch name acc1 acc2 = do
    let match = Match {matchName = name, matchBoard = startBoard, matchTurn = False, matchP1 = createPlayer acc1, matchP2 = createPlayer acc2}
    saveMatch match
    return match

matchExists :: String -> IO (Bool)
matchExists name = do
    maybeMatch <- getMatchByName name
    return $ case maybeMatch of
        Nothing -> False
        Just _ -> True

getMatchByName :: String -> IO (Maybe Match)
getMatchByName targetName = do
    matches <- UT.readJsonFile "data/matches.json"
    return $ UT.getObjByField matches matchName targetName
{-# LANGUAGE DeriveGeneric #-}

module MatchesController where

import GHC.Generics
import Data.Aeson
import BoardController;
import AccountsController
import PlayerController as PC
import LettersController
import Utils as UT

data Match = Match {
    mName :: String,
    mBoard :: Board,
    mTurn :: Bool,
    mP1 :: Player,
    mP2 :: Player,
    mLetters :: [Letter]
} deriving (Show, Generic)

instance ToJSON Match
instance FromJSON Match

saveMatch :: Match -> IO()
saveMatch match = UT.incJsonFile match "data/matches.json"

createMatch :: String -> Account -> Account -> IO(Match)
createMatch name acc1 acc2 = do
    let match = Match {
            mName = name,
            mBoard = startBoard,
            mTurn = False,
            mP1 = createPlayer acc1,
            mP2 = createPlayer acc2,
            mLetters = startLetters
        }
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
    return $ UT.getObjByField matches mName targetName

updateMatchLetters :: Match -> [Letter] -> Match
updateMatchLetters match newLetters = match {mLetters = newLetters}

updateMatchPlayer :: Match -> Player -> Match
updateMatchPlayer match player 
    | mTurn match = match {mP1 = player}
    | otherwise = match {mP2 = player}

updatePlayerLetters :: Match -> IO(Match)
updatePlayerLetters match = do
    let (updatedLetters, playerLetters) = UT.popRandomElements (mLetters match) (7 - (length (pLetters player)))

    let updatedMatch = updateMatchLetters match updatedLetters
    let updatedPlayer = PC.updatePlayerLetters player playerLetters

    return $ updateMatchPlayer updatedMatch updatedPlayer
    where
        player 
            | mTurn match = mP1 match
            | otherwise = mP2 match

toggleMatchTurn :: Match -> Match 
toggleMatchTurn match = match {mTurn = not (mTurn match)}

updateMatchBoard :: Match -> Board -> Match
updateMatchBoard match newBoard = match {mBoard = newBoard}

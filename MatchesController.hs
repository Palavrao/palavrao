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
} deriving (Show, Generic, Eq)

instance ToJSON Match
instance FromJSON Match

saveMatchJson :: Match -> IO()
saveMatchJson match = UT.incJsonFile match "data/matches.json"

deleteMatchFromJson :: Match -> IO()
deleteMatchFromJson match = do 
    UT.deleteJsonObj match "data/matches.json"

updateMatchJson :: Match -> IO()
updateMatchJson updatedMatch = do
    accs <- UT.readJsonFile "data/matches.json"
    let updatedAccs = _getUpdatedMatches accs targetMatchName updatedMatch
    UT.writeJsonFile updatedAccs "data/matches.json"
    where
        targetMatchName = mName updatedMatch

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
    saveMatchJson match
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

incPlayerScore :: Match -> Int -> Match
incPlayerScore match incScore
    | mTurn match = match {mP2 = PC.incPlayerScore (mP2 match) incScore}
    | otherwise = match {mP1 = PC.incPlayerScore (mP1 match) incScore}

updatePlayerLetters :: Match -> IO(Match)
updatePlayerLetters match = do
    let (updatedLetters, playerLetters) = UT.popRandomElements (mLetters match) (7 - (length (pLetters player)))

    let updatedMatch = updateMatchLetters match updatedLetters
    let updatedPlayer = PC.updatePlayerLetters player playerLetters

    return $ _updateMatchPlayer updatedMatch updatedPlayer
    where
        player 
            | mTurn match = mP2 match
            | otherwise = mP1 match

toggleMatchTurn :: Match -> Match 
toggleMatchTurn match = match {mTurn = not (mTurn match)}

updateMatchBoard :: Match -> Board -> Match
updateMatchBoard match newBoard = match {mBoard = newBoard}

_updateMatchPlayer :: Match -> Player -> Match
_updateMatchPlayer match player 
    | mTurn match = match {mP2 = player}
    | otherwise = match {mP1 = player}

_getUpdatedMatches :: [Match] -> String -> Match -> [Match]
_getUpdatedMatches [] _ _ = []
_getUpdatedMatches (match:tail) targetMatchName updatedMatch
    | mName match == targetMatchName = (updatedMatch:tail)
    | otherwise = (match:_getUpdatedMatches tail targetMatchName updatedMatch)
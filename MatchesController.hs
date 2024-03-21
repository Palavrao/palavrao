{-# LANGUAGE DeriveGeneric #-}

module MatchesController where

import GHC.Generics
import Data.Aeson
import BoardController;
import AccountsController
import PlayerController
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

matchesPath :: String
matchesPath = "data/matches.json"

saveMatchJson :: Match -> IO()
saveMatchJson match = UT.incJsonFile match matchesPath

deleteMatchFromJson :: String -> IO()
deleteMatchFromJson name = do
    match <- getMatchByName name
    UT.deleteFromJson match matchesPath

updateMatchJson :: Match -> IO()
updateMatchJson updatedMatch = do
    accs <- getMatches
    let updatedAccs = _getUpdatedMatches accs targetMatchName updatedMatch
    UT.writeJsonFile updatedAccs matchesPath
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

    matchP1Letters <- updatePlayerLetters match
    let matchP2 = toggleMatchTurn matchP1Letters
    matchP2Letters <- updatePlayerLetters matchP2
    let initMatch = toggleMatchTurn matchP2Letters

    saveMatchJson initMatch
    return initMatch

finishMatch :: Match -> IO()
finishMatch match = do
    incAccScore (accName (pAcc p1)) (pScore p1)
    incAccScore (accName (pAcc p2)) (pScore p2)
    deleteMatchFromJson (mName match)
    where
        p1 = mP1 match
        p2 = mP2 match

matchExists :: String -> IO (Bool)
matchExists name = do
    maybeMatch <- getMatchByName name
    return $ case maybeMatch of
        Nothing -> False
        Just _ -> True

getMatches :: IO ([Match])
getMatches = do
    UT.readJsonFile matchesPath

getMatchByName :: String -> IO (Maybe Match)
getMatchByName targetName = do
    matches <- getMatches
    return $ UT.getObjByField matches mName targetName

incPlayerScore :: Match -> Int -> Match
incPlayerScore match score
    | mTurn match = match {mP2 = incScore (mP2 match) score}
    | otherwise = match {mP1 = incScore (mP1 match) score}

updatePlayerLetters :: Match -> IO(Match)
updatePlayerLetters match = do
    (playerLetters, updatedLetters) <- UT.popRandomElements (mLetters match) (7 - (length (pLetters player)))

    let updatedMatch = _updateMatchLetters match updatedLetters
    let updatedPlayer = updateLetters player playerLetters

    return $ _updateMatchPlayer updatedMatch updatedPlayer
        where
            player
                | mTurn match = mP2 match
                | otherwise = mP1 match

toggleMatchTurn :: Match -> Match
toggleMatchTurn match = match {mTurn = not (mTurn match)}

updateMatchBoard :: Match -> Board -> Match
updateMatchBoard match newBoard = match {mBoard = newBoard}

switchPlayerLetter :: Match -> Letter -> IO(Match)
switchPlayerLetter match letter = do
    (newLetter, updatedLetters) <- UT.popRandomElements (mLetters match) 1

    let playerLetters = (head newLetter:pLetters player)

    let updatedPlayerLetters = UT.removeOneElement playerLetters letter

    let updatedMatch = _updateMatchLetters match updatedLetters
    let updatedPlayer = updateLetters player updatedPlayerLetters

    return $ _updateMatchPlayer updatedMatch updatedPlayer
    where
        player
            | mTurn match = mP2 match
            | otherwise = mP1 match

_updateMatchPlayer :: Match -> Player -> Match
_updateMatchPlayer match player
    | mTurn match = match {mP2 = player}
    | otherwise = match {mP1 = player}

_getUpdatedMatches :: [Match] -> String -> Match -> [Match]
_getUpdatedMatches [] _ _ = []
_getUpdatedMatches (match:tail) targetMatchName updatedMatch
    | mName match == targetMatchName = (updatedMatch:tail)
    | otherwise = (match:_getUpdatedMatches tail targetMatchName updatedMatch)

_updateMatchLetters :: Match -> [Letter] -> Match
_updateMatchLetters match newLetters = match {mLetters = newLetters}
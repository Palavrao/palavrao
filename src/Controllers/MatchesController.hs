{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Controllers.MatchesController where

import GHC.Generics
import Data.Aeson
import Controllers.BoardController;
import Controllers.AccountsController
import Controllers.PlayerController
import Controllers.LettersController
import Utils.Utils as UT
import Data.Time.Clock
import System.Console.ANSI


data Match = Match {
    mName :: String,
    mBoard :: Board,
    mTurn :: Bool,
    mP1 :: Player,
    mP2 :: Player,
    mLetters :: [Letter],
    mUsedWords :: [String],
    mTimer :: Float,
    mSkips :: Int
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
            mLetters = startLetters,
            mUsedWords = [],
            mTimer = 300,
            mSkips = 0
        }

    matchP1Letters <- updatePlayerLetters match
    let matchP2 = toggleMatchTurn matchP1Letters
    matchP2Letters <- updatePlayerLetters matchP2
    let initMatch = toggleMatchTurn matchP2Letters

    saveMatchJson initMatch
    return initMatch

finishMatch :: Match -> IO(Match)
finishMatch match = do
    incAccScore (accName (pAcc p1)) (pScore p1)
    incAccScore (accName (pAcc p2)) (pScore p2)
    deleteMatchFromJson (mName match)
    return match
    where
        p1 = mP1 match
        p2 = mP2 match

updateMatchTimer :: Match -> Float -> Match
updateMatchTimer match time = match { mTimer = time }

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
    let updatedPlayer = addLetters player playerLetters

    return $ _updateMatchPlayer updatedMatch updatedPlayer
        where
            player
                | mTurn match = mP2 match
                | otherwise = mP1 match

getBestPlayer :: Match -> Player
getBestPlayer match
    | p1Score > p2Score = p1
    | p1Score < p2Score = p2
    | otherwise = Player{pAcc = Account{accName = "Nenhum! Empate :D"}}
    where 
        p1 = mP1 match
        p2 = mP2 match
        p1Score = pScore p1
        p2Score = pScore p2

skipPlayerTurn :: Match -> Match
skipPlayerTurn match = (toggleMatchTurn match) {mSkips = mSkips match + 1}

resetMatchSkipsQtd :: Match -> Match
resetMatchSkipsQtd match = match{mSkips = 0}

toggleMatchTurn :: Match -> Match
toggleMatchTurn match = match {mTurn = not (mTurn match), mTimer = 300}

updateMatchBoard :: Match -> Board -> Match
updateMatchBoard match newBoard = match {mBoard = newBoard}

updateMUsedWords :: Match -> [String] -> Match
updateMUsedWords match words = match {mUsedWords = words ++ mUsedWords match}

switchPlayerLetter :: Match -> Letter -> IO(Match)
switchPlayerLetter match letter = do
    (newLetter, updatedLetters) <- UT.popRandomElements (mLetters match) 1

    let playerLetters = UT.removeOneElement (pLetters player) letter
    let updatedPlayer = updateLetters player playerLetters 

    let updatedMatch = _updateMatchLetters match (letter:updatedLetters)
    let updatedPlayer' = addLetters updatedPlayer newLetter

    return $ _updateMatchPlayer updatedMatch updatedPlayer'
    where
        player
            | mTurn match = mP2 match
            | otherwise = mP1 match


removePlayerLetters :: Match -> [Char] -> Match
removePlayerLetters match toRemove = _updateMatchPlayer match (updateLetters player newLetters)
    where 
        newLetters = getLetterArray (UT.removeChars toRemove [letter l | l <- (pLetters player)])
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
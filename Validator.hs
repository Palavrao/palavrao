module Validator where

import MatchesController
import BoardController
import PlayerController
import Data.Char
import MatchesController

isStringInt :: String -> Bool
isStringInt str = case reads str :: [(Int, String)] of
    [(num, "")] -> True
    _           -> False

initialValidation :: Match -> String -> Bool
initialValidation _ "" = False
initialValidation match linha
    | length palavras /= 3 = False
    | otherwise = ((palavras !! 1) `elem` ["V", "H"]) && (coordValidation coord)  && (tileValidationSize isHorizontal (x, y) word) && (wordValidation word) && (tileValidationLetters letrasNoBoard word)

    where
        palavras = words $ map toUpper linha
        coord = (palavras !! 0)
        isHorizontal = (palavras !! 1) == "H"       
        word = (palavras !! 2)
        x = ord (head coord ) - ord 'A'
        y = (read (tail coord) :: Int)
        letrasNoBoard = takeUpTo isHorizontal match (x,y) (length word) --DEBUGAR


coordValidation :: [Char] -> Bool
coordValidation (x:y) = x `elem` ['A' .. 'O'] && (isStringInt y) && (ord x - ord 'A' >= 0 && ord x - ord 'A' <= 14) && ((read y :: Int) >= 0 && (read y :: Int) <= 14)


wordValidation :: String -> Bool
wordValidation word = [] == [l | l <- word, not (isLetter l)]

--DEBUGAR
tileValidationLetters :: String -> String -> Bool
tileValidationLetters (tileHead:tileTail) (wordHead:wordTail)
    | wordTail == [] = True
    | (tileHead `elem` ['A'..'Z']) && tileHead /= wordHead = False
    | otherwise = tileValidationLetters tileTail wordTail

takeUpTo :: Bool -> Match -> (Int, Int) -> Int -> [Char]
takeUpTo isHorizontal match (x, y) len
    | isHorizontal = (take len (drop y (b !! x)))
    | otherwise = take len $ map (!! x) $ drop y b
    where b = curTiles (mBoard match)


-- Recebe o board, o booleano que informa se eh horizontal, as coordenadas x e y (col, row) indexadas em zero, a palavra, e retorna se passou ou não
tileValidationSize :: Bool -> (Int, Int) -> String -> Bool
tileValidationSize isHorizontal (x, y) word
    | isHorizontal = (x <= 15 - (length word) && x >= 0) && (y >= 0 && y <= 14)
    | otherwise = (y <= 15 - (length word) && y >= 0) && (x >= 0 && x <= 14)
{-
playerHasLetter :: Player -> Char -> Bool
playerHasLetter player letter = any (\l -> letter l == 'A') (pLetters player)
 -}

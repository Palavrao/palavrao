module Utils.Validator where

import Controllers.MatchesController
import Controllers.BoardController
import Controllers.PlayerController
import Data.Char
import Controllers.MatchesController
import Utils.Utils as UT

isStringInt :: String -> Bool
isStringInt str = case reads str :: [(Int, String)] of
    [(num, "")] -> True
    _           -> False

initialValidation :: Match -> [String] -> String -> (Bool, [String])
initialValidation _ _ "" = (False, [])
initialValidation match wordlist linha
    | length palavras /= 3 = (False, [])
    | otherwise = (resCoordenadas, resPalavras)

    where
        palavras = words $ map toUpper linha
        coord = (palavras !! 0)
        isHorizontal = (palavras !! 1) == "H"       
        word = (palavras !! 2)
        x = ord (head coord ) - ord 'A'
        y = (read (tail coord) :: Int)
        letrasNoBoard = _takeUpTo isHorizontal match (x,y) (length word) --DEBUGAR
        resCoordenadas = (((palavras !! 1) `elem` ["V", "H"]) && (_coordValidation coord) && (_tileValidationSize isHorizontal (x, y) word) && (_tileValidationLetters letrasNoBoard word))
        resPalavras = (_allWordsExist match wordlist (getWords (placeWord (x,y,isHorizontal,word) (mBoard match))))


_wordExistenceValidation :: Match -> [String] -> String -> Bool
_wordExistenceValidation match wordList word = ((map toLower word) `elem` (mUsedWords match)) || ((map toLower word) `elem` wordList)


_allWordsExist :: Match -> [String] -> [String] -> [String]
_allWordsExist match wordlist wordsToTest = [x | x <- wordsToTest, (_wordExistenceValidation match wordlist x) == False] 


_coordValidation :: [Char] -> Bool
_coordValidation (x:y) = (x `elem` ['A' .. 'O']) && (isStringInt y) && ((read y :: Int) >= 0 && (read y :: Int) <= 14)


_wordValidation :: String -> Bool
_wordValidation word = [] == [l | l <- word, not (isLetter l)]

--DEBUGAR
_tileValidationLetters :: String -> String -> Bool
_tileValidationLetters [] [] = True
_tileValidationLetters (tileHead:tileTail) (wordHead:wordTail)
    | (tileHead `elem` ['A'..'Z']) && (tileHead /= wordHead) = False
    | otherwise = _tileValidationLetters tileTail wordTail

_takeUpTo :: Bool -> Match -> (Int, Int) -> Int -> [Char]
_takeUpTo isHorizontal match (x, y) len
    | isHorizontal = (take len (drop x (b !! y)))
    | otherwise = take len $ map (!! x) $ drop y b
    where b = curTiles (mBoard match)


-- Recebe o board, o booleano que informa se eh horizontal, as coordenadas x e y (col, row) indexadas em zero, a palavra, e retorna se passou ou não
_tileValidationSize :: Bool -> (Int, Int) -> String -> Bool
_tileValidationSize isHorizontal (x, y) word
    | isHorizontal = (x <= 15 - (length word) && x >= 0) && (y >= 0 && y <= 14)
    | otherwise = (y <= 15 - (length word) && y >= 0) && (x >= 0 && x <= 14)
{-
playerHasLetter :: Player -> Char -> Bool
playerHasLetter player letter = any (\l -> letter l == 'A') (pLetters player)
 -}

readWordInput :: String -> Match -> (Int, Int, Bool, String)
readWordInput linha match = (x, y, isHorizontal, word)
    where
        palavras = words $ map toUpper linha
        coord = (palavras !! 0)
        isHorizontal = (palavras !! 1) == "H"       
        word = (palavras !! 2)
        x = ord (head coord ) - ord 'A'
        y = (read (tail coord) :: Int)
        letrasNoBoard = _takeUpTo isHorizontal match (x,y) (length word)

module Validator where

import MatchesController
import BoardController
import PlayerController
import Data.Char
import MatchesController

initialValidation :: Match -> String -> Bool
initialValidation _ "" = False
initialValidation match linha
    | length palavras /= 3 = False
    | otherwise =
        (coordValidation (x, y)) && (lettersValidation (head direction) word) && (tileValidation (mBoard match) (x, y) (head direction) word 0)
    where
        palavras = words $ map toUpper linha
        coord = (palavras !! 0)
        x = ord (coord !! 0) - ord 'A'
        y = read (dropWhile (== '0') coord) + 1 :: Int
        isHorizontal = (palavras !! 1) == "H"
        word = (palavras !! 2)

coordValidation :: Char -> (Int, Int) -> Bool
coordValidation coord (x, y) = coord `elem` ['A' .. 'O'] && (x >= 0 || x <= 14) && (y >= 0 || y <= 14)

wordValidation :: String -> Bool
wordValidation word = [] == [l | l <- word, not (isLetter l)]

tileValidationLetters :: String -> String -> Bool
tileValidationLetters (tileHead:tileTail) (wordHead:wordTail)
    | wordTail == [] = True
    | not (isSymbol tileHead) && tileHead /= wordHead = False
    | otherwise = tileValidation tileTail wordTail

-- Recebe o board, o booleano que informa se eh horizontal, as coordenadas x e y (col, row) indexadas em zero, a palavra, e retorna se passou ou não
tileValidationSize :: Bool -> (Int, Int) -> String -> Bool
tileValidationSize isHorizontal (x, y) word
    | isHorizontal = (x <= 15 - (length word) && x >= 0) && (y >= 0 && y <= 14)
    | otherwise = (y <= 15 - (length word) && y >= 0) && (x >= 0 && x <= 14):

playerHasLetter :: Player -> Char -> Bool
playerHasLetter player letter = any (\l -> letter l == 'A') (pLetters player)


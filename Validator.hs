module Validator where

import MatchesController
import BoardController
import PlayerController
import Data.Char
{- 
    
initialValidation :: Match -> String -> Bool
initialValidation _ "" = False
initialValidation match linha
    | length palavras /= 3 = False
    | otherwise = 
        (coordValidation coord) && (tileValidation (mBoard Match) (x, y) direction word)
    where
        palavras = words $ map toUpper linha
        coord = (palavras !! 0)
        x = ord (coord !! 0) - ord 'A'
        y = read (dropWhile (== '0') coord) :: Int
        direction = (palavras !! 1)
        word = (palavras !! 2)

coordValidation :: String -> Bool
coordValidation coord
   | length coord /= 3 = False
   | otherwise = 
        let x = toUpper (coord !! 0)
            y = coord !! 1
            y' = coord !! 2
        in
            x `elem` ['A' .. 'O'] && (y == '0' && y' `elem` ['1' .. '9'] || y == '1' && y' `elem` ['0' .. '5'])

lettersValidation :: Char -> String -> Bool
lettersValidation direction word = (direction == "V" || direction == "H") && [] == [l | l <- word, l `notElem` ['A' .. 'Z']]

-- Recebe o board, o booleano que informa se eh horizontal, as coordenadas x e y (col, row) indexadas em zero, a palavra, e retorna se passou ou não
tileValidationSize :: Board -> Bool -> (Int, Int) -> String -> Bool
tileValidationSize b isHorizontal (x,y) word 
    | isHorizontal = (x <= 15 - (length word) && x >= 0) && (y >= 0 && y <= 14)
    | otherwise = (y <= 15 - (length word) && y >= 0) && (x >= 0 && x <= 14)

tileValidationLetters :: Board -> Bool -> (Int, Int) -> String -> Bool
tileValidationLetters _ _ _ _ = False

tileValidation :: Board -> (Int, Int) -> Bool -> String -> Bool
tileValidation b (x, y) isHorizontal word = (tileValidationSize b isHorizontal (x, y) word) && (tileValidationLetters b isHorizontal (x, y)  word)
 -}
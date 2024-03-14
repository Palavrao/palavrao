module Validator where

import Data.Char (toUpper)

coordValidation :: String -> Bool
coordValidation coord
   | length coord /= 3 = False
   | otherwise = 
        let x = toUpper (coord !! 0)
            y = coord !! 1
            y' = coord !! 2
        in
            x `elem` ['A' .. 'O'] && (y == '0' && y' `elem` ['1' .. '9'] || y == '1' && y' `elem` ['0' .. '5'])

lettersValidation :: String -> Bool
lettersValidation "" = False
lettersValidation word = [] == [l | l <- word, toUpper l `notElem` ['A' .. 'Z']]

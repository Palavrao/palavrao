{-# LANGUAGE DeriveGeneric #-}

module Controllers.BoardController where
        
import Text.Printf
import System.Console.ANSI
import Data.Char
import GHC.Generics
import Data.Aeson
import Controllers.LettersController as LC

data Board = Board {
    curTiles :: [[Char]],
    workTiles :: [[Char]]
} deriving (Show, Generic, Eq)

instance ToJSON Board
instance FromJSON Board

startBoard :: Board
startBoard = Board {
                curTiles= [
                        ['#', '~', '~', '*', '~', '~', '~', '#', '~', '~', '~', '*', '~', '~', '#'],
                        ['~', '-', '~', '~', '~', '!', '~', '~', '~', '!', '~', '~', '~', '-', '~'],
                        ['~', '~', '-', '~', '~', '~', '*', '~', '*', '~', '~', '~', '-', '~', '~'],
                        ['*', '~', '~', '-', '~', '~', '~', '*', '~', '~', '~', '-', '~', '~', '*'],
                        ['~', '~', '~', '~', '-', '~', '~', '~', '~', '~', 'S', '~', '~', '~', '~'],
                        ['~', '!', '~', '~', 'C', 'A', 'S', 'A', '~', 'S', 'A', '~', '~', '!', '~'],
                        ['~', '~', '*', '~', '~', 'V', '*', '~', '*', 'O', 'L', 'A', '*', '~', '~'],
                        ['#', '~', '~', '*', '~', 'E', 'S', 'C', 'O', 'L', 'A', '*', '~', '~', '#'],
                        ['~', '~', '*', '~', '~', '~', '*', '~', '*', '~', '~', '~', '*', '~', '~'],
                        ['~', '!', '~', '~', '~', '!', '~', '~', '~', '!', '~', '~', '~', '!', '~'],
                        ['~', '~', '~', '~', '-', '~', '~', '~', '~', '~', '-', '~', '~', '~', '~'],
                        ['*', '~', '~', '-', '~', '~', '~', '*', '~', '~', '~', '-', '~', '~', '*'],
                        ['~', '~', '-', '~', '~', '~', '*', '~', '*', '~', '~', '~', '-', '~', '~'],
                        ['~', '-', '~', '~', '~', '!', '~', '~', '~', '!', '~', '~', '~', '-', '~'],
                        ['#', '~', '~', '*', '~', '~', '~', '#', '~', '~', '~', '*', '~', '~', '#']]
                ,workTiles =[
                        ['#', '~', '~', '*', '~', '~', '~', '#', '~', '~', '~', '*', '~', '~', '#'],
                        ['~', '-', '~', '~', '~', '!', '~', '~', '~', '!', '~', '~', '~', '-', '~'],
                        ['~', '~', '-', '~', '~', '~', '*', '~', '*', '~', '~', '~', '-', '~', '~'],
                        ['*', '~', '~', '-', '~', '~', '~', '*', '~', '~', '~', '-', '~', '~', '*'],
                        ['~', '~', '~', '~', '-', '~', '~', '~', '~', '~', 'S', '~', '~', '~', '~'],
                        ['~', '!', '~', '~', 'C', 'A', 'S', 'A', '~', 'S', 'A', '~', '~', '!', '~'],
                        ['~', '~', '*', '~', '~', 'V', '*', '~', '*', 'O', 'L', 'A', '*', '~', '~'],
                        ['#', '~', '~', '*', '~', 'E', 'S', 'C', 'O', 'L', 'A', '*', '~', '~', '#'],
                        ['~', '~', '*', '~', '~', '~', '*', '~', '*', '~', '~', '~', '*', '~', '~'],
                        ['~', '!', '~', '~', '~', '!', '~', '~', '~', '!', '~', '~', '~', '!', '~'],
                        ['~', '~', '~', '~', '-', '~', '~', '~', '~', '~', '-', '~', '~', '~', '~'],
                        ['*', '~', '~', '-', '~', '~', '~', '*', '~', '~', '~', '-', '~', '~', '*'],
                        ['~', '~', '-', '~', '~', '~', '*', '~', '*', '~', '~', '~', '-', '~', '~'],
                        ['~', '-', '~', '~', '~', '!', '~', '~', '~', '!', '~', '~', '~', '-', '~'],
                        ['#', '~', '~', '*', '~', '~', '~', '#', '~', '~', '~', '*', '~', '~', '#']]}

toyBoard :: Board
toyBoard = Board {
                curTiles= [
                         ['#', '~', '~', '*', '~', '~', '~', '#', '~', '~', '~', '*', '~', '~', '#'],
                        ['~', '-', '~', '~', '~', '!', '~', '~', '~', '!', '~', '~', '~', '-', '~'],
                        ['~', '~', '-', '~', '~', '~', '*', '~', '*', '~', '~', '~', '-', '~', '~'],
                        ['*', '~', '~', '-', '~', '~', '~', '*', '~', '~', '~', '-', '~', '~', '*'],
                        ['~', '~', '~', '~', '-', '~', '~', '~', '~', '~', '-', '~', '~', '~', '~'],
                        ['~', '!', '~', '~', '~', '!', '~', '~', '~', '!', '~', '~', '~', '!', '~'],
                        ['~', '~', '*', '~', '~', '~', '*', '~', '*', '~', '~', '~', '*', '~', '~'],
                        ['#', '~', '~', '*', '~', '~', '~', '-', '~', '~', '~', '*', '~', '~', '#'],
                        ['~', '~', '*', '~', '~', '~', '*', '~', '*', '~', '~', '~', '*', '~', '~'],
                        ['~', '!', '~', '~', '~', '!', '~', '~', '~', '!', '~', '~', '~', '!', '~'],
                        ['~', '~', '~', '~', '-', '~', '~', '~', '~', '~', '-', '~', '~', '~', '~'],
                        ['*', '~', '~', '-', '~', '~', '~', '*', '~', '~', '~', '-', '~', '~', '*'],
                        ['~', '~', '-', '~', '~', '~', '*', '~', '*', '~', '~', '~', '-', '~', '~'],
                        ['~', '-', '~', '~', '~', '!', '~', '~', '~', '!', '~', '~', '~', '-', '~'],
                        ['#', '~', '~', '*', '~', '~', '~', '#', '~', '~', '~', '*', '~', '~', '#']]
                ,workTiles =[
                        ['#', '~', '~', '*', '~', '~', '~', '#', '~', '~', '~', '*', '~', '~', '#'],
                        ['~', '-', '~', '~', '~', '!', '~', '~', '~', '!', '~', '~', '~', '-', '~'],
                        ['~', '~', '-', '~', '~', '~', '*', '~', '*', '~', '~', '~', '-', '~', '~'],
                        ['*', '~', '~', '-', '~', '~', '~', '*', '~', '~', '~', '-', '~', '~', '*'],
                        ['~', '~', '~', '~', '-', '~', '~', '~', '~', '~', 'S', '~', '~', '~', '~'],
                        ['~', '!', '~', '~', 'C', 'A', 'S', 'A', '~', 'S', 'A', '~', '~', '!', '~'],
                        ['~', '~', '*', '~', '~', 'V', '*', '~', '*', 'O', 'L', 'A', '*', '~', '~'],
                        ['#', '~', '~', '*', '~', 'E', 'S', 'C', 'O', 'L', 'A', '*', '~', '~', '#'],
                        ['~', '~', '*', '~', '~', '~', '*', '~', '*', '~', '~', '~', '*', '~', '~'],
                        ['~', '!', '~', '~', '~', '!', '~', '~', '~', '!', '~', '~', '~', '!', '~'],
                        ['~', '~', '~', '~', '-', '~', '~', '~', '~', '~', '-', '~', '~', '~', '~'],
                        ['*', '~', '~', '-', '~', '~', '~', '*', '~', '~', '~', '-', '~', '~', '*'],
                        ['~', '~', '-', '~', '~', '~', '*', '~', '*', '~', '~', '~', '-', '~', '~'],
                        ['~', '-', '~', '~', '~', '!', '~', '~', '~', '!', '~', '~', '~', '-', '~'],
                        ['#', '~', '~', '*', '~', '~', '~', '#', '~', '~', '~', '*', '~', '~', '#']]}

updateBoard :: Board -> [[Char]] -> Board
updateBoard initial update = initial { workTiles=update}


_replacements :: Char -> Char
_replacements a
    |elem a ['A'..'Z'] = a
    |otherwise = ' '

replaceElement :: [[Char]] -> Int -> Int -> Char -> [[Char]]
replaceElement matrix y x c =
  take y matrix ++ [take x (matrix !! y) ++ [c] ++ drop (x + 1) (matrix !! y)] ++ drop (y + 1) matrix


_getCol :: [[Char]] -> Int -> [Char]
_getCol mat n = map (!!n) mat


getWords :: Board -> [String]
getWords board = (_search (!!) board 0) ++ (_search (_getCol) board 0)


_search :: ([[Char]]->Int->[Char]) -> Board -> Int -> [String]
_search _ _ 15 = []
_search func board n =
        (filter (\x -> length x > 1) (words $ map _replacements (func b n))) ++ _search (func) board (n+1)
        where b = workTiles board


placeWord :: (Int, Int, Bool, [Char]) -> Board -> Board
placeWord (x, y, isHorizontal, wrd) initialBoard
    | isHorizontal = updateBoard initialBoard (placeLetters True x y wrd (workTiles initialBoard))
    | otherwise = updateBoard initialBoard (placeLetters False x y wrd (workTiles initialBoard))

placeLetters :: Bool -> Int -> Int -> [Char] -> [[Char]] -> [[Char]]
placeLetters _ _ _ [] b = b

placeLetters True x y (h:t) b = placeLetters True (x+1) y t (replaceElement b y x h)

placeLetters False x y (h:t) b = placeLetters False x (y+1) t (replaceElement b y x h)


verifyWord :: [String] -> String -> Bool
verifyWord words word = elem word words




getPointsWord :: [Char] -> [Char]-> Int
getPointsWord tiles word = (wordBonuses tiles word) * (getPointsLetter tiles word) + (bingo tiles)

getPointsLetter :: [Char] -> [Char] -> Int
getPointsLetter [] [] = 0
getPointsLetter (hBoard:tBoard) (hWord:tWord) 
        | hBoard == '*' = 2 * (LC.letterValue hWord) -- azul dobra
        | hBoard == '!' = 3 * (LC.letterValue hWord) -- verde triplica
        | otherwise = LC.letterValue hWord

bingo :: [Char] -> Int
bingo tiles
        | playedLetters > 6 = 50
        |otherwise = 0
        where playedLetters = length [x | x <- tiles, (x `elem` ['A'..'Z']) == False]

wordBonuses :: [Char] -> [Char] -> Int
wordBonuses [] [] = 1
wordBonuses (hBoard:tBoard) (hWord:tWord) 
        | hBoard == '-' = 2 * (wordBonuses tBoard tWord)
        | hBoard == '#' = 3 * (wordBonuses tBoard tWord) -- verde triplica
        | otherwise = (wordBonuses tBoard tWord)


{- 
        | hBoard `elem` ['A'..'Z'] = (letterValue hWord)
        | hBoard == '#' = triplicaPalavra
        | hBoard == '-' = duplicaPalavra -}


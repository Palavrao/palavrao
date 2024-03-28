{-# LANGUAGE DeriveGeneric #-}
{-# HLINT ignore "Redundant bracket" #-}

module Controllers.BoardController where

import Text.Printf
import System.Console.ANSI
import Data.Char
import GHC.Generics
import Data.Aeson
import Controllers.LettersController as LC



-- Um Board é um tipo que guarda um tabuleiro. 
-- Para manter um board é necessário guardar duas matrizes: a do estado atual e a de trabalho
-- A de trabalho é usada para fazer verificações, somente quando todas as verificações passam a atual é substituída pela de trabalho
data Board = Board {
    curTiles :: [[Char]],
    workTiles :: [[Char]]
} deriving (Show, Generic, Eq)

instance ToJSON Board
instance FromJSON Board


-- Inicializa um board com as tiles originais, cada símbolo representa uma modificação de pontuação que pode ser 
-- aplicada nas letras e palavras que ficarem sobre ele
startBoard :: Board
startBoard = Board {
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
                        ['#', '~', '~', '*', '~', '~', '~', '#', '~', '~', '~', '*', '~', '~', '#']]}


-- Recebe: um Board com as workTiles modificadas
-- Retorna: um novo board com as curTiles atualizadas para serem iguais às workTiles
updateBoard :: Board -> Board
updateBoard initial = initial { curTiles=workTiles initial}



-- Recebe: um board inicial
-- Recebe: uma matriz de Char
-- Retorna: um board onde as workTiles foram substituídas pela matriz de char
updateBoardWork :: Board -> [[Char]] -> Board
updateBoardWork initial update = initial {workTiles=update}

-- Recebe: Um char
-- Retorna: ' ' se não for uma letra, e a letra caso seja
_replacements :: Char -> Char
_replacements a
    |a `elem` ['A'..'Z'] = a
    |otherwise = ' '


-- Recebe: uma matrix
-- Recebe: coordenadas x e y
-- Recebe: um caracter
-- Retorna: a matrix com o elemento na coordenada trocado pelo caracter passado
replaceElement :: [[Char]] -> Int -> Int -> Char -> [[Char]]
replaceElement matrix y x c =
  take y matrix ++ [take x (matrix !! y) ++ [c] ++ drop (x + 1) (matrix !! y)] ++ drop (y + 1) matrix


-- Recebe: uma matriz
-- Recebe: uma coordenada x
-- Retorna: a coluna X daquela matriz
_getCol :: [[Char]] -> Int -> [Char]
_getCol mat x = map (!!x) mat


-- Recebe: um board
-- Retorna: as palavras do board, obtidas linha a linha e coluna a coluna
getWords :: Board -> [String]
getWords board = (_search (!!) board 0) ++ (_search (_getCol) board 0)


-- Recebe: uma função que recebe uma matriz e uma coordenada e retorna uma lista de caracteres
-- Recebe: um board
-- Recebe: uma coordenada (linha ou coluna)
-- Retorna: as palavras presentes na linha ou coluna obtida através da função passada como argumento
_search :: ([[Char]]->Int->[Char]) -> Board -> Int -> [String]
_search _ _ 15 = []
_search func board n =
        (filter (\x -> length x > 1) (words $ map _replacements (func b n))) ++ _search (func) board (n+1)
        where b = workTiles board


-- Recebe: (coordenada x, coordenada y, booleano que informa se é vertical ou horizontal, palavra)
-- Recebe: um board
-- Retorna: O board com a letra nas coordenadas e posição especificadas (chama a função que coloca letras recursivamente)
placeWord :: (Int, Int, Bool, [Char]) -> Board -> Board
placeWord (x, y, isHorizontal, wrd) initialBoard
    | isHorizontal = updateBoardWork initialBoard (placeLetters True x y wrd (workTiles initialBoard))
    | otherwise = updateBoardWork initialBoard (placeLetters False x y wrd (workTiles initialBoard))


-- Recebe: um booleano que informa se é vertical ou horizontal
-- Recebe: coordenadas x e y
-- Recebe: uma palavra
-- Recebe: uma matriz de char
-- Retorna: A matriz com a palavra no local especificado (coloca cada letra recursivamente)
placeLetters :: Bool -> Int -> Int -> [Char] -> [[Char]] -> [[Char]]
placeLetters _ _ _ [] b = b
placeLetters True x y (h:t) b = placeLetters True (x+1) y t (replaceElement b y x h)
placeLetters False x y (h:t) b = placeLetters False x (y+1) t (replaceElement b y x h)


-- Recebe: uma lista de palavras
-- Recebe: uma palavra
-- Retorna: True se a palavra está na lista
verifyWord :: [String] -> String -> Bool
verifyWord words word = word `elem` words


-- Recebe: uma palavra
-- Recebe: as tiles no local que a palavra vai ocupar no board
-- Retorna: os pontos da palavra
getPointsWord :: [Char] -> [Char]-> Int
getPointsWord tiles word = (wordBonuses tiles word) * (getPointsLetter tiles word) + (bingo tiles)


-- Recebe: uma letra
-- Recebe: um caracter especial
-- Retorna: os pontos obtidos aplicando o bonus do caracter especial sobre a letra
getPointsLetter :: [Char] -> [Char] -> Int
getPointsLetter [] [] = 0
getPointsLetter (hBoard:tBoard) (hWord:tWord)
        | hBoard == '*' = 2 * (LC.letterValue hWord) + getPointsLetter tBoard tWord-- azul dobra
        | hBoard == '!' = 3 * (LC.letterValue hWord) + getPointsLetter tBoard tWord-- verde triplica
        | otherwise = LC.letterValue hWord + getPointsLetter tBoard tWord


-- Recebe: As tiles na posição em que ficará uma palavra
-- Retorna: 50 se entre as tiles há ao menos 7 símbolos especiais, indicando que o jogador usou 7 letras próprias
bingo :: [Char] -> Int
bingo tiles
        | playedLetters > 6 = 50
        |otherwise = 0
        where playedLetters = length [x | x <- tiles, x `notElem` ['A'..'Z']]


-- Recebe: uma palavra
-- Recebe: as tiles no lugar que a palavra vai ocupar no tabuleiro
-- Retorna: os bonuses decorrentes de caracteres especiais aplicados à palavra
wordBonuses :: [Char] -> [Char] -> Int
wordBonuses [] [] = 1
wordBonuses (hBoard:tBoard) (hWord:tWord)
        | hBoard == '-' = 2 * (wordBonuses tBoard tWord) -- rosa dobra
        | hBoard == '#' = 3 * (wordBonuses tBoard tWord) -- vermelho triplica
        | otherwise = (wordBonuses tBoard tWord)





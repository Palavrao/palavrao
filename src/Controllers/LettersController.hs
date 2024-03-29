{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Controllers.LettersController where

import Data.Aeson
import GHC.Generics
import Data.Char

data Letter = Letter {
    letter :: Char,
    letterScore :: Int
} deriving (Show, Generic, Eq)

instance ToJSON Letter
instance FromJSON Letter



-- Retorna as letras iniciais de uma partida
-- Retorna: todas as letras 120 letras iniciais
startLetters :: [Letter] 
startLetters = [
        Letter {letter='<', letterScore=0},
        Letter {letter='<', letterScore=0},
        Letter {letter='<', letterScore=0},
        Letter {letter='A', letterScore=1},
        Letter {letter='A', letterScore=1},
        Letter {letter='A', letterScore=1},
        Letter {letter='A', letterScore=1},
        Letter {letter='A', letterScore=1},
        Letter {letter='A', letterScore=1},
        Letter {letter='A', letterScore=1},
        Letter {letter='A', letterScore=1},
        Letter {letter='A', letterScore=1},
        Letter {letter='A', letterScore=1},
        Letter {letter='A', letterScore=1},
        Letter {letter='A', letterScore=1},
        Letter {letter='A', letterScore=1},
        Letter {letter='A', letterScore=1},
        Letter {letter='E', letterScore=1},
        Letter {letter='E', letterScore=1},
        Letter {letter='E', letterScore=1},
        Letter {letter='E', letterScore=1},
        Letter {letter='E', letterScore=1},
        Letter {letter='E', letterScore=1},
        Letter {letter='E', letterScore=1},
        Letter {letter='E', letterScore=1},
        Letter {letter='E', letterScore=1},
        Letter {letter='E', letterScore=1},
        Letter {letter='E', letterScore=1},
        Letter {letter='I', letterScore=1},
        Letter {letter='I', letterScore=1},
        Letter {letter='I', letterScore=1},
        Letter {letter='I', letterScore=1},
        Letter {letter='I', letterScore=1},
        Letter {letter='I', letterScore=1},
        Letter {letter='I', letterScore=1},
        Letter {letter='I', letterScore=1},
        Letter {letter='I', letterScore=1},
        Letter {letter='I', letterScore=1},
        Letter {letter='O', letterScore=1},
        Letter {letter='O', letterScore=1},
        Letter {letter='O', letterScore=1},
        Letter {letter='O', letterScore=1},
        Letter {letter='O', letterScore=1},
        Letter {letter='O', letterScore=1},
        Letter {letter='O', letterScore=1},
        Letter {letter='O', letterScore=1},
        Letter {letter='O', letterScore=1},
        Letter {letter='O', letterScore=1},
        Letter {letter='S', letterScore=1},
        Letter {letter='S', letterScore=1},
        Letter {letter='S', letterScore=1},
        Letter {letter='S', letterScore=1},
        Letter {letter='S', letterScore=1},
        Letter {letter='S', letterScore=1},
        Letter {letter='S', letterScore=1},
        Letter {letter='S', letterScore=1},
        Letter {letter='U', letterScore=1},
        Letter {letter='U', letterScore=1},
        Letter {letter='U', letterScore=1},
        Letter {letter='U', letterScore=1},
        Letter {letter='U', letterScore=1},
        Letter {letter='U', letterScore=1},
        Letter {letter='U', letterScore=1},
        Letter {letter='M', letterScore=1},
        Letter {letter='M', letterScore=1},
        Letter {letter='M', letterScore=1},
        Letter {letter='M', letterScore=1},
        Letter {letter='M', letterScore=1},
        Letter {letter='M', letterScore=1},
        Letter {letter='R', letterScore=1},
        Letter {letter='R', letterScore=1},
        Letter {letter='R', letterScore=1},
        Letter {letter='R', letterScore=1},
        Letter {letter='R', letterScore=1},
        Letter {letter='R', letterScore=1},
        Letter {letter='T', letterScore=1},
        Letter {letter='T', letterScore=1},
        Letter {letter='T', letterScore=1},
        Letter {letter='T', letterScore=1},
        Letter {letter='T', letterScore=1},
        Letter {letter='D', letterScore=2},
        Letter {letter='D', letterScore=2},
        Letter {letter='D', letterScore=2},
        Letter {letter='D', letterScore=2},
        Letter {letter='D', letterScore=2},
        Letter {letter='L', letterScore=2},
        Letter {letter='L', letterScore=2},
        Letter {letter='L', letterScore=2},
        Letter {letter='L', letterScore=2},
        Letter {letter='L', letterScore=2},
        Letter {letter='C', letterScore=2},
        Letter {letter='C', letterScore=2},
        Letter {letter='C', letterScore=2},
        Letter {letter='C', letterScore=2},
        Letter {letter='C', letterScore=2},
        Letter {letter='C', letterScore=2},
        Letter {letter='P', letterScore=2},
        Letter {letter='P', letterScore=2},
        Letter {letter='P', letterScore=2},
        Letter {letter='P', letterScore=2},
        Letter {letter='N', letterScore=3},
        Letter {letter='N', letterScore=3},
        Letter {letter='N', letterScore=3},
        Letter {letter='N', letterScore=3},
        Letter {letter='B', letterScore=3},
        Letter {letter='B', letterScore=3},
        Letter {letter='B', letterScore=3},
        Letter {letter='F', letterScore=4},
        Letter {letter='F', letterScore=4},
        Letter {letter='G', letterScore=4},
        Letter {letter='G', letterScore=4},
        Letter {letter='H', letterScore=4},
        Letter {letter='H', letterScore=4},
        Letter {letter='V', letterScore=4},
        Letter {letter='V', letterScore=4},
        Letter {letter='J', letterScore=5},
        Letter {letter='J', letterScore=5},
        Letter {letter='Q', letterScore=6},
        Letter {letter='X', letterScore=7},
        Letter {letter='Z', letterScore=7}
    ]


-- Mapeia cada caractere em um score
-- Recebe: caractere que terá sua pontuacao retornada
-- Retorna: valor da pontuacao do caractere recebido
letterValue :: Char -> Int
letterValue i
    | c == 'A' = 1
    | c == 'E' = 1
    | c == 'I' = 1
    | c == 'O' = 1
    | c == 'S' = 1
    | c == 'U' = 1
    | c == 'M' = 1
    | c == 'R' = 1
    | c == 'T' = 1
    | c == 'D' = 2
    | c == 'L' = 2
    | c == 'C' = 2
    | c == 'P' = 2
    | c == 'N' = 3
    | c == 'B' = 3
    | c == 'F' = 4
    | c == 'G' = 4
    | c == 'H' = 4
    | c == 'V' = 4
    | c == 'J' = 5
    | c == 'Q' = 6
    | c == 'X' = 7
    | c == 'Z' = 7
    | otherwise = 0
    where c  = toUpper i


-- Recebe um array de caracteres e os retorna com suas pontuacoes
-- Recebe: array de caracteres que será transformado em letter
-- Retorna: array de letter possuindo os caracteres recebidos e suas pontuacoes
getLetterArray :: [Char] -> [Letter]
getLetterArray characters = map (\c -> Letter { letter=toUpper c, letterScore=letterValue c}) characters


-- Recebe um array de letters e retorna a soma de suas pontuações
-- Recebe: um array de Letters
-- Retorna: a soma de suas pontuações
getLetterSum :: [Letter] -> Int
getLetterSum [] = 0
getLetterSum (l:tail) = (letterScore l) + getLetterSum tail
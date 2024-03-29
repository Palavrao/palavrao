{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Controllers.PlayerController where

import Utils.Utils as UT
import GHC.Generics
import Data.Aeson
import Controllers.AccountsController
import Controllers.LettersController



-- Um player é uma entidade que existe durante uma partida, ele deve ter associado a ele:
-- Uma conta
-- Um conjunto de até 7 letras
-- Um score
data Player = Player {
    pAcc :: Account,
    pLetters :: [Letter],
    pScore :: Int
} deriving (Show, Generic, Eq)

instance ToJSON Player
instance FromJSON Player


-- Recebe: uma Account a ser associada ao player
-- Retorna: um player cujo dono é a account passada, sem letras e com o score zerado
createPlayer :: Account -> Player
createPlayer acc = Player {
        pAcc = acc,
        pLetters = [],
        pScore = 0
    }


-- Recebe: um player
-- Recebe: array de Letters
-- Retorna: um player com o conjunto de letras substituído pelo conjunto de letras passado
updateLetters :: Player -> [Letter] -> Player
updateLetters player newLetters = player {pLetters = newLetters}


-- Recebe: um player
-- Recebe: Um array de Letters
-- Retorna: o player com as letters adicionadas ao seu conjunto de letras
addLetters :: Player -> [Letter] -> Player
addLetters player newLetters = player {pLetters = (pLetters player) ++ newLetters}


-- Recebe: um player
-- Recebe: um valor de score
-- Retorna: um player com a pontuação incrementada do valor recebido
incScore :: Player -> Int -> Player
incScore player score = player {pScore = (pScore player + score)}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use null" #-}
module Utils.Validator where

import Controllers.MatchesController
import Controllers.BoardController
import Controllers.PlayerController
import Controllers.AccountsController
import Data.Char
import Controllers.LettersController
import Utils.Utils as UT

-- Encapsula lógica de validação
-- Recebe: uma match, a lista de palavras do portuguÊs, uma string de input
-- Retorna: um a tupla com: (True se a coordenada e posicionamento forem válidos,
--                            A string de palavras inválidas,
--                            A quantidade de pontos obtidos,
--                            String de letras inválidas,
--                            String de letras usadas pelo jogador)
initialValidation :: Match -> [String] -> String -> (Bool, [String], Int, [Char], [Char])
initialValidation _ _ "" = (False, [], 0, [], [])
initialValidation match wordlist linha
    | length palavras /= 3 = (False, [], 0, [], []) -- Input completamente errado
    | otherwise = (resCoordenadas, resPalavras, getPointsWord letrasNoBoard word, invalidLetters, letrasUsadas)
    where
        (x, y, isHorizontal, word) = readWordInput linha match
        palavras = words $ map toUpper linha
        coord = (head palavras)
        playerOnTurn = getPlayerOnTurn match
        playerLetters = [letter l | l <- pLetters playerOnTurn]
        letrasNoBoard = _takeUpTo isHorizontal match (x,y) (length word)
        letterUsageReport = _wordLetterReport playerLetters letrasNoBoard word
        invalidLetters = _lettersMissing word letterUsageReport
        letrasUsadas = [l | l <- letterUsageReport, l /= '!', l /= ' ']
        estaConectado = 0 /= (length [x | x <- letrasNoBoard, x `elem` ['A'..'Z']])
        centroLivre = (((curTiles (mBoard match)) !! 7) !! 7) == '-'
        resCoordenadas = (((palavras !! 1) `elem` ["V", "H"]) 
                            && (_coordValidation coord) 
                            && (_wordHasOnlyLettersAndAddsNewLetters word letrasNoBoard) 
                            && (_wordFitsInSpace isHorizontal (x, y) word) 
                            && (_tileValidationDifferentLettersDontOverlap letrasNoBoard word) 
                            && (estaConectado || centroLivre) 
                            && (_coordCenterValidation match isHorizontal (x,y) word))
        resPalavras = (_allWordsExist match wordlist (getWords (placeWord (x,y,isHorizontal,word) (mBoard match))))


-- Recebe: uma match
-- Retorna: o jogador da vez
getPlayerOnTurn :: Match -> Player
getPlayerOnTurn match
    | mTurn match = mP2 match
    | otherwise = mP1 match


-- Recebe: uma match, o booleano que informa se é horizontal, as coordenadas x e y, a palavra
-- Retorna: True se for a primeira palavra deve ocupar o centro, False se não
_coordCenterValidation :: Match -> Bool -> (Int, Int) -> String -> Bool
_coordCenterValidation match isHorizontal (x,y) word
    | centerValue /= '-' = True -- Centro já estava ocupado: não é a primeira palavra
    | isHorizontal = (y == 7) && (x <= 7 && 7 <= finalXInd)
    | otherwise = (x == 7) && (y <= 7 && 7 <= finalYInd)
    where
        finalXInd = x + (length word) - 1
        finalYInd = y + (length word) - 1
        matrix = curTiles (mBoard match)
        centerValue = (matrix !! 7) !! 7


-- Recebe: uma match, a lista de palavras do português, a palvra a ser testada
-- Retorna: True se uma palavra está no arquivo de palavras, False se não
_wordExistenceValidation :: Match -> [String] -> String -> Bool
_wordExistenceValidation match wordList word = ((map toLower word) `elem` (mUsedWords match)) || ((map toLower word) `elem` wordList)


-- Verifica que todas as palavras formadas no tabuleiro existem
-- Recebe: uma match, a lista de todas as palavras do português, a lista de palavras a testar
_allWordsExist :: Match -> [String] -> [String] -> [String]
_allWordsExist match wordlist wordsToTest = [x | x <- wordsToTest, not (_wordExistenceValidation match wordlist x)]


-- Verifica que a coordenada passada é válida: o primeiro caracter é uma letra de A a O e a segunda é uma string com um int de 0 a 14
-- Recebe: "coord", a primeira palavra do input
_coordValidation :: [Char] -> Bool
_coordValidation (x:y) = (x `elem` ['A' .. 'O']) && (UT.isStringInt y) && ((read y :: Int) >= 0 && (read y :: Int) <= 14)


-- Verifica que a palavra jogada pelo jogador é composta apenas de letras e que ela adiciona alguma letra nova ao board
-- Recebe: palavra, letras na posição que ela vai ocupar
-- Retorna: se é uma posição válida
_wordHasOnlyLettersAndAddsNewLetters :: String -> String -> Bool
_wordHasOnlyLettersAndAddsNewLetters word letrasNoBoard = ([] == [l | l <- word, not (isLetter l)]) && (length word /= length [l | l <- letrasNoBoard, isLetter l])


-- Verifica que não haverá sobreposição de letras: o jogador não vai colocar uma letra diferente em um espaço já ocupado por outra
-- Recebe: as letras/simbolos do board na posição que será ocupada, a palavra
-- Retorna: falso se a uma letra das tiles for diferente de uma letra na palavra na mesma posição
_tileValidationDifferentLettersDontOverlap :: String -> String -> Bool
_tileValidationDifferentLettersDontOverlap [] [] = True
_tileValidationDifferentLettersDontOverlap (tileHead:tileTail) (wordHead:wordTail)
    | (tileHead `elem` ['A'..'Z']) && (tileHead /= wordHead) = False
    | otherwise = _tileValidationDifferentLettersDontOverlap tileTail wordTail


-- Recebe: o booleano que informa se eh horizontal, as coordenadas x e y (col, row) indexadas em zero, e a palavra
-- Retorna: True se a palavra cabe no espaço disponível no board, False se não
_wordFitsInSpace :: Bool -> (Int, Int) -> String -> Bool
_wordFitsInSpace isHorizontal (x, y) word
    | isHorizontal = (x <= 15 - (length word) && x >= 0) && (y >= 0 && y <= 14)
    | otherwise = (y <= 15 - (length word) && y >= 0) && (x >= 0 && x <= 14)


-- A função gera um relatório que traz informações sobre as letras usadas
-- Recebe: letras (chars) do jogador, tiles do board na posição que será ocupada, palavra que será colocada
-- Retorna: [Char] que traz informações sobre quais letras do player foram consumidas, quais estavam no board, e quais o jogador não tinha
{-  
    Exemplo de output:

    playerLetters = [A C E <] inicialmente

    PALAVRA: [A B C D E F] tinha A e D no board, não usou do player -> não tinha B e o player não tinha mas tinha curinga, usa o curinga -> ...
   NO BOARD: [A - - D - -] ... não tinha C e E o player tinha, usa C e E -> Não tinha F no board nem o player tinha curinga, vai exclamação
     OUTPUT: [  < C   E !]

     playerLetters = [A] final

     Assim, para saber o que o player usou olha os caracteres e pra saber se passou na validação olha se tem exclamação
     As posições estão sendo respeitadas, então pra saber qual letra faltou basta olhar qual é a letra da palavra na posição exclamação.
 -}
_wordLetterReport :: [Char] -> [Char] -> [Char] -> [Char]
_wordLetterReport _ _ [] = []
_wordLetterReport playerLetters (t:tileTail) (w:wordTail) -- (letrasNoBoard) (word)
    | w == t = ' ':(_wordLetterReport playerLetters tileTail wordTail)
    | w `elem` playerLetters = w:(_wordLetterReport (_removeChar w playerLetters) tileTail wordTail)
    | '<' `elem` playerLetters = '<':(_wordLetterReport (_removeChar '<' playerLetters) tileTail wordTail)
    | otherwise = '!':(_wordLetterReport playerLetters tileTail wordTail)


-- Recebe: uma Match e uma letter, obtém o jogador a partir da match
-- Retorna: True se o jogador tem a Letter passada, false se não 
playerHasLetter :: Match -> Letter -> Bool
playerHasLetter match letter = letter `elem` (pLetters player)
    where player = getPlayerOnTurn match


-- Recebe: palavra a ser testada, string do _wordLetterReport
-- Retorna: as letras que o jogador não tinha para completar a palavra
_lettersMissing :: [Char] -> [Char] -> [Char]
_lettersMissing word filterString =
    map fst $ filter (\(_, el) -> el == '!') $ zip word filterString


-- Recebe: um input em string, uma match
-- Retorna: extrai as informações necessárias para validar o input e retorna as informações contidas na string
readWordInput :: String -> Match -> (Int, Int, Bool, String)
readWordInput linha match = (x, y, isHorizontal, word)
    where
        palavras = words $ map toUpper linha
        coord = (head palavras)
        isHorizontal = (palavras !! 1) == "H"
        word = (palavras !! 2)
        x = ord (head coord ) - ord 'A'
        y = (read (tail coord) :: Int)
        letrasNoBoard = _takeUpTo isHorizontal match (x,y) (length word)


-- Verifica se uma conta existe baseado no nome
-- Recebe: nome da conta
-- Retorna: True se a conta existir, False se não
accExistsValidation :: String -> IO(Bool)
accExistsValidation accName = do
    acc <- getAccByName accName
    case acc of
        Just acc -> return True
        Nothing -> return False


-- Verifica se uma conta existe baseado no nome
-- Recebe: nome da conta
-- Retorna: True se a conta existir, False se não
matchExistsValidation :: String -> IO(Bool)
matchExistsValidation matchName = do
    match <- getMatchByName matchName
    case match of
        Just match -> return True
        Nothing    -> return False


-- Obtém as tiles presentes no board no intervalo especificado
-- Recebe: o booleano se informa se é horizontal, uma match, coordenadas x e y, o tamanho do intervalo
-- Retorna: as tiles no intervalo especificado
_takeUpTo :: Bool -> Match -> (Int, Int) -> Int -> [Char]
_takeUpTo isHorizontal match (x, y) len
    | isHorizontal = (take len (drop x (b !! y)))
    | otherwise = take len $ map (!! x) $ drop y b
    where b = curTiles (mBoard match)
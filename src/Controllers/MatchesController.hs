{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Controllers.MatchesController where

import GHC.Generics
import Data.Aeson
import Controllers.BoardController;
import Controllers.AccountsController
import Controllers.PlayerController
import Controllers.LettersController
import Utils.Utils as UT
import Data.Time.Clock
import System.Console.ANSI



-- Uma partida é uma entidade que armazena todos os dados de um jogo, ele deve ter associado a ele:
-- Um nome de conta
-- Um tabuleiro onde será trabalhada a lógica de disposição, validação e pontuação de palavras
-- Um booleano que representará de quem é a vez de jogar, caso false, player 1, caso contrário, player 2
-- O jogador 1 que jogará na partida
-- O jogador 2 que jogará na partida
-- As letras que poderão ser distribuídas para os jogadores e que se acabarem acaba o jogo
-- As palavras que já foram dispostas no tabuleiro
-- O tempo que o jogador tem para terminar a jogada antes de passar para o próximo jogador
-- A quantidade de jogadas puladas no jogo, com limite de 4 até o jogo acabar
data Match = Match {
    mName :: String,
    mBoard :: Board,
    mTurn :: Bool,
    mP1 :: Player,
    mP2 :: Player,
    mLetters :: [Letter],
    mUsedWords :: [String],
    mTimer :: Float,
    mSkips :: Int
} deriving (Show, Generic, Eq)

instance ToJSON Match
instance FromJSON Match


-- Constante do path para as matches.json
-- Retorna: path do matches.json
matchesPath :: String
matchesPath = "data/matches.json"


-- Salva uma partida no json
-- Recebe: partida que será salva no json
saveMatchJson :: Match -> IO()
saveMatchJson match = UT.incJsonFile match matchesPath


-- Deleta uma partida do json
-- Recebe: partida que será deletada no json
deleteMatchFromJson :: String -> IO()
deleteMatchFromJson name = do
    match <- getMatchByName name
    UT.deleteFromJson match matchesPath


-- Atualiza uma partida do json
-- Recebe: partida que será atualizada no json
updateMatchJson :: Match -> IO()
updateMatchJson updatedMatch = do
    accs <- getMatches
    let updatedAccs = _getUpdatedMatches accs targetMatchName updatedMatch
    UT.writeJsonFile updatedAccs matchesPath
    where
        targetMatchName = mName updatedMatch


-- Cria uma partida e a retorna, adicionando-a no json
-- Recebe: nome da partida que será criada
-- Recebe: conta do player 1 que jogará a partida
-- Recebe: conta do player 2 que jogará a partida
-- Retorna: partida criada com as configuracoes iniciais
createMatch :: String -> Account -> Account -> IO Match
createMatch name acc1 acc2 = do
    let match = Match {
            mName = name,
            mBoard = startBoard,
            mTurn = False,
            mP1 = createPlayer acc1,
            mP2 = createPlayer acc2,
            mLetters = startLetters,
            mUsedWords = [],
            mTimer = 300,
            mSkips = 0
        }

    matchP1Letters <- updatePlayerLetters match
    let matchP2 = toggleMatchTurn matchP1Letters
    matchP2Letters <- updatePlayerLetters matchP2
    let initMatch = toggleMatchTurn matchP2Letters

    saveMatchJson initMatch
    return initMatch


-- Finaliza uma partida, apagando-a do json, atualizando a pontuacao das contas que jogaram
-- e retornando os status final da partida
-- Recebe: partida que será finalizada
-- Retorna: partida finalizada com seus status finais
finishMatch :: Match -> IO Match
finishMatch match = do
    incAccScore (accName (pAcc p1)) (pScore p1)
    incAccScore (accName (pAcc p2)) (pScore p2)
    deleteMatchFromJson (mName match)
    return match
    where
        p1 = mP1 match
        p2 = mP2 match


-- Atualiza o timer da partida
-- Recebe: partida que terá o timer atualizado
-- Recebe: tempo que o timer da partida terá
-- Retorna: partida com timer atualizado
updateMatchTimer :: Match -> Float -> Match
updateMatchTimer match time = match { mTimer = time }


-- Verifica se uma partida existe no json
-- Recebe: nome da partida que será verificada se existe
-- Retorna: bool que indicará se a partida existe ou nao 
matchExists :: String -> IO Bool
matchExists name = do
    maybeMatch <- getMatchByName name
    return $ case maybeMatch of
        Nothing -> False
        Just _ -> True


-- Retorna todas as partidas salvas no json
-- Retorna: array das partidas salvas no json
getMatches :: IO [Match]
getMatches = do
    UT.readJsonFile matchesPath


-- Retorna uma partida salva no json, procurando-a pelo nome
-- Recebe: nome da partida que será retornada
-- Retorna: partida que está armazenada no json, ou nada se a partida nao existir
getMatchByName :: String -> IO (Maybe Match)
getMatchByName targetName = do
    matches <- getMatches
    return $ UT.getObjByField matches mName targetName



-- Incrementa o score do player que está no turno
-- Recebe: partida que terá o player que terá score incrementado
-- Recebe: score que será incrementado no score do player
-- Retorna: partida com o score do player atualizado
incPlayerScore :: Match -> Int -> Match
incPlayerScore match score
    | mTurn match = match {mP2 = incScore (mP2 match) score}
    | otherwise = match {mP1 = incScore (mP1 match) score}


-- Atualiza as letras do player até ele ter 7 letras no máximo
-- Recebe: partida que terá as letras que serão dadas ao player
-- Retorna: partida com as letras do player e dela própria atualizadas
updatePlayerLetters :: Match -> IO Match
updatePlayerLetters match = do
    (playerLetters, updatedLetters) <- UT.popRandomElements (mLetters match) (7 - (length (pLetters player)))

    let updatedMatch = _updateMatchLetters match updatedLetters
    let updatedPlayer = addLetters player playerLetters

    return $ _updateMatchPlayer updatedMatch updatedPlayer
        where
            player
                | mTurn match = mP2 match
                | otherwise = mP1 match


-- Retorna o player com maior pontuação na partida
-- Recebe: partida que terá os players que terão o score comparados
-- Retorna: player com maior pontuação na partida
getBestPlayer :: Match -> Player
getBestPlayer match
    | p1Score > p2Score = p1
    | p1Score < p2Score = p2
    | p1Score == p2Score && p1RemainingScore < p2RemainingScore = p1
    | p1Score == p2Score && p2RemainingScore > p1RemainingScore = p2
    | otherwise = Player{pAcc = Account{accName = "Empate :D"}, pScore = p1Score}
    where 
        p1 = mP1 match
        p2 = mP2 match
        p1Score = pScore p1
        p2Score = pScore p2
        p1RemainingScore = getLetterSum $ pLetters p1
        p2RemainingScore = getLetterSum $ pLetters p2


-- Muda o turno da partida e soma 1 skip na contagem de skips da partida
-- Recebe: partida que terá o turno skipado
-- Retorna: partida com um skip a mais e rodada pro outro player
skipPlayerTurn :: Match -> Match
skipPlayerTurn match = (toggleMatchTurn match) {mSkips = mSkips match + 1}


-- Reseta a quantidade de skips da partida para 0
-- Recebe: partida que terá os skips resetados
-- Retorna: partida com os skips resetados para 0
resetMatchSkipsQtd :: Match -> Match
resetMatchSkipsQtd match = match{mSkips = 0}


-- Muda o turno da partida para o próximo player 
-- Recebe: partida que terá o turno mudado
-- Retorna: partida com o turno mudado
toggleMatchTurn :: Match -> Match
toggleMatchTurn match = match {mTurn = not (mTurn match), mTimer = 300}


-- Atualiza o tabuleiro da partida
-- Recebe: partida que terá o tabuleiro mudado
-- Recebe: tabuleiro atualizado que a partida possuirá
-- Retorna: partida com o tabuleiro atualizado
updateMatchBoard :: Match -> Board -> Match
updateMatchBoard match newBoard = match {mBoard = newBoard}


-- Atualiza as palavras usadas na partida
-- Recebe: partida que terá as palavras utilizadas atualizadas
-- Recebe: array de palavras que foram utilizadas na jogada
-- Retorna: partida com as palavras utilizadas atualizadas
updateMUsedWords :: Match -> [String] -> Match
updateMUsedWords match words = match {mUsedWords = words ++ mUsedWords match}


-- Troca uma letra selecionada pelo jogador por uma aleatória armazenada pela partida
-- Recebe: partida que terá as letras do player e dela mesma atualizadas
-- Recebe: letra que o jogador vai trocar com a partida
-- Retorna: partida com as letras do player e dela mesma atualizadas
switchPlayerLetter :: Match -> Letter -> IO Match
switchPlayerLetter match letter = do
    (newLetter, updatedLetters) <- UT.popRandomElements (mLetters match) 1

    let playerLetters = UT.removeOneElement (pLetters player) letter
    let updatedPlayer = updateLetters player playerLetters 

    let updatedMatch = _updateMatchLetters match (letter:updatedLetters)
    let updatedPlayer' = addLetters updatedPlayer newLetter

    return $ _updateMatchPlayer updatedMatch updatedPlayer'
    where
        player
            | mTurn match = mP2 match
            | otherwise = mP1 match


-- Remove as letras do jogador do turno, letras essas passadas na função
-- Recebe: partida que terá o player que terá as letras removidas
-- Recebe: array de letras que serão removidas do jogador
-- Retorna: partida com o player com as letras passadas removidas
removePlayerLetters :: Match -> [Char] -> Match
removePlayerLetters match toRemove = _updateMatchPlayer match (updateLetters player newLetters)
    where 
        newLetters = getLetterArray (UT.removeChars toRemove [letter l | l <- (pLetters player)])
        player
            | mTurn match = mP2 match
            | otherwise = mP1 match


-- Função interna que fará a atualização do player da vez
-- Recebe: partida que terá o player atualizado
-- Recebe: player atualizado que será passado para a partida
-- Retorna: partida com player atualizado
_updateMatchPlayer :: Match -> Player -> Match
_updateMatchPlayer match player
    | mTurn match = match {mP2 = player}
    | otherwise = match {mP1 = player}


-- Função interna que retorna um array das partidas atualizadas
-- Recebe: array de partidas 
-- Recebe: nome da partida que será atualizada
-- Recebe: partida atualizada que substituirá a desatualizada
-- Retorna: array de partidas atualizadas
_getUpdatedMatches :: [Match] -> String -> Match -> [Match]
_getUpdatedMatches [] _ _ = []
_getUpdatedMatches (match:tail) targetMatchName updatedMatch
    | mName match == targetMatchName = (updatedMatch:tail)
    | otherwise = (match:_getUpdatedMatches tail targetMatchName updatedMatch)


-- Função interna que atualiza as letras que a partida possui
-- Recebe: partida que terá as letras atualizadas
-- Recebe: letras que serão postas na partida atualizada
-- Retorna: partida com as letras atualizadas
_updateMatchLetters :: Match -> [Letter] -> Match
_updateMatchLetters match newLetters = match {mLetters = newLetters}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Controllers.AccountsController where

import GHC.Generics
import Data.Aeson
import Utils.Utils as UT



-- Uma conta é uma entidade que é armazenada e utilizada para ranking, e associar a players, ela deve ter associado a ela:
-- Um nome
-- Um score representando a pontuação de todas as partidas jogadas pela conta
data Account = Account {
    accName :: String,
    accScore :: Int
} deriving (Show, Generic, Eq)

instance ToJSON Account
instance FromJSON Account


-- Constante do path para as accounts.json
-- Retorna: path do accounts.json
accsPath :: String
accsPath = "data/accounts.json"


-- Salva uma conta no json
-- Recebe: conta que será salva no json
saveAccJson :: Account -> IO()
saveAccJson acc = UT.incJsonFile acc accsPath


-- Deleta uma conta do json
-- Recebe: conta que será deletada no json
deleteAccFromJson :: Account -> IO()
deleteAccFromJson acc = do
    UT.deleteFromJson acc accsPath


-- Cria uma conta e a retorna, adicionando-a no json
-- Recebe: nome da conta que será criada
-- Retorna: conta criada com score 0
createAcc :: String -> IO Account
createAcc name = do
    let acc = Account {
            accName = name,
            accScore = 0
        }
    saveAccJson acc
    return acc


-- Retorna todas as contas do json
-- Retorna: array de contas
getAccs :: IO[Account]
getAccs = do
    UT.readJsonFile accsPath


-- Verifica se uma conta existe no json
-- Retorna: boolean que indica se a conta existe
accExists :: String -> IO Bool
accExists name = do
    maybeAcc <- getAccByName name
    return $ case maybeAcc of
        Nothing -> False
        Just _ -> True


-- Retorna uma conta do json procuradaa pelo nome
-- Recebe: nome da conta que será retornada
-- Retorna: conta que está armazenada no json, ou nada se a conta nao existir
getAccByName :: String -> IO (Maybe Account)
getAccByName targetName = do
    accs <- getAccs
    return $ UT.getObjByField accs accName targetName


-- Soma o score atual de uma conta com o passado na funcao, salvando o novo score no json 
-- Recebe: nome da conta que terá o score incrementado
-- Recebe: score que será incrementado no score da conta
incAccScore :: String -> Int -> IO()
incAccScore targetAccName incScore = do
    accs <- getAccs
    let updatedAccs = _getUpdatedAccs accs targetAccName incScore
    UT.writeJsonFile updatedAccs accsPath


-- Retorna os top 5 players com mais pontos das contas
-- Retorna: array de tamanho 5 das contas com maior score
getAccRank :: IO[Account]
getAccRank = do
    accs <- getAccs
    let sortedAccs = UT.sortObjsByField accs accScore
    return $ drop (length sortedAccs - 5) sortedAccs


-- Funcao interna que atualiza o score de uma conta
-- Recebe: array de contas antes do update na conta que terá seu score incrementado
-- Recebe: nome da conta que terá seu score incrementado
-- Recebe: score que será incrementado no score da conta selecionada
-- Retorna: array de contas com update na conta que terá seu score incrementado
_getUpdatedAccs :: [Account] -> String -> Int -> [Account]
_getUpdatedAccs [] _ _ = []
_getUpdatedAccs (acc:acct) targetAccName incScore
    | accName acc == targetAccName = (updatedAcc:acct)
    | otherwise = (acc:_getUpdatedAccs acct targetAccName incScore)
    where updatedAcc = acc {accScore = (accScore acc + incScore)}
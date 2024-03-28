{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Controllers.AccountsController where

import GHC.Generics
import Data.Aeson
import Utils.Utils as UT

data Account = Account {
    accName :: String,
    accScore :: Int
} deriving (Show, Generic, Eq)

instance ToJSON Account
instance FromJSON Account

accsPath :: String
accsPath = "data/accounts.json"

saveAccJson :: Account -> IO()
saveAccJson acc = UT.incJsonFile acc accsPath

deleteAccFromJson :: Account -> IO()
deleteAccFromJson acc = do
    UT.deleteFromJson acc accsPath

createAcc :: String -> IO Account
createAcc name = do
    let acc = Account {
            accName = name,
            accScore = 0
        }
    saveAccJson acc
    return acc

getAccs :: IO[Account]
getAccs = do
    UT.readJsonFile accsPath

accExists :: String -> IO Bool
accExists name = do
    maybeAcc <- getAccByName name
    return $ case maybeAcc of
        Nothing -> False
        Just _ -> True

getAccByName :: String -> IO (Maybe Account)
getAccByName targetName = do
    accs <- getAccs
    return $ UT.getObjByField accs accName targetName

incAccScore :: String -> Int -> IO()
incAccScore targetAccName incScore = do
    accs <- getAccs
    let updatedAccs = _getUpdatedAccs accs targetAccName incScore
    UT.writeJsonFile updatedAccs accsPath

getAccRank :: IO[Account]
getAccRank = do
    accs <- getAccs
    let sortedAccs = UT.sortObjsByField accs accScore
    return $ drop (length sortedAccs - 5) sortedAccs

_getUpdatedAccs :: [Account] -> String -> Int -> [Account]
_getUpdatedAccs [] _ _ = []
_getUpdatedAccs (acc:acct) targetAccName incScore
    | accName acc == targetAccName = (updatedAcc:acct)
    | otherwise = (acc:_getUpdatedAccs acct targetAccName incScore)
    where updatedAcc = acc {accScore = (accScore acc + incScore)}
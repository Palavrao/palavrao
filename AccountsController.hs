{-# LANGUAGE DeriveGeneric #-}

module AccountsController where

import GHC.Generics
import Data.Aeson
import Utils as UT

data Account = Account {
    accName :: String,
    accScore :: Int
} deriving (Show, Generic)

instance ToJSON Account
instance FromJSON Account

saveAcc :: Account -> IO()
saveAcc acc = UT.incJsonFile acc "data/accounts.json"

createAcc :: String -> IO(Account)
createAcc name = do
    let acc = Account {
            accName = name, 
            accScore = 0
        }
    saveAcc acc
    return acc

accExists :: String -> IO (Bool)
accExists name = do
    maybeAcc <- getAccByName name
    return $ case maybeAcc of
        Nothing -> False
        Just _ -> True

getAccByName :: String -> IO (Maybe Account)
getAccByName targetName = do
    accs <- UT.readJsonFile "data/accounts.json"
    return $ UT.getObjByField accs accName targetName

incAccScore :: String -> Int -> IO()
incAccScore targetAccName incScore = do
    accs <- UT.readJsonFile "data/accounts.json"
    let updatedAccs = _getUpdatedAccs accs targetAccName incScore
    UT.writeJsonFile updatedAccs "data/accounts.json"

_getUpdatedAccs :: [Account] -> String -> Int -> [Account]
_getUpdatedAccs [] _ _ = []
_getUpdatedAccs (acc:acct) targetAccName incScore
    | accName acc == targetAccName = (updatedAcc:acct)
    | otherwise = (acc:_getUpdatedAccs acct targetAccName incScore)
    where updatedAcc = acc {accScore = (accScore acc + incScore)}
{-# LANGUAGE DeriveGeneric #-}

module AccountsController where

import GHC.Generics
import Data.Aeson

import Utils as UT

data Account = Account {
    accName :: String,
    score :: Int
} deriving (Show, Generic)

instance ToJSON Account
instance FromJSON Account

saveAcc :: Account -> IO()
saveAcc acc = UT.incJsonFile acc "data/accounts.json"

getAccByName :: String -> IO (Maybe Account)
getAccByName targetName = do
    accs <- UT.readJsonFile "data/accounts.json"
    return $ UT.getObjByField accs accName targetName

incAccScore :: String -> Int -> IO()
incAccScore targetAccName targetScore = do
    accs <- UT.readJsonFile "data/accounts.json"
    let updatedAccs = _getUpdatedAccs accs targetAccName targetScore
    putStrLn(">>" ++ getJsonStr updatedAccs)
    UT.writeJsonFile updatedAccs "data/accounts.json"

_getUpdatedAccs :: [Account] -> String -> Int -> [Account]
_getUpdatedAccs [] _ _ = []
_getUpdatedAccs (acc:acct) targetAccName targetScore
    | accName acc == targetAccName = (Account {accName = accName acc, score = (score acc + targetScore)}:_getUpdatedAccs acct targetAccName targetScore)
    | otherwise = (acc:_getUpdatedAccs acct targetAccName targetScore)
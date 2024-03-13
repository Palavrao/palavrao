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

createAcc :: Account -> IO()
createAcc acc = UT.updateJsonFile acc "data/accounts.json"

getAccByName :: String -> IO (Maybe Account)
getAccByName targetName = do
    accs <- UT.readJsonFile "data/accounts.json"
    return $ _getAccByName accs targetName

_getAccByName :: [Account] -> String -> Maybe Account
_getAccByName [] _ = Nothing
_getAccByName (acc:acct) targetName 
    | accName acc == targetName = Just acc
    | otherwise = _getAccByName acct targetName
{-# LANGUAGE DeriveGeneric #-}

module AccountsController where

import GHC.Generics
import Data.Aeson

import Utils as UT

data Account = Account {
    name :: String,
    score :: Int
} deriving (Show, Generic)

instance ToJSON Account
instance FromJSON Account

createAcc :: Account -> IO()
createAcc acc = UT.updateJsonFile acc "data/accounts.json"
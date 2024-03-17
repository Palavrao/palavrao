module Utils where

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)

startPersistence :: IO()
startPersistence = do
    createDirectoryIfMissing True (takeDirectory "data/matches.json")
    createDirectoryIfMissing True (takeDirectory "data/accounts.json")
    
    writeFile "data/matches.json" "[]"
    writeFile "data/accounts.json" "[]"

readJsonFile :: (ToJSON t, FromJSON t) => FilePath -> IO [t]
readJsonFile path = do
    contents <- B.readFile path
    case eitherDecode contents of
        Left err -> error $ "Error decoding JSON: " ++ err
        Right people -> return people

writeJsonFile :: (ToJSON t, FromJSON t) => [t] -> FilePath -> IO()
writeJsonFile obj path = B.writeFile path (encode obj)

incJsonFile :: (ToJSON t, FromJSON t) => t -> FilePath -> IO()
incJsonFile obj path = do
    contents <- readJsonFile path
    let updatedContents = (obj:contents)
    writeJsonFile updatedContents path

getJsonStr :: (ToJSON t, FromJSON t) => t -> String
getJsonStr obj = BS.unpack encodedObj
    where encodedObj = encode obj

getObjByField :: (ToJSON t, FromJSON t, Eq b) => [t] -> (t -> b) -> b -> Maybe t
getObjByField [] _ _= Nothing
getObjByField (obj:objt) targetField targetValue
    | targetField obj == targetValue = Just obj
    | otherwise = getObjByField objt targetField targetValue
module Utils where

import qualified Data.ByteString.Lazy as B
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson

import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.ByteString.Lazy.Char8 as BS

readJsonFile :: (ToJSON t, FromJSON t) => FilePath -> IO [t]
readJsonFile path = do
    contents <- B.readFile path
    case eitherDecode contents of
        Left err -> error $ "Error decoding JSON: " ++ err
        Right people -> return people

writeJsonFile :: (ToJSON t, FromJSON t) => [t] -> FilePath -> IO()
writeJsonFile obj path = B.writeFile path (encode obj)

updateJsonFile :: (ToJSON t, FromJSON t) => t -> FilePath -> IO()
updateJsonFile obj path = do
    contents <- readJsonFile path
    let updatedContents = (obj:contents)
    writeJsonFile updatedContents path

getJsonStr :: (ToJSON t, FromJSON t) => t -> String
getJsonStr obj = BS.unpack encodedObj
    where encodedObj = encode obj
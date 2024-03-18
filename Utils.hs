module Utils where

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Directory (createDirectoryIfMissing, doesFileExist, doesDirectoryExist)
import System.FilePath.Posix (takeDirectory)
import System.Random

startPersistence :: IO()
startPersistence = do
    let dataPath = "data/"
    let matchesPath = dataPath ++ "/matches.json"
    let accountsPath = dataPath ++ "/accounts.json"

    dataDirectoryExists <- doesDirectoryExist (takeDirectory dataPath)
    createDirectoryIfMissing dataDirectoryExists (takeDirectory dataPath)
    
    matchesFileExists <- doesFileExist matchesPath
    if not matchesFileExists 
        then writeFile matchesPath "[]"
        else putStrLn "matches.json file already exists"

    accountsFileExists <- doesFileExist accountsPath
    if not accountsFileExists
        then writeFile accountsPath "[]"
        else putStrLn "accounts.json file already exists"

readJsonFile :: (ToJSON t, FromJSON t) => FilePath -> IO [t]
readJsonFile path = do
    contents <- B.readFile path
    case eitherDecode contents of
        Left err -> error $ "Error decoding JSON: " ++ err
        Right decodedContents -> return decodedContents

writeJsonFile :: (ToJSON t, FromJSON t) => [t] -> FilePath -> IO()
writeJsonFile obj path = B.writeFile path (encode obj)

deleteFromJsonObj :: (ToJSON t, FromJSON t, Eq t) => t -> FilePath -> IO()
deleteFromJsonObj obj path = do
    contents <- readJsonFile path
    let updatedContents = _removeOneElement contents obj
    writeJsonFile updatedContents path

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

popRandomElements :: (Eq t) => [t] -> Int -> ([t], [t])
popRandomElements avaiableElements qtdElements = _popRandomElements [] avaiableElements qtdElements

_removeOneElement :: (Eq t) => [t] -> t -> [t]
_removeOneElement [] _ = []
_removeOneElement (element:tail) removed 
    | element == removed = tail 
    | otherwise = (element:_removeOneElement tail removed)

generator = mkStdGen 40

_popRandomElements :: (Eq t) => [t] -> [t] -> Int -> ([t], [t])
_popRandomElements removedElements [] _ = (removedElements, [])
_popRandomElements removedElements finalElements 0 = (removedElements, finalElements)
_popRandomElements removedElements elements qtdElements = _popRandomElements (removedElements ++ [randElement]) updatedElements (qtdElements - 1)
    where 
        randElement = elements !! randIndex 
        (randIndex, _) = randomR (0,((length elements)-1)) generator
        updatedElements = _removeOneElement elements randElement
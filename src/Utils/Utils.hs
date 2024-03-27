module Utils.Utils where

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Directory (createDirectoryIfMissing, doesFileExist, doesDirectoryExist)
import System.FilePath.Posix (takeDirectory)
import System.Random
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Fixed (mod')
import System.Console.ANSI
import Controllers.LettersController
import Data.Char


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

deleteFromJson :: (ToJSON t, FromJSON t, Eq t) => t -> FilePath -> IO()
deleteFromJson obj path = do
    contents <- readJsonFile path
    let updatedContents = removeOneElement contents obj
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

sortObjsByField :: (Ord b) => [t]-> (t -> b) -> [t]
sortObjsByField [] _  = []
sortObjsByField objs targetField = sortBy (comparing targetField) objs 

popRandomElements :: (Eq t) => [t] -> Int -> IO([t], [t])
popRandomElements avaiableElements qtdElements = _popRandomElements [] avaiableElements qtdElements

removeOneElement :: (Eq t) => [t] -> t -> [t]
removeOneElement [] _ = []
removeOneElement (element:tail) removed 
    | element == removed = tail 
    | otherwise = (element:removeOneElement tail removed)

formatTime :: Float -> String
formatTime time = show (round (time / 60)) ++ ":" ++ show (round (mod' time 60))

_popRandomElements :: (Eq t) => [t] -> [t] -> Int -> IO([t], [t])
_popRandomElements removedElements [] _ = return (removedElements, [])
_popRandomElements removedElements finalElements 0 = return (removedElements, finalElements)
_popRandomElements removedElements elements qtdElements = do
    randIndex <- randomRIO (0,((length elements)-1))
    let randElement = elements !! (randIndex) 
    let updatedElements = removeOneElement elements randElement

    _popRandomElements (removedElements ++ [randElement]) updatedElements (qtdElements - 1)


getWordList :: IO [String]
getWordList = do
    base <- readFile "palavras/br-sem-acentos.txt"
    --putStrLn $ show (lines base)
    return (lines base)

manual :: IO ()
manual = do
            putStrLn "TODO"


__colorText :: String -> Color -> IO ()
__colorText text color = do
    setSGR [SetColor Foreground Vivid color]  -- Set the foreground color
    putStr text
    setSGR [Reset]

getLetterObject :: Char -> Maybe Letter
getLetterObject c
    | value == 0 = Nothing
    | otherwise = Just $ Letter {letter=toUpper c, letterScore=value}
    where value = letterValue (toUpper c)


_removeChar :: Char -> [Char] -> [Char]
_removeChar x (y:ys) 
    | x == y = ys
    | otherwise = y : _removeChar x ys

removeChars :: [Char] -> [Char] -> [Char]
removeChars toRemove list = foldr (\x acc -> _removeChar x acc) list toRemove

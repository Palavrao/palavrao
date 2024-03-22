import Data.Char

--DEBUGAR
tileValidationLetters :: String -> String -> IO ()
tileValidationLetters (tileHead:tileTail) (wordHead:wordTail)
    | wordTail == [] = putStrLn "Empty True"
    | (tileHead `elem` ['A'..'Z']) && tileHead /= wordHead = putStrLn (tileTail ++ " " ++ wordTail)
    | otherwise = tileValidationLetters tileTail wordTail

takeUpTo :: Bool -> [[Char]] -> (Int, Int) -> String -> [Char]
takeUpTo isHorizontal b (x, y) word
    | isHorizontal = (take (length word) (drop x (b !! y)))
    | otherwise = take (length word) $ map (!! x) $ drop y b


getOrds :: IO ()
getOrds = mapM_ putStrLn [show (ord (coord ) - ord 'A')| coord <- ['A'..'O']]


main :: IO ()
main = do
    let m = [ ['#', '~', '~', '*', '~', '~', '~', '#', '~', '~', '~', '*', '~', '~', '#'],
              ['~', '-', '~', '~', '~', '!', '~', '~', '~', '!', '~', '~', '~', '-', '~'],
              ['~', '~', '-', '~', '~', '~', '*', '~', '*', '~', '~', '~', '-', '~', '~'],
              ['*', '~', '~', '-', '~', '~', '~', '*', '~', '~', '~', '-', '~', '~', '*'],
              ['~', '~', '~', '~', '-', '~', '~', '~', '~', '~', 'S', '~', '~', '~', '~'],
              ['~', '!', '~', '~', 'C', 'A', 'S', 'A', '~', 'S', 'A', '~', '~', '!', '~'],
              ['~', '~', '*', '~', '~', 'V', '*', '~', '*', 'O', 'L', 'A', '*', '~', '~'],
              ['#', '~', '~', '*', '~', 'E', 'S', 'C', 'O', 'L', 'A', '*', '~', '~', '#'],
              ['~', '~', '*', '~', '~', '~', '*', '~', '*', '~', '~', '~', '*', '~', '~'],
              ['~', '!', '~', '~', '~', '!', '~', '~', '~', '!', '~', '~', '~', '!', '~'],
              ['~', '~', '~', '~', '-', '~', '~', '~', '~', '~', '-', '~', '~', '~', '~'],
              ['*', '~', '~', '-', '~', '~', '~', '*', '~', '~', '~', '-', '~', '~', '*'],
              ['~', '~', '-', '~', '~', '~', '*', '~', '*', '~', '~', '~', '-', '~', '~'],
              ['~', '-', '~', '~', '~', '!', '~', '~', '~', '!', '~', '~', '~', '-', '~'],
              ['#', '~', '~', '*', '~', '~', '~', '#', '~', '~', '~', '*', '~', '~', '#']]
    
    getOrds

    tileValidationLetters (takeUpTo True m (3,5) "ACASA") "ACASA" -- "01234567"
    tileValidationLetters (takeUpTo True m (0,1) "BALOFO") "BALOFO" -- "12345678"
    {- 
    tileValidationLetters (takeUpTo True m (1,0) "") "" -- "abcdefgh"
    tileValidationLetters (takeUpTo True m (1,1) "") "" -- "bcdefghi"
    putStrLn ""
    tileValidationLetters (takeUpTo True m (0,1) "") "" -- ""
    tileValidationLetters (takeUpTo True m (1,0) "") "" -- "abcdefghij" pega ate o fim nao da erro
    tileValidationLetters (takeUpTo True m (1,1) "") "" -- "b"
    putStrLn "" 
    tileValidationLetters (takeUpTo True m (4,6) "") "" -- ""
    tileValidationLetters (takeUpTo True m (4,4) "") "" -- "abcdefghij" pega ate o fim nao da erro
    tileValidationLetters (takeUpTo True m (6,4) "") ""
    -}
    

    putStrLn "Verticals"
    tileValidationLetters (takeUpTo False m (0,0) "A") "A" -- "01234567"
    tileValidationLetters (takeUpTo False m (5,4) "CAVE") "CAVE" -- "12345678"
    {-
    tileValidationLetters (takeUpTo False m (1,0) "A") "A" -- "abcdefgh"
    tileValidationLetters (takeUpTo False m (1,1) "A") "A" -- "bcdefghi"
    putStrLn ""
    tileValidationLetters (takeUpTo False m (0,1) "A") "A" -- ""
    tileValidationLetters (takeUpTo False m (1,0) "A") "A" -- "abcdefghij" pega ate o fim nao da erro
    tileValidationLetters (takeUpTo False m (1,1) "A") "A" -- "b" 
    -}

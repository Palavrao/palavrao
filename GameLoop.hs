module GameLoop where
import Text.Printf
import System.Console.ANSI
import MatchesController
import Data.Char


printColoredText :: String -> Color -> IO ()
printColoredText text color = do
    setSGR [SetColor Foreground Vivid color]  -- Set the foreground color
    --setSGR [SetColor Background Vivid color]
    putStr text
    setSGR [Reset]  -- Reset text attributes to default

data Tabuleiro = Tabuleiro {
    match :: Match,
    curTiles :: [[String]],
    workTiles :: [[String]]
}
-- █ ■
replacements :: Char -> IO ()
replacements (a)
    |a == '#' = printColoredText "■ " Red 
    |a == '-' = printColoredText "■ " Magenta
    |a == '*' = printColoredText "■ " Blue
    |a == '!' = printColoredText "■ " Green
    |a == '.' = printColoredText "■ " White
    |a == '\n' = printf "\n"
    |isSpace a = return ()
    |isPrint a = printColoredText ( [toUpper a] ++ " ") White
    |otherwise = return ()

instance Show Tabuleiro where
    show (Tabuleiro match curTiles workTiles) = 
        unlines ["                                                              ",
                      "                                                              ",
                      "    A B C D E F G H I J K L M N O                             "] ++
        unlines (map (unwords . map printf) curTiles)


startTabuleiro :: Match -> Tabuleiro
startTabuleiro m = Tabuleiro {match=m
                             ,curTiles= 
                                    [["#", ".", ".", "*", ".", ".", ".", "#", ".", ".", ".", "*", ".", ".", "#"],
                                     [".", "-", ".", ".", ".", "!", ".", ".", ".", "!", ".", ".", ".", "-", "."],
                                     [".", ".", "-", ".", ".", ".", "*", ".", "*", ".", ".", ".", "-", ".", "."],
                                     ["*", ".", ".", "-", "H", "e", "L", "L", "O", ".", ".", "-", ".", ".", "*"],
                                     [".", ".", ".", ".", "-", ".", "O", ".", ".", ".", "-", ".", ".", ".", "."],
                                     [".", "!", ".", ".", ".", "!", "V", ".", ".", "!", ".", ".", ".", "!", "."],
                                     [".", ".", "*", ".", ".", ".", "E", ".", "*", ".", ".", ".", "*", ".", "."],
                                     ["#", ".", ".", "*", ".", ".", ".", "-", ".", ".", ".", "*", ".", ".", "#"],
                                     [".", ".", "*", ".", ".", ".", "*", ".", "*", ".", ".", ".", "*", ".", "."],
                                     [".", "!", ".", ".", ".", "!", ".", ".", ".", "!", ".", ".", ".", "!", "."],
                                     [".", ".", ".", ".", "-", ".", ".", ".", ".", ".", "-", ".", ".", ".", "."],
                                     ["*", ".", ".", "-", ".", ".", ".", "*", ".", ".", ".", "-", ".", ".", "*"],
                                     [".", ".", "-", ".", ".", ".", "*", ".", "*", ".", ".", ".", "-", ".", "."],
                                     [".", "-", ".", ".", ".", "!", ".", ".", ".", "!", ".", ".", ".", "-", "."],
                                     ["#", ".", ".", "*", ".", ".", ".", "#", ".", ".", ".", "*", ".", ".", "#"]]
                            ,workTiles =
                                    [["#", ".", ".", "*", ".", ".", ".", "#", ".", ".", ".", "*", ".", ".", "#"],
                                     [".", "-", ".", ".", ".", "!", ".", ".", ".", "!", ".", ".", ".", "-", "."],
                                     [".", ".", "-", ".", ".", ".", "*", ".", "*", ".", ".", ".", "-", ".", "."],
                                     ["*", ".", ".", "-", ".", ".", ".", "*", ".", ".", ".", "-", ".", ".", "*"],
                                     [".", ".", ".", ".", "-", ".", ".", ".", ".", ".", "-", ".", ".", ".", "."],
                                     [".", "!", ".", ".", ".", "!", ".", ".", ".", "!", ".", ".", ".", "!", "."],
                                     [".", ".", "*", ".", ".", ".", "*", ".", "*", ".", ".", ".", "*", ".", "."],
                                     ["#", ".", ".", "*", ".", ".", ".", "-", ".", ".", ".", "*", ".", ".", "#"],
                                     [".", ".", "*", ".", ".", ".", "*", ".", "*", ".", ".", ".", "*", ".", "."],
                                     [".", "!", ".", ".", ".", "!", ".", ".", ".", "!", ".", ".", ".", "!", "."],
                                     [".", ".", ".", ".", "-", ".", ".", ".", ".", ".", "-", ".", ".", ".", "."],
                                     ["*", ".", ".", "-", ".", ".", ".", "*", ".", ".", ".", "-", ".", ".", "*"],
                                     [".", ".", "-", ".", ".", ".", "*", ".", "*", ".", ".", ".", "-", ".", "."],
                                     [".", "-", ".", ".", ".", "!", ".", ".", ".", "!", ".", ".", ".", "-", "."],
                                     ["#", ".", ".", "*", ".", ".", ".", "#", ".", ".", ".", "*", ".", ".", "#"]]}


tabuleiro :: String -> Int -> String -> Int -> String
tabuleiro p1 s1 p2 s2 = unlines [
                      "                                                              ",
                      "                                                              ",
                      "    A B C D E F G H I J K L M N O                             ",
                      "                                                              ",
                      "    # . . * . . . # . . . * . . #   01                        ",
                      "    . - . . . ! . . . ! . . . - .   02                        ",
                      "    . . - . . . * . * . . . - . .   03                        ",
                      "    * . . - . . . * . . . - . . *   04                        ",
                      "    . . . . - . . . . . - . . . .   05                        ",
                      "    . ! . . . ! . . . ! . . . ! .   06                        ",
                      "    . . * . . . * . * . . . * . .   07                        ",
                      "    # . . * . . . - . . . * . . #   08                        ",
                      "    . . * . . . * . * . . . * . .   09                        ",
                      "    . ! . . . ! . . . ! . . . ! .   10                        ",
                      "    . . . . - . . . . . - . . . .   11                        ",
                      "    * . . - . . . * . . . - . . *   12   :C   sair            ",
                      "    . . - . . . * . * . . . - . .   13   :?   manual          ",
                      "    . - . . . ! . . . ! . . . - .   14   :!   pular vez       ",
                      "    # . . * . . . # . . . * . . #   15   :*X  trocar letra x  ",
                      "                                                              ",
              (printf "                                          %-5s.     %-5s.    " p1 p2),
              (printf "    _______      00:00                    %03d pt     %03d pt    " s1 s2)]



getCol :: Int -> [[t]] -> [t]
getCol n mat = map (!!n) mat

verTabuleiro :: Tabuleiro -> IO ()
verTabuleiro t = do
    mapM replacements $ show t
    let p1 = "fulano"
    let p2 = "beltrano"
    let s1 = 1 :: Int
    let s2 = 100 :: Int
    putStrLn (tabuleiro (take 5 p1) s1 (take 5 p2) s2)
    printf "VEZ DE FULANO\n"
    printf "Digite a posição e sua palavra no formato X00 V PALAVRA\n"
    let col = getCol 2 ([[1,2,3],[4,5,6]] :: [[Int]])
    printColoredText (show col) Red





{-     createAcc Account {accName="fulano", score=0}
    createAcc Account {accName="beltrano", score=0}
    let m = Match {matchName="match1", p1Name="fulano", p1Score=0, p2Name="beltrano", p2Score=0}
    createMatch m

    let t = startTabuleiro m
    verTabuleiro t
    
    --mapM (mapM replacements) (curTiles t) 
    putStr "" -}
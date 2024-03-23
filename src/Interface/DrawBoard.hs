module Interface.DrawBoard where

import Text.Printf
import System.Console.ANSI
import Controllers.MatchesController
import Controllers.AccountsController
import Controllers.BoardController
import Data.Char
import Controllers.PlayerController
import Utils.Utils as UT

__colorText :: String -> Color -> IO ()
__colorText text color = do
    setSGR [SetColor Foreground Vivid color]  -- Set the foreground color
    --setSGR [SetColor Background Vivid color]
    putStr text
    setSGR [Reset]  -- Reset text attributes to default


-- █ ■
__pintaBoard :: Char -> IO ()
__pintaBoard (a)
    |a == '#' = __colorText "■ " Red 
    |a == '-' = __colorText "■ " Magenta
    |a == '*' = __colorText "■ " Blue
    |a == '!' = __colorText "■ " Green
    |a == '~' = __colorText "■ " White
    |a == '>' = printf " "
    |a == '\n' = printf "\n"
    |isDigit a = printf [a]
    |isSpace a = return ()
    |isPrint a = __colorText ( [toUpper a] ++ " ") White
    |otherwise = return ()


tt :: Board -> IO ()
tt b = do
    putStrLn (unlines $ workTiles b)


_suffixes :: Match -> Int -> String
_suffixes match i 
    |i == 1 = " 01                        "
    |i == 2 = printf " 02    %-5s.     %-5s." (take 5 $ p1n) (take 5 $ p2n)
    |i == 3 = printf " 03    %03d pt     %03d pt    " p1s p2s
    |i == 4 = " 04                        "
    |i == 5 = " 05                        "
    |i == 6 = " 06                        "
    |i == 7 = printf " 07    %s     " (UT.formatTime (mTimer match))
    |i == 8 = " 08                        "
    |i == 9 = " 09                        "
    |i == 10 = " 10                        "
    |i == 11 = " 11                        "
    |i == 12 = " 12   :C   sair            "
    |i == 13 = " 13   :?   manual          "
    |i == 14 = " 14   :!   pular vez       "
    |i == 15 = " 15   :*X  trocar letra x  "
    |otherwise = ""
    where 
        p1n = accName (pAcc (mP1 match))
        p2n = accName (pAcc (mP2 match))
        p1s = pScore (mP1 match)
        p2s = pScore (mP2 match)


_buildBoard :: Match -> Int -> IO ()
_buildBoard match i = do
    if i > 15 then 
        putStr (unlines
                [printf "\n     X X X X X X X                  00:00",
                 printf "     0 0 0 0 0 0 0                  Letras Restantes: 00\n"])
    else do
        putStr "     "
        mapM_ __pintaBoard (lines!!i)
        putStrLn (_suffixes match i)
        _buildBoard match (i + 1)
    where 
        formattedLines = map printf (curTiles $ mBoard match)
        lines = ["A B C D E F G H I J K L M N O"] ++ formattedLines


printBoard :: Match -> IO ()
printBoard match = do
    --clearScreen
    _buildBoard match 0
    

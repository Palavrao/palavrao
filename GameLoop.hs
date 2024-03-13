module GameLoop where
import Text.Printf
import System.Console.ANSI

tabuleiro  = unlines [
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
                      "                                                              "]
linhas :: String -> Int -> String -> Int -> String
linhas p1 s1 p2 s2 = unlines [
                        (printf "                                          %-5s.     %-5s.    " p1 p2),
                        (printf "    _______      00:00                    %03d pt     %03d pt    " s1 s2),
                        "                                                              "]

getCol :: Int -> [[t]] -> [t]
getCol n mat = map (!!n) mat

verTabuleiro = do
    putStr tabuleiro
    let p1 = "fulano"
    let p2 = "beltrano"
    let s1 = 1
    let s2 = 10
    printf (linhas (take 5 p1) s1 (take 5 p2) s2)
    printf "VEZ DE FULANO\n"
    printf "Digite a posição e sua palavra no formato X00 V PALAVRA\n"
    let col = getCol 2 ([[1,2,3],[4,5,6]] :: [[Int]])
    printf (show col)






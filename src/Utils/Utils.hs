{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda" #-}
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



-- Inicia a persistência dos jsons na pasta data 
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


-- Lê um arquivo json e decodifica para um array de objeto
-- Recebe: path para o arquivo que será lido
-- Retorna: array de objetos que podem ser decodificados do json
readJsonFile :: (ToJSON t, FromJSON t) => FilePath -> IO [t]
readJsonFile path = do
    contents <- B.readFile path
    case eitherDecode contents of
        Left err -> error $ "Error decoding JSON: " ++ err
        Right decodedContents -> return decodedContents


-- Escreve um arquivo json
-- Recebe: array de objetos que podem ser codificados para json
-- Recebe: path para onde o arquivo json será escrito
writeJsonFile :: (ToJSON t, FromJSON t) => [t] -> FilePath -> IO()
writeJsonFile obj path = B.writeFile path (encode obj)


-- Deleta algo de um arquivo json
-- Recebe: objeto que pode ser codificado para json
-- Recebe: path para o arquivo que terá o objeto deletado
deleteFromJson :: (ToJSON t, FromJSON t, Eq t) => t -> FilePath -> IO()
deleteFromJson obj path = do
    contents <- readJsonFile path
    let updatedContents = removeOneElement contents obj
    writeJsonFile updatedContents path


-- Adiciona conteúdo em um arquivo json
-- Recebe: objeto que pode ser codificado para json
-- Recebe: path para o arquivo que terá seu conteúdo adicionado
incJsonFile :: (ToJSON t, FromJSON t) => t -> FilePath -> IO()
incJsonFile obj path = do
    contents <- readJsonFile path
    let updatedContents = (obj:contents)
    writeJsonFile updatedContents path


-- Retorna um objeto que será procurado por um field específicio
-- Recebe: array de objetos que possuem um field específico
-- Recebe: função que terá o papel de pegar o field do objeto
-- Recebe: valor esperado que o objeto tenha para ser retornado
-- Retorna: objeto que está sendo produrado pelo field ou nada caso não seja encontrado
getObjByField :: (ToJSON t, FromJSON t, Eq b) => [t] -> (t -> b) -> b -> Maybe t
getObjByField [] _ _= Nothing
getObjByField (obj:objt) targetField targetValue
    | targetField obj == targetValue = Just obj
    | otherwise = getObjByField objt targetField targetValue


-- Ordena um array de objetos por um field específico
-- Recebe: array de objetos que será ordenado
-- Recebe: função que terá o papel de pegar o field do objeto
-- Retorna: array de objetos ordenados
sortObjsByField :: (Ord b) => [t]-> (t -> b) -> [t]
sortObjsByField [] _  = []
sortObjsByField objs targetField = sortBy (comparing targetField) objs 


-- Faz um pop de elementos aleatórios de um array de objetos
-- Recebe: array de objetos que terá seus elementos removidos
-- Recebe: quantidade de elementos que serão removidos do array
-- Retorna: tupla contendo informações dos elementos que foram removidos e dos que ficaram
-- no array final
popRandomElements :: (Eq t) => [t] -> Int -> IO([t], [t])
popRandomElements avaiableElements qtdElements = _popRandomElements [] avaiableElements qtdElements


-- Remove um elemento específico de um array de objetos
-- Recebe: array de objetos que terá o elemento removido
-- Recebe: elemento que será removido do array
-- Retorna: array de elementos pós remoção do elemento desejado
removeOneElement :: (Eq t) => [t] -> t -> [t]
removeOneElement [] _ = []
removeOneElement (element:tail) removed 
    | element == removed = tail 
    | otherwise = (element:removeOneElement tail removed)


-- Retorna a lista de palavras registradas no arquivo txt
-- Retorna: array de palavras disponíveis
getWordList :: IO [String]
getWordList = do
    base <- readFile "palavras/br-sem-acentos.txt"
    return (lines base)


-- Codifica um char para uma letra
-- Recebe: caractere que será codificado para uma letra
-- Retorna: letra representando o caractere e seu valor, ou nada caso o caractere
-- não possa ser mapeado para uma letra
getLetterObject :: Char -> Maybe Letter
getLetterObject c
    | value == 0 = Nothing
    | otherwise = Just $ Letter {letter=toUpper c, letterScore=value}
    where value = letterValue (toUpper c)


-- Remove caracteres de uma lista de caracteres
-- Recebe: uma lista inicial de caracteres
-- Recebe: uma lista de caracteres a ser removidos
-- Retorna: a lista inicial com os caracteres removidos, um caracter que aparece N vezes tem suas primeiras N instâncias removidas
removeChars :: [Char] -> [Char] -> [Char]
removeChars toRemove list = foldr (\x acc -> _removeChar x acc) list toRemove


-- Verifica se uma string é composta de dígitos e se pode ser lida como int
-- Recebe: uma string
-- Retorna: True se ela puder ser lida como int, False se não
isStringInt :: String -> Bool
isStringInt str = case reads str :: [(Int, String)] of
    [(num, "")] -> True
    _           -> False


-- Função interna que faz pop de elementos aleatórios de um certo array de objetos
-- Recebe: elementos que foram removidos de outras iterações na recursão
-- Recebe: elementos que ficaram depois da remoção nas outras iterações da recursão
-- Recebe: quantidade de elementos restantes que devem ser removidos do array
-- Retorna: tupla contendo informações dos elementos que foram removidos e dos que ficaram
-- no array final
_popRandomElements :: (Eq t) => [t] -> [t] -> Int -> IO([t], [t])
_popRandomElements removedElements [] _ = return (removedElements, [])
_popRandomElements removedElements finalElements 0 = return (removedElements, finalElements)
_popRandomElements removedElements elements qtdElements = do
    randIndex <- randomRIO (0,((length elements)-1))
    let randElement = elements !! (randIndex) 
    let updatedElements = removeOneElement elements randElement

    _popRandomElements (removedElements ++ [randElement]) updatedElements (qtdElements - 1)


-- Imprime texto colorido no terminal
-- Recebe: a string a ser impressa
-- Recebe: a cor da string a ser impressa
-- Retorna: não tem retorno, printa no terminal a string na cor especificada
__colorText :: String -> Color -> IO ()
__colorText text color = do
    setSGR [SetColor Foreground Vivid color]  -- Set the foreground color
    putStr text
    setSGR [Reset]


-- Imprime na tela as regras do jogo
_printRules :: IO ()
_printRules = do
    __colorText "\nPalavrão! " Yellow
    putStrLn "é um jogo estratégico double player de formação de palavras em um tabuleiro matricial, baseado no popular \"Scrabble\".\n"
    
    __colorText "Objetivo do jogo:\n\n" Green
    
    putStrLn "  O objetivo do jogo é acumular a maior quantidade de pontos possível a partir da formação de novas palavras horizontal ou verticalmente, e adjacentes às palavras já dispostas no tabuleiro.\n"
    
    __colorText "Funcionamento do jogo:\n\n" Green
    
    putStrLn 
        "   - Cada jogador recebe 7 letras, que podem ser trocadas por letras aleatórias do saco de letras, fazendo-o perder a vez.\n\n\
        \   - O primeiro jogador coloca ao menos duas letras no tabuleiro formando a primeira palavra.\n\n\
        \   - Em seguida os jogadores tomam turnos adicionando letras adjacentes às letras dispostas no tabuleiro para formar novas palavras.\n\n\
        \   - Para formar novas palavras, o jogador deve indicar a coordenada da célula onde deseja adicionar a primeira letra e se a palavra deve ser disposta horizontal ou verticalmente no tabuleiro.\n\n\
        \   - O tabuleiro irá entender quando as letras da palavra já estiverem presentes na posição necessária, então basta escrever a palavra normalmente com as coordenadas.\n\n\
        \   - Dentre as letras dos jogadores pode haver peças curinga (<), que os permitem utilizar letras que não estão em mãos nem no tabuleiro. Cada letra ausente consome um curinga.\n\n\
        \   - A pontuação para cada rodada é a soma dos valores das letras em cada palavra formada ou modificada + os pontos adicionais obtidos de células e ocasiões especiais.\n\n\
        \   - As rodadas se alternam com um limite de tempo de 5 minutos para cada jogada. Passado esse tempo, o jogador perde a vez.\n\n\
        \   - O jogador pode pular a sua vez."
        

    
    __colorText "Pontuação das letras:\n\n" Green 
    
    __colorText "    0   " Blue
    putStr "|   < (Curinga)\n"
    __colorText "    1   " Blue
    putStr "|   A, E, I, O, S, U, M, R, T\n"
    __colorText "    2   " Blue
    putStr "|   D, L, C, P\n"
    __colorText "    3   " Blue
    putStr "|   N, B, Ç\n"
    __colorText "    4   " Blue
    putStr "|   F, G, H, V\n"      
    __colorText "    5   " Blue
    putStr "|   J\n"
    __colorText "    6   " Blue
    putStr "|   Q\n" 
    __colorText "    7   " Blue
    putStrLn "|   X, Z\n"

    __colorText "Pontuações especiais:\n\n" Green 

    __colorText "    ■ " Blue
    putStrLn "-> Dobra a pontuação da letra sobre a célula."
    __colorText "    ■ " Green
    putStrLn "-> Triplica a pontuação da letra sobre a célula."
    __colorText "    ■ " Magenta
    putStrLn "-> Dobra a pontuação de toda a palavra cuja letra está sobre a célula."
    __colorText "    ■ " Red
    putStrLn "-> Triplica a pontuação de toda a palavra cuja letra está sobre a célula.\n"

    putStr "   - "
    __colorText "Bingo! " Magenta
    putStrLn "Se um jogador usar 7 letras para formar uma nova palavra, a pontuação dela é incrementada em 20 pontos.\n\n"
    
    __colorText "Fim de jogo:\n\n" Green
    
    putStrLn "  O jogo termina quando não há mais peças no saco ou ambos os jogadores realizam 4 trocas de peças seguidas. Em caso de empate, o jogador com a menor soma na pontuação das letras em sua mão vence.\n\n"

    __colorText "Enter para voltar\n\n" Blue


-- Imprime na tela uma versão reduzida das regras do jogo
_printRulesShortened :: IO ()
_printRulesShortened = do

    __colorText "\nPontuações especiais:\n\n" Green 

    __colorText "    ■ " Blue
    putStrLn "-> Dobra a pontuação da letra sobre a célula."
    __colorText "    ■ " Green
    putStrLn "-> Triplica a pontuação da letra sobre a célula."
    __colorText "    ■ " Magenta
    putStrLn "-> Dobra a pontuação de toda a palavra cuja letra está sobre a célula."
    __colorText "    ■ " Red
    putStrLn "-> Triplica a pontuação de toda a palavra cuja letra está sobre a célula.\n"
    
    putStr "   - "
    __colorText "Bingo! " Magenta
    putStrLn "Se um jogador usar 7 letras para formar uma nova palavra, a pontuação dela é incrementada em 20 pontos.\n\n"

    __colorText "Fim de jogo:\n\n" Green
    
    putStrLn "  O jogo termina quando não há mais peças no saco ou os jogadores realizam, em conjunto, 4 trocas de peças ou saltos de vez seguidos. Em caso de empate, o jogador com a menor soma na pontuação das letras em sua mão vence.\n\n"

    __colorText "Enter para voltar\n\n" Blue


-- Remove um caracter de uma lista de caracteres
-- Recebe: o caracter a ser removido
-- Recebe: uma lista de caracteres
-- Retorna: a lista com aquele caracter removido
_removeChar :: Char -> [Char] -> [Char]
_removeChar x (y:ys) 
    | x == y = ys
    | otherwise = y : _removeChar x ys


centerString :: String -> Int -> Char -> String
centerString str width f
    | length str >= width = str
    | otherwise = let spaces = width - length str
                      leftSpaces = spaces `div` 2
                      rightSpaces = spaces - leftSpaces
                  in replicate leftSpaces f ++ str ++ replicate rightSpaces ' '

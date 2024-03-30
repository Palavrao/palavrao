{-# OPTIONS_GHC -Wno-missing-fields #-}
module Interface.BoxesMenu where

import Text.Printf
import System.Console.ANSI
import Data.Char
import Controllers.AccountsController
import Controllers.PlayerController
import Controllers.MatchesController



-- Ação que representa cada tela do jogo
data Action = NewGame | ContinueGame | Login | Register | RegisterMatch | StartMenu | Rank | InvalidAction | BeforeGame | FinishMatch | Matches deriving (Show, Eq)


-- Menu é uma entidade que tem propósito de expor os dados aos usuários, ele deve ter associado a ele:
-- Box, que é a parte visual do menu, que muda de acordo com o action
-- P1, que representa o player 1 logado no menu
-- P2, que representa o player 2 logado no menu
-- AccsRank, que é um array das 5 contas com as maiores pontuações registradas
-- Action, que representa a tela atual do menu
-- BoxBefore, que representa a tela anterior que estava sendo exibida
-- CurrentMatch, que representa a partida atual que está armazenada no menu, que será iniciada ou substituida
-- IndexMenu, que representa a pagina de listagem de partidas
data Menu = Menu {
    box :: [[Char]],
    p1 :: Account,
    p2 :: Account,
    accsRank :: [Account],
    action :: Action,
    boxBefore :: Action,
    currentMatch :: Match,
    indexMatch :: Int
} deriving (Show, Eq)


-- Tela inicial do jogo, que mostra ao usuário os limites do terminal que devem ser seguidos, inicializando
-- também os dados do menu, como vazio e valor default
beginGame :: Menu
beginGame = Menu {
    box = [
        "                 ┌─────────────────────────────┐                ",
        "                 │                             │                ",
        "                 │    Redimensione para que    │                ",
        "                 │  a linha caiba no terminal! │                ",
        "                 │                             │                ",
        "<-------------------------------------------------------------->",
        "                 │           > Enter           │                ",
        "                 │                             │                ",
        "                 └─────────────────────────────┘                "
    ], boxBefore = InvalidAction, action = StartMenu, p1 = defaultAccount, p2 = defaultAccount, currentMatch = Match{mName = ""}}


-- Atualiza o menu de acordo com a action recebida
-- Recebe: ação do player que será utilizada para redirecionamento da tela
-- Recebe: tela atual do jogador com as informações atuais
-- Retorna: tela redirecionada com as informações do menu anterior
updateMenu :: Action -> Menu -> Menu
updateMenu action menu = case action of
    -- Tela de menu inicial
        StartMenu -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        printf "    │  %-5s                 %-5s  │   " (take 5 $ accName (p1 menu)) (take 5 $ accName (p2 menu)),
        "    │                               │   ",
        "    │           PALAVRÃO            │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │      1.  novo jogo            │   ",
        "    │      2.  continuar jogo       │   ",
        "    │      3.  regras               │   ",
        "    │      4.  rank                 │   ",
        "    │      5.  sair                 │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    └───────────────────────────────┘   "
     ], boxBefore = StartMenu, action = StartMenu}
    -- Tela de iniciação de novo jogo
        NewGame -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        printf "    │  %-5s                 %-5s  │   " (take 5 $ accName (p1 menu)) (take 5 $ accName (p2 menu)),
        "    │                               │   ",
        "    │           PALAVRÃO            │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │      1. criar conta           │   ",
        "    │      2. login                 │   ",
        "    │      3. voltar                │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │      iniciando novo jogo      │   ",
        "    └───────────────────────────────┘   "
    ], boxBefore = StartMenu, action = NewGame}
    -- Tela de iniciação de continuar jogo
        ContinueGame -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        printf "    │  %-5s                 %-5s  │   " (take 5 $ accName (p1 menu)) (take 5 $ accName (p2 menu)),
        "    │                               │   ",
        "    │           PALAVRÃO            │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │      1. nome da partida       │   ",
        "    │      2. partidas criadas      │   ",
        "    │      3. voltar                │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │        continuar jogo         │   ",
        "    └───────────────────────────────┘   "
    ], boxBefore = StartMenu, action = ContinueGame}
    -- Tela de iniciação de login de conta
        Login -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        printf "    │  %-5s                 %-5s  │   " (take 5 $ accName (p1 menu)) (take 5 $ accName (p2 menu)),
        "    │                               │   ",
        "    │           PALAVRÃO            │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │     1. digitar nome           │   ",
        "    │     2. voltar                 │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │            login              │   ",
        "    └───────────────────────────────┘   "
    ], boxBefore = NewGame, action = Login}
    -- Tela antes do jogo contendo as informações da partida
        BeforeGame -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        printf "    │  %-5s                 %-5s  │   " (take 5 $ accName (p1 menu)) (take 5 $ accName (p2 menu)),
        "    │                               │   ",
        "    │           PALAVRÃO            │   ",
        "    │                               │   ",
        printf "    │        %-16s       │   " (take 16 $ mName (currentMatch menu)),
        "    │                               │   ",
        "    │     1. ir para jogo           │   ",
        "    │     2. voltar                 │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │        opcoes de jogo         │   ",
        "    └───────────────────────────────┘   "
    ], boxBefore = StartMenu, action = BeforeGame}
    -- Tela de criação de conta
        Register -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        printf "    │  %-5s                 %-5s  │   " (take 5 $ accName (p1 menu)) (take 5 $ accName (p2 menu)),
        "    │                               │   ",
        "    │           PALAVRÃO            │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │     1. digitar nome           │   ",
        "    │     2. voltar                 │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │        criando conta          │   ",
        "    └───────────────────────────────┘   "
    ], boxBefore = StartMenu, action = Register}
    -- Tela de criação de partida
        RegisterMatch -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        printf "    │  %-5s                 %-5s  │   " (take 5 $ accName (p1 menu)) (take 5 $ accName (p2 menu)),
        "    │                               │   ",
        "    │           PALAVRÃO            │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │     1. criar nome da partida  │   ",
        "    │     2. tirar player 1         │   ",
        "    │     3. tirar player 2         │   ",
        "    │     4. voltar                 │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │       criando partida         │   ",
        "    └───────────────────────────────┘   "
    ], boxBefore = StartMenu, action = RegisterMatch}
    -- Tela de rank das contas, com as 5 contas com maior pontuação
        Rank -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        printf "    │  %-5s                 %-5s  │   " (take 5 $ accName (p1 menu)) (take 5 $ accName (p2 menu)),
        "    │                               │   ",
        "    │           PALAVRÃO            │   ",
        "    │                               │   ",
        "    │                               │   "]
        ++ _getRankLines menu ++
       ["    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │             rank              │   ",
        "    └───────────────────────────────┘   "
    ], boxBefore = StartMenu, action = Rank}
    -- Tela de listagem das partidas criadas
        Matches -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        "    │  1. avançar        2. voltar  │   ",
        "    │                               │   ",
        "    │           PALAVRÃO            │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    └───────────────────────────────┘   "
    ], boxBefore = ContinueGame, action = Matches, indexMatch = 0}
    -- Tela de finalização de jogo, contendo as informações finais da partida
        FinishMatch -> menu { box = [
        "    ┌───────────────────────────────┐   ",
        printf "    │  %-5s                 %-5s  │   " (take 5 $ accName (p1 menu)) (take 5 $ accName (p2 menu)),
        "    │                               │   ",
        "    │           PALAVRÃO            │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │        Player ganhador        │   ",
        "    │                               │   ",
        printf "    │        %-17s      │   " (take 17 $ accName (pAcc (getBestPlayer (currentMatch menu)))),
        printf "    │            %-3s pts            │   " (take 3 $ show (pScore (getBestPlayer (currentMatch menu)))),
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │                               │   ",
        "    │       partida finalizada      │   ",
        "    └───────────────────────────────┘   "
    ], boxBefore = StartMenu, action = FinishMatch}


{- Recebe o menu atual, o index da página de listagem, e pega a lista de
 partidas criadas pra chamar a função _updateMatchesMenu, que faz a
 lógica de atualização da paǵina -}
updateMatchesMenu :: Menu -> Int -> IO(Menu)
updateMatchesMenu menu indexMatch = do
  matches <- getMatches
  let namesMatches = map mName matches
      lengthList = length namesMatches
      numPages = _numPagesMatches lengthList
  if indexMatch >= numPages then
     return menu
     else return (_updateMatchesMenu menu namesMatches indexMatch lengthList numPages)


{- Função interna que recebe a lista de partidas criadas e o index da página de listagem,
 e retorna um array de string contendo os 5 nomes de partidas a serem exibidos -}
_geraMatchLines :: [String] -> Int -> [String]
_geraMatchLines namesMatches indexMatch =
    let names = take 5 $ drop (5 * indexMatch) namesMatches
        maxLength = maximum (map length names)
        paddedNames = map (\name -> name ++ replicate (maxLength - length name) ' ') names
        boxMiddle = map (\name -> printf "    │       %-10s              │" name) paddedNames
        emptyLines = replicate (5 - length names) "    │                               │"
    in boxMiddle ++ emptyLines


{- Função interna que recebe o menu atual, a lista de todas as partidas criadas, o index
da página de listagem, e atualiza a box do menu listando as cinco partidas
a serem exibidas para o usuário -}
_updateMatchesMenu :: Menu -> [String] -> Int -> Int -> Int -> Menu
_updateMatchesMenu menu namesMatches indexMatch lengthList numPages =
    menu {
        box = [
            "    ┌───────────────────────────────┐   ",
            "    │  1. voltar        2. avançar  │   ",
            "    │                               │   ",
            "    │           PALAVRÃO            │   ",
            "    │                               │   "
        ] ++ _geraMatchLines namesMatches indexMatch ++ [
            "    │                               │   ",
      printf"    │      total de partidas: %-2s    │   " (show lengthList),
            "    │                               │   ",
      printf"    │                        %-2s/ %-2s │   " (show (indexMatch + 1)) (show numPages),
            "    └───────────────────────────────┘   "
        ],
        boxBefore = ContinueGame,
        action = Matches, indexMatch = indexMatch
    }


{- Função interna que recebe o numero de partidas criadas e calcula quantas páginas são
necessárias para listar (listando cinco partidas por tela)-}
_numPagesMatches :: Int -> Int
_numPagesMatches numMatches = (roundNumMatches numMatches) `div` 5
  where
    roundNumMatches num
        | num `mod` 5 == 0 = num
        | otherwise = num + (5 - num `mod` 5)


-- Função interna que gera as linhas do rank, preenchendo com as 5 contas com maior pontuação,
-- e preenchendo os espaços em branco caso tenham menos de 5 contas registradas
-- Recebe: menu com accRank, que será utilizada para a formatação das strings
-- Retorna: array de strings formatada com as informações do rank de contas
_getRankLines :: Menu -> [String]
_getRankLines menu = accs ++ blankLines
    where
        accs = ["    │     " ++ show i ++ printf ". %-5s  -  %-4s         │   " (take 5 $ accName acc) (take 4 $ show (accScore acc)) | (acc, i) <- zip (take 5 $ reverse (accsRank menu)) [1..5]]
        blankLines = replicate (5 - (length (accsRank menu))) "    │                               │"

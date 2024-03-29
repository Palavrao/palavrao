{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use print" #-}
{-# HLINT ignore "Use forM_" #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Core.Menu where

import Text.Printf
import System.Console.ANSI
import System.Exit
import Data.Char
import Data.Aeson
import Interface.BoxesMenu
import System.IO
import Controllers.AccountsController
import Controllers.MatchesController
import Data.Time.Clock (getCurrentTime)
import Utils.Utils as UT
import Utils.Validator as VL
import Core.Game
import Controllers.PlayerController (Player(pScore))



{-  Função recursiva que fica atualizando o menu de acordo com o que recebe de entrada
 Recebe: menu que ficará sendo atualiado a cada loop -}
menuLoop :: Menu -> IO ()
menuLoop menu = do
    _drawMenu menu
    userInput <- getLine
    updatedMenu <- _menuFlux menu userInput
    menuLoop updatedMenu


{-  Função interna que desenha o menu na tela
 Recebe: menu que será desenhado na tela -}
_drawMenu :: Menu -> IO ()
_drawMenu menu = do
    clearScreen
    mapM_ putStrLn (box menu)


{-  Função interna que gerencia o fluxo de comandos e telas do menu
 Recebe: menu que será atualizado
 Recebe: input do usuário
 Retorna: menu atualizado de acordo com o input do usuário -}
_menuFlux :: Menu -> String -> IO Menu
_menuFlux menu input = do
    case action menu of
        -- Tela inicial do menu
        StartMenu -> case input of
            -- Redirecionamento para tela de criar partida caso as 2 contas estejam logadas
            -- ou para a tela de criar conta caso contrário
            "1" -> do
                if _accsFull menu then do
                    return (updateMenu RegisterMatch menu)
                else do
                    return (updateMenu NewGame menu)
            -- Redirecionamento para tela de continuar jogos
            "2" -> return (updateMenu ContinueGame menu)
            -- Redirecionamento para tela de criar conta caso 2 contas não estejam logadas
            -- ou nada caso já esteja no limite de login
            "3" ->
                if _accsFull menu then do
                    return (updateMenu (action menu) menu)
                else do
                    return (updateMenu Register menu)
            -- Redirecionamento para tela de regras
            "4" -> return (updateMenu Rules menu)
            -- Redirecionamento para tela de rank de contas e suas pontuações, atualizando
            -- o rank armazenado pelo menu
            "5" -> do
                updatedMenu <- _getRank menu
                return updatedMenu
            -- Saída do jogo
            "6" -> exitSuccess >> return menu
            -- Caso base o qual somente repete a tela atual
            _   -> return (updateMenu (action menu) menu)

        -- Tela de novo jogo
        NewGame -> case input of
            -- Redirecionamento para tela de criar conta
            "1" -> return (updateMenu Register menu)
            -- Redirecionamento para tela de login
            "2" -> return (updateMenu Login menu)
            -- Redirecionamento para tela anterior
            "3" -> return (updateMenu (boxBefore menu) menu)
            -- Caso base o qual somente repete a tela atual
            _   -> return (updateMenu (action menu) menu)

        -- Tela de login
        Login -> case input of
            -- Lógica de login, retornando para tela de menu inicial caso 2 jogadores sejam
            -- logados, ou para tela de novo jogo caso contrário
            "1" -> do
                updatedMenu <- _login menu
                return updatedMenu
            -- Redirecionamento para tela anterior
            "2" -> return (updateMenu (boxBefore menu) menu)
            -- Caso base o qual somente repete a tela atual
            _   -> return (updateMenu (action menu) menu)

        -- Tela de registro de conta
        Register -> case input of
            -- Lógica de registro de conta, retornando para tela de menu inicial caso 2 jogadores sejam
            -- logados, ou para tela de novo jogo caso contrário
            "1" -> do
                updatedMenu <- _createAcc menu
                return updatedMenu
            -- Redirecionamento para tela anterior
            "2" -> return (updateMenu (boxBefore menu) menu)
            -- Caso base o qual somente repete a tela atual
            _   -> return (updateMenu (action menu) menu)

        -- documentar
        ContinueGame -> case input of
            "1" -> do
                matchMenu <- _continueGame menu
                _loadMatch matchMenu
            "2" -> do
                let updatedMenu = updateMatchesMenu menu 0
                updatedMenu
            "3" -> return (updateMenu (boxBefore menu) menu)
            _   -> return (updateMenu (action menu) menu)

        -- Tela de regras, que retorna para a tela anterior com qualquer input
        Rules -> return (updateMenu (boxBefore menu) menu)

        -- Tela antes do jogo
        BeforeGame -> case input of
            "1" -> _loadMatch menu
            "2" -> do
                let updatedMenu = menu {p1 = p2 menu}
                let updatedMenu' = _updateAccs updatedMenu Account{accName = ""}
                return (updateMenu (boxBefore menu) updatedMenu')
            -- Redirecionamento para tela anterior removendo o login de player 2
            "3" -> do
                let updatedMenu = _updateAccs menu Account{accName = ""}
                return (updateMenu (boxBefore menu) updatedMenu)
            -- Caso base o qual somente repete a tela atual
            _   -> return (updateMenu (action menu) menu)

        -- Tela de criação partida
        RegisterMatch -> case input of
            -- Lógica de criação de partida, redirecionando o menu para tela de
            -- antes do jogo caso a partida criada seja válida
            "1" -> do
                updatedMenu <- _createMatch menu
                return updatedMenu
            -- Redirecionamento para tela anterior
            "2" -> return (updateMenu (boxBefore menu) menu)
            -- Caso base o qual somente repete a tela atual
            _   -> return (updateMenu (action menu) menu)

        -- Tela de rank das contas e suas pontuações, retornando ao menu inicial
        -- com qualquer input do usuário
        Rank -> return (updateMenu (boxBefore menu) menu)

        -- documentar
        {-Matches -> case input of
            "1" -> do
                let idxMatch = (indexMatch menu) + 1
                    updatedMenu = updateMatchesMenu menu idxMatch
                updatedMenu
            "2" -> do
                let idxMatch = (indexMatch menu) - 1
                    updatedMenu <- if idxMatch == 0
                                   then return (updateMenu (boxBefore menu) menu)
                                   else updateMatchesMenu menu idxMatch
                return updatedMenu
            _ -> return menu-}

        -- Tela de finalização do jogo, mostrando os dados finais da partida,
        -- retornando ao menu inicial com qualquer input do usuário
        FinishMatch -> return (updateMenu (boxBefore menu) beginGame)

        -- Caso base o qual so retorna
        _ -> return (updateMenu (action menu) menu)

{-  Redirecionamento para o jogo, passando o stattime e a lista de palavras
 disponíveis pro jogo, atualizando o menu com os dados da partida e
 retornando para a tela de finalização do jogo caso o jogo tenha acabado
 ou para tela inicial caso o jogo tenha sido pausado -}
_loadMatch :: Menu -> IO (Menu)
_loadMatch menu = do
    wordList <- UT.getWordList
    startTime <- getCurrentTime
    updatedMatch <- gameLoop (currentMatch menu) wordList startTime ""
    updatedMenu <- _menuPauseOrEnd menu updatedMatch
    return updatedMenu

{-  Função interna que retornará um menu atualizado com informações do termino da partida, caso
 a partida tenha terminado, ou com o menu inicial caso o jogo tenha sido pausado
 Recebe: menu que será atualizado de acordo se o jogo terminou ou pausou
 Recebe: partida atualizada com informações de quando o jogo terminou ou pausou
 Retorna: menu atualizado com a tela de start menu sem informações de players logados
 caso a partida tenha sido pausada, ou com a tela de finish match mostrando as informações
 finais do jogo, caso a partida tenha sido terminada -}
_menuPauseOrEnd :: Menu -> Match -> IO Menu
_menuPauseOrEnd menu updatedMatch = do
    match <- getMatchByName (mName updatedMatch)
    case match of
        Just match -> do
            return (updateMenu StartMenu beginGame)
        Nothing    -> do
            let updatedMenu = _updateCurrentMatch menu updatedMatch
            return (updateMenu FinishMatch updatedMenu)


{-  Função interna que verifica se a quantidade de players logados no menu está em 2, cheia ou não
 Recebe: menu que terá a quantidade de players analisada
 Retorna: boolean que será false caso a quantidade de players logados seja 1 e 2 caso contrário -}
_accsFull :: Menu -> Bool
_accsFull menu = accName (p2 menu) /= ""


{-  Função interna que criará uma partida se a partida não já tiver sido criada com esse nome
 Recebe: menu que terá a partida criada e armazenada
 Retorna: menu com a partida criada e com a tela de before game -}
_createMatch :: Menu -> IO Menu
_createMatch menu = do
    putStr "nome_da_partida> "
    hFlush stdout
    newMatchName <- getLine
    if newMatchName == "2" || newMatchName == "" then do
        return menu
    else do
        matchExists <- VL.matchExistsValidation newMatchName
        if matchExists then do
            putStrLn "partida ja registrada"
            _createMatch menu
        else do
            match <- createMatch newMatchName (p1 menu) (p2 menu)
            let updatedMatch = _updateCurrentMatch menu match
            return $ updateMenu BeforeGame updatedMatch


{-  Função interna que faz o login de uma conta no menu e o retorna, verificando a existência da conta passada
 e se a quantidade de contas máxima foi atingida
 Recebe: menu que terá a conta logada
 Retorna: menu com a conta logada e com a tela de start menu caso tiver atingido 2 players ou para a tela
 de newgame caso tenha atingido apenas 1 player logado -}
_login :: Menu -> IO Menu
_login menu = do
    putStr "nome_de_usuario> "
    hFlush stdout
    accName <- getLine
    if accName == "2" || accName == "" then do
        return menu
    else do
        accExists <- VL.accExistsValidation accName
        if not accExists then do
            putStrLn "nome não registrado"
            _login menu
        else do
            acc <- getAccByName accName
            case acc of
                Just acc -> do
                    if acc == p1 menu then do
                        putStrLn "usuario ja logado"
                        _login menu
                    else do
                        let updatedAccs = _updateAccs menu acc
                        if _accsFull updatedAccs then do
                            putStrLn "quantidade de contas máxima atingida"
                            return $ updateMenu StartMenu updatedAccs
                        else do
                            return $ updateMenu NewGame updatedAccs


{-  Função interna que cria uma conta no menu e o retorna, verificando se o nome passado ja existe em alguma conta
 Recebe: menu que terá a conta criada e logada
 Retorna: menu com a conta criada e logada, com a tela de start menu caso tiver atingido 2 players ou para a tela
 de newgame caso tenha atingido apenas 1 player logado -}
_createAcc :: Menu -> IO Menu
_createAcc menu = do
    putStr "nome_de_usuario> "
    hFlush stdout
    newAccName <- getLine
    if newAccName == "2" || newAccName == "" then do
        return menu
    else do
        accExists <- VL.accExistsValidation newAccName
        if accExists then do
                putStrLn "nome já registrado"
                _createAcc menu
        else do
            acc <- createAcc newAccName
            let updatedAccs = _updateAccs menu acc
            if _accsFull updatedAccs then do
                return $ updateMenu StartMenu updatedAccs
            else do
                return $ updateMenu NewGame updatedAccs


{-  Função interna que atualiza a partida que o menu está armazenando
 Recebe: menu que terá a partida armazenada
 Recebe: match que será armazenada no menu
 Retorna: menu com sua match substituída pela nova passada como argumento -}
_updateCurrentMatch :: Menu -> Match -> Menu
_updateCurrentMatch menu match = menu {currentMatch = match}

{-  Recebe: menu atual do jogo
 Retorna: menu contendo a partida a ser continuada -}
_continueGame :: Menu -> IO Menu
_continueGame menu = do
  putStr "nome_da_partida> "
  hFlush stdout
  matchName <- getLine
  maybeMatch <- getMatchByName matchName
  case maybeMatch of
    Just match -> return $ (_updateCurrentMatch menu match)
    Nothing    -> do
               putStrLn "partida não encontrada"
               _continueGame menu

{-  Função interna que retorna o menu com as contas logadas atualizadas
 Recebe: menu que terá contas logadas atualizadas
 Recebe: conta que será logada no menu
 Retorna: menu com a conta do player adicionada como jogador -}
_updateAccs :: Menu -> Account -> Menu
_updateAccs menu acc
  | p1 menu == acc = menu
  | accName (p1 menu) == "" = menu {p1 = acc}
  | otherwise = menu {p2 = acc}


{-  Função interna que retorna o menu atualizado com a tela de rank das contas
 Recebe: menu que terá o rank atualizado
 Retorna: menu com o rank atualizado -}
_getRank :: Menu -> IO Menu
_getRank menu = do
    accs <- getAccRank
    let updatedRank = _updateRank menu accs
    return $ updateMenu Rank updatedRank


{-  Função interna que atualiza o rank de contas do menu
 Recebe: menu que terá o rank atualizado
 Recebe: array das 5 contas com melhores scores do json
 Retorna: menu com o rank substituído pelo array -}
_updateRank :: Menu -> [Account] -> Menu
_updateRank menu accs = menu {accsRank = accs}

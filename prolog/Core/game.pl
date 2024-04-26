


gameLoop(MatchName, WordList, LastMessage, LastState):-
    Match = match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, MatchTimer, MatchSkips),

    ((MatchSkips =:= 4 ; length(MatchLetters, 0)) -> 
    finish_match(MatchName),!,.;

    clear_screen,
    writeln(LastMessage),
    writeln("> Enter para ver o tabuleiro do jogador da vez!\n\n"),
    
    )
    
        -- Mostra a tela de transição e aguarda continuação
        clearScreen
        UT.__colorText lastMessage Green
        hFlush stdout
        UT.__colorText "> Enter para ver o tabuleiro do jogador da vez!\n\n" Blue
        hFlush stdout
        c <- getLine

        -- Mostra a tela de jogo 
        printBoard match
        UT.__colorText ("Turno de: " ++ (map toUpper (accName (pAcc (getPlayerOnTurn match))))) Blue
        putStr "\nDigite sua palavra no formato X00 V/H PALAVRA:\n > "
        hFlush stdout
        
        -- Recebe o input do jogador e valida
        input <- getLine

        -- Jogador pausou a partida e saiu para o menu
        if input == ":C" || input == ":c" then do 
            UT.__colorText "\n >> Pausando e saindo do jogo...\n" Green
            UT.__colorText " > Aperte enter...\n\n" Blue
            c <- getLine
            return match
        -- Jogador jogou uma palavra ou outra ação especial
        else do 
            currentTime <- getCurrentTime
            let elapsed = realToFrac (currentTime `diffUTCTime` lastUpdate) :: NominalDiffTime
            let updatedTimer = mTimer match - realToFrac elapsed

            -- Se tiver acabado o tempo não registra a palavra jogada
            if updatedTimer <= 0 then do
                let updatedMatch = toggleMatchTurn match
                updateMatchJson updatedMatch
                gameLoop updatedMatch wordList currentTime "\nTempo de rodada excedido!\n"
            
            -- Se estiver dentro do tempo recebe a palavra ou comando e os processa
            else do
                (m, msg) <- fluxHandler match wordList input
                if getPlayerOnTurn match /= getPlayerOnTurn m then do
                    updateMatchJson m
                    gameLoop m wordList currentTime msg
                else do 
                    let updatedMatch = updateMatchTimer m updatedTimer
                    threadDelay 100000
                    updateMatchJson updatedMatch
                    gameLoop updatedMatch wordList currentTime msg
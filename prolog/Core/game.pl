
gameLoop(MatchName, LastMessage):-
    writeln(1),
    match_exists(MatchName),
    match(MatchName, _, _, _, _, MatchLetters, _, MatchTimer, MatchSkips),
    writeln(2),
    get_turn_player_name(MatchName, PlayerOnTurn),

    ((MatchSkips == 4 ; length(MatchLetters, 0)) -> 
        (   
            write('>> 4 skips ou trocas! Encerrando o jogo...\n\nAperte Enter para ver o placar...\n\n'),
            no_period_input(_),
            finish_match(MatchName)); 
            (
            % Mostra a tela de transição e aguarda continuação
            string_upper(PlayerOnTurn, PlayerOnTurnUpper),
            writeln(3),
            clear_screen,
            writeln(LastMessage),
            format('> Enter para ver o tabuleiro de ~w!\n\n\n', [PlayerOnTurnUpper]),
            no_period_input(I),
            clear_screen,
            
            % Mostra a tela de jogo 
            print_board(MatchName),
            nl,
            writef('Turno de: %w\n',[PlayerOnTurnUpper]),
            now(StartTime),
            writef('Digite sua palavra no formato X00 V/H PALAVRA:\n > '),

            % Recebe o input do jogador e valida
            no_period_input(UserPlayString),

            % Jogador pausou a partida e saiu para o menu
            ((UserPlayString == ':C'; UserPlayString == ':c') -> 
                (
                    writef('\n >> Pausando e saindo do jogo...\n'),
                    writef(' > Aperte enter...\n\n'),
                    no_period_input(_),
                    !
                ); 
            
            % Jogador jogou uma palavra ou outra ação especial
                (   
                    now(CurTime),
                    ((
                        too_long(StartTime, CurTime),
                        toggle_player_turn(MatchName),
                        gameLoop(MatchName, '\nTempo de rodada excedido!\n'),!
                    ) ; 
                        write('hasTime')),
                        flux_handler(MatchName, UserPlayString, Msg),
                        writeln('flux handled'),
                        gameLoop(MatchName, Msg),!
                )
            )
        )
    ).
/* 
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
            
 */

printShortRules:- 
    write('\nPontuações especiais:\n\n' ),

    write('    * '),
    writeln('-> Dobra a pontuação da letra sobre a célula.'),
    write('    ! '),
    writeln('-> Triplica a pontuação da letra sobre a célula.'),
    write('    - '),
    writeln('-> Dobra a pontuação de toda a palavra cuja letra está sobre a célula.'),
    write('    # '),
    writeln('-> Triplica a pontuação de toda a palavra cuja letra está sobre a célula.\n'),
    
    write('Bingo! ' ),
    write('Se um jogador usar 7 letras para formar uma nova palavra, a pontuação dela é incrementada em 20 pontos.\n\n'),
    write('Fim de jogo:\n\n'),
    write('  O jogo termina quando não há mais peças no saco ou os jogadores realizam, em conjunto, 4 trocas de peças ou saltos de vez seguidos. Em caso de empate, o jogador com a menor soma na pontuação das letras em sua mão vence.\n\n'),
    write('Enter para voltar\n\n'),
    no_period_input(_).

flux_handler(MatchName, ':!', Msg):-
    get_turn_player_name(MatchName, PlayerOnTurn),
    skip_player_turn(MatchName),
    format(atom(Msg), '>> ~w pulou o turno!\n', [PlayerOnTurn]).

flux_handler(MatchName, ':?', ''):-
    printShortRules.

flux_handler(_, ':*', '').

flux_handler(MatchName, S, 'Msg'):- 
    write(here),
    no_period_input(_),
    prefix(':*', S), 
    string_chars(S, SL), 
    nth0(2, SL, L),
    get_turn_player_name(MatchName, P),
    get_player_letters(MatchName, P, PlayerLetters),
    (letter_score(L, Score) > -1, member(L, PlayerLetters),) -> 
        (write(' > Escolha uma letra válida \n:*'),
        no_period_input(I),
        format(atom(A), '*:~w', [I]),
        flux_handler());
        (write('hi')),!.



flux_handler(_,_,'placeholder'):-writeln('placeholder').
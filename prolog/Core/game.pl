
gameLoop(MatchName, LastMessage):-
    writeln(1),
    match(MatchName, _, _, _, _, MatchLetters, _, MatchTimer, MatchSkips),
    writeln(2),
    get_turn_player_name(MatchName, PlayerOnTurn),

    ((MatchSkips == 4 ; length(MatchLetters, 0)) -> finish_match(MatchName); 
            (
            % Mostra a tela de transição e aguarda continuação
            writeln(3),
            clear_screen,
            writeln(LastMessage),
            writeln('> Enter para ver o tabuleiro do jogador da vez!\n\n'),
            no_period_input(I),
            clear_screen,
            
            % Mostra a tela de jogo 
            print_board(MatchName),
            nl,
            string_upper(PlayerOnTurn, PlayerOnTurnUpper),
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
                        write('hasTime')),!
                )
            )
        )
    ).

        
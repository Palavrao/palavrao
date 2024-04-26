:- include('../Utils/utils.pl').
:- include('../Controllers/board_controller.pl').
:- include('../Controllers/matches_controller.pl').
:- include('../Controllers/accs_controller.pl').
:- include('../Controllers/letters_controller.pl').
:- include('../Controllers/players_controller.pl').




gameLoop(MatchName, WordList, LastMessage):-
    Match = match(MatchName, _, _, _, _, MatchLetters, _, MatchTimer, MatchSkips),
    get_turn_player_name(MatchName, PlayerOnTurn),

    ((MatchSkips == 4 ; length(MatchLetters, 0)) -> finish_match(MatchName); 
            (
            % Mostra a tela de transição e aguarda continuação
            clear_screen,
            writeln(LastMessage),
            writeln('> Enter para ver o tabuleiro do jogador da vez!\n\n'),
            no_period_input(_),
            
            % Mostra a tela de jogo 
            buildBoard(MatchName),
            string_upper(PlayerOnTurn, PlayerOnTurnUpper),
            writef('Turno de: %w\n',[PlayerOnTurnUpper]),
            writef('\nDigite sua palavra no formato X00 V/H PALAVRA:\n > '),

            % Recebe o input do jogador e valida
            no_period_input(UserPlayString),

            % Jogador pausou a partida e saiu para o menu
            ((UserPlayString == ':C'; UserPlayString == ':c') -> 
                (
                    writef('\n >> Pausando e saindo do jogo...\n'),
                    writef(' > Aperte enter...\n\n'),
                    no_period_input(_)
                ); 
            
            % Jogador jogou uma palavra ou outra ação especial
                (   
                    now(CurTime),
                    ((
                        too_long(MatchTimer, CurTime),
                        toggle_player_turn(MatchName),
                        gameLoop(MatchName, WordList, '\nTempo de rodada excedido!\n')
                    ) ; 
                        write('hasTime'))
                )
            )
        )
    ).

        
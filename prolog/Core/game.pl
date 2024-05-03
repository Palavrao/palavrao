
game_loop(MatchName, LastMessage):-
    match_exists(MatchName),
    match(MatchName, _, _, _, _, MatchLetters, _, MatchTimer, MatchSkips),
    get_turn_player_name(MatchName, PlayerOnTurn),
    update_player_letters(MatchName),
    ((MatchSkips == 4 ; length(MatchLetters, 0)) -> 
        (   
            ansi_format([bold, fg(green)], '>> 4 skips ou trocas! Encerrando o jogo...\n\n',[]),
            ansi_format([bold, fg(blue)], 'Aperte Enter para ver o placar...\n\n',[]),
            no_period_input(_),
            finish_match(MatchName)
        );(

            % Mostra a tela de transição e aguarda continuação
            string_upper(PlayerOnTurn, PlayerOnTurnUpper),
            ansi_format([bold, fg(blue)], '~w' , [LastMessage]),
            ansi_format([bold, fg(green)], '> Enter para ver o tabuleiro de ~w!\n\n\n', [PlayerOnTurnUpper]),
            no_period_input(I),
            
            % Mostra a tela de jogo 
            print_board(MatchName),
            now(StartTime),
            writef('Digite sua palavra no formato X00 V/H PALAVRA:\n > '),

            % Recebe o input do jogador e valida
            no_period_input(UserPlayString),

            % Jogador pausou a partida e saiu para o menu
            ((UserPlayString == ':C'; UserPlayString == ':c') -> 
                (
                    ansi_format([bold, fg(green)], '\n >> Pausando e saindo do jogo...\n',[]),
                    ansi_format([bold, fg(blue)], ' > Aperte enter...\n\n',[]),
                    no_period_input(_),
                    !
                ); 
                
                % Jogador jogou uma palavra ou outra ação especial
                (   
                    now(CurTime),
                    (
                        (
                            too_long(StartTime, CurTime),
                            toggle_player_turn(MatchName),
                            game_loop(MatchName, '\nTempo de rodada excedido!\n'),!
                        ) ; (
                            flux_handler(MatchName, UserPlayString, Msg, StartTime),
                            game_loop(MatchName, Msg),!
                        )
                    )
                )
            )
        )
    ).


flux_handler(MatchName, ':!', Msg,_):-
    get_turn_player_name(MatchName, PlayerOnTurn),
    skip_player_turn(MatchName),
    format(atom(Msg), '>> ~w pulou o turno!\n', [PlayerOnTurn]).

flux_handler(MatchName, ':?', Msg,_):-
    print_short_rules,
    print_board(MatchName),
    writef('Digite sua palavra no formato X00 V/H PALAVRA:\n > '),
    no_period_input(I),
    flux_handler(MatchName, I, Msg).

flux_handler(_, ':*', '',_).

flux_handler(MatchName, S, Msg,_):- 
    sub_string(S, 0, 2, _, ':*'), 
    string_chars(S, SL),
    nth0(2, SL, L),
    get_turn_player_name(MatchName, P),
    get_player_letters(MatchName, P, PlayerLetters),
    letter_score(L, Score),

    ((
        Score > -1, member(L, PlayerLetters),
        format(atom(Msg), ' >> ~w trocou a letra ~w!\n\n', [P, L]),
        switch_player_letter(MatchName, L),
        skip_player_turn(MatchName)
    );(
        ansi_format([bold, fg(red)],' > Escolha uma letra válida \n', []),
        write(':*'),
        no_period_input(I),
        format(atom(A), ':*~w', [I]),
        flux_handler(MatchName, A, Msg))), !.


flux_handler(MatchName,StringInput, Msg, StartTime):-
    validation(MatchName, StringInput, [true, Points, UsedLetters, [], []]),
    now(CurTime),
    ((too_long(StartTime,CurTime),
    Msg = '\nTempo de rodada excedido!\n'),!;
    (remove_player_letters(MatchName, UsedLetters),
    inc_player_score(MatchName, Points),
    get_match_board_name(MatchName, BoardName),
    update_cur_tiles(BoardName),
    reset_match_skips(MatchName),
    format(atom(Msg), '\nPalavra válida! Pontos: ~d\n', [Points]))),
    toggle_player_turn(MatchName), !.


flux_handler(MatchName,StringInput, Msg, StartTime):-
    validation(MatchName, StringInput, [false|_]),
    now(CurTime),
    ((too_long(StartTime,CurTime),
        Msg = '\nTempo de rodada excedido!\n',
        toggle_player_turn(MatchName), !),!
        ;
    (get_match_board_name(MatchName, BoardName),
    reset_work_tiles(BoardName),
    ansi_format([bold, fg(red)], '\nJogada inválida, tente novamente: \n', []),
    write('Digite sua palavra no formato X00 V/H PALAVRA:\n > '),
    no_period_input(I),
    flux_handler(MatchName, I, Msg, StartTime))),!.

flux_handler(MatchName,StringInput, Msg):-
    validation(MatchName, StringInput, [_, _, _, Invalidletters, _]),
    get_match_board_name(MatchName, BoardName),
    reset_work_tiles(BoardName),
    ansi_format([bold, fg(red)], '\nVocê não tem as letras: ~w, tente novamente: \n', [InvalidLetters]),
    write('Digite sua palavra no formato X00 V/H PALAVRA:\n > '),
    no_period_input(I),
    flux_handler(MatchName, I, Msg), !.

flux_handler(MatchName,StringInput, Msg):-
    validation(MatchName, StringInput,[ _, _, _, _, InvalidWords]),
    get_match_board_name(MatchName, BoardName),
    reset_work_tiles(BoardName),
    ansi_format([bold, fg(red)], '\nPalavras inválidas: ~w, tente novamente: \n', [InvalidWords]),
    write('Digite sua palavra no formato X00 V/H PALAVRA:\n > '),
    no_period_input(I),
    flux_handler(MatchName, I, Msg), !.


flux_handler(_,_,'Pânico geral!').



game_loop(MatchName, LastMessage):-
    match_exists(MatchName),
    match(MatchName, _, _, _, _, MatchLetters, _, MatchTimer, MatchSkips),
    get_turn_player_name(MatchName, PlayerOnTurn),
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
                            flux_handler(MatchName, UserPlayString, Msg),
                            game_loop(MatchName, Msg),!
                        )
                    )
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
                 updatedMatch wordList currentTime "\nTempo de rodada excedido!\n"
            
            -- Se estiver dentro do tempo recebe a palavra ou comando e os processa
            else do
                (m, msg) <- fluxHandler match wordList input
                if getPlayerOnTurn match /= getPlayerOnTurn m then do
                    updateMatchJson m
                     m wordList currentTime msg
                else do 
                    let updatedMatch = updateMatchTimer m updatedTimer
                    threadDelay 100000
                    updateMatchJson updatedMatch
                     updatedMatch wordList currentTime msg
            
 */

print_short_rules:- 
    ansi_format([bold, fg(green)],' \nPontuações especiais:\n\n', []),

    ansi_format([bold, fg(blue)], '    * ', []),
    write('-> Dobra a pontuação da letra sobre a célula.\n'),
    ansi_format([bold, fg(green)], '    ! ', []),
    write('-> Triplica a pontuação da letra sobre a célula.\n'),
    ansi_format([bold, fg(magenta)], '    - ', []),
    write('-> Dobra a pontuação de toda a palavra cuja letra está sobre a célula.\n'),
    ansi_format([bold, fg(red)],'    # ',[]),
    write('-> Triplica a pontuação de toda a palavra cuja letra está sobre a célula.\n\n'),
    
    ansi_format([bold, fg(magenta)], 'Bingo! ', []),
    write('Se um jogador usar 7 letras para formar uma nova palavra, a pontuação dela é incrementada em 20 pontos.\n\n'),
    ansi_format([bold, fg(green)],'Fim de jogo:\n\n',[]),
    write('  O jogo termina quando não há mais peças no saco ou os jogadores realizam, em conjunto, 4 trocas de peças ou saltos de vez seguidos. Em caso de empate, o jogador com a menor soma na pontuação das letras em sua mão vence.\n\n'),
    ansi_format([bold, fg(blue)],'Enter para voltar\n\n',[]),
    no_period_input(_).

flux_handler(MatchName, ':!', Msg):-
    get_turn_player_name(MatchName, PlayerOnTurn),
    skip_player_turn(MatchName),
    format(atom(Msg), '>> ~w pulou o turno!\n', [PlayerOnTurn]).

flux_handler(MatchName, ':?', Msg):-
    print_short_rules,
    print_board(MatchName),
    writef('Digite sua palavra no formato X00 V/H PALAVRA:\n > '),
    no_period_input(I),
    flux_handler(MatchName, I, Msg).

flux_handler(_, ':*', '').

% flux_handler(MatchName, S, Msg):- 
%     sub_string(S, 0, 2, _, ':*'), 
%     string_chars(S, SL),
%     nth0(2, SL, L),
%     get_turn_player_name(MatchName, P),
%     get_player_letters(MatchName, P, PlayerLetters),
%     letter_score(L, Score),

%     ((
%         Score > -1, member(L, PlayerLetters),
%         format(atom(Msg), ' >> ~w trocou a letra ~w!\n\n', [P, L]),
%         switch_player_letter(MatchName, L),
%         skip_player_turn(MatchName)
%     );(
%         ansi_format([bold, fg(red)],' > Escolha uma letra válida \n', []),
%         write(':*'),
%         no_period_input(I),
%         format(atom(A), ':*~w', [I]),
%         flux_handler(MatchName, A, Msg))), !.


flux_handler(MatchName,StringInput, Msg):-
    validation(MatchName, StringInput, [true, Points, UsedLetters, [], []]),
    remove_player_letters(MatchName, UsedLetters),
    inc_player_score(MatchName, Points),
    get_match_board_name(MatchName, BoardName),
    update_cur_tiles(BoardName),
    format(atom(Msg), '\nPalavra válida! Pontos: ~d\n', [Points]),
    toggle_player_turn(MatchName), !.


flux_handler(MatchName,StringInput, Msg):-
    validation(MatchName, StringInput, [false|_]),
    get_match_board_name(MatchName, BoardName),
    reset_work_tiles(BoardName),
    ansi_format([bold, fg(red)], '\nCoordenada ou Formatação inválidas, tente novamente: \n', []),
    write('Digite sua palavra no formato X00 V/H PALAVRA:\n > '),
    no_period_input(I),
    flux_handler(MatchName, I, Msg), !.

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



print_matrix(_, []).
print_matrix(MatchName, [Row|Rest]) :-
    write('  '),
    print_list(Row),
    length(Rest, L),
    I is 14 - L,
    format('~|~`0t~d~2+', [I]),
    suffix(MatchName, I, S),
    write(S),
    nl,
    print_matrix(MatchName, Rest).

print_list([]).
print_list([X|Xs]) :-
    write(X),
    write(' '),
    print_list(Xs).


suffix(MatchName, 1, ''):-
    match(MatchName, _, _, P1Name, P2Name, _, _, _, _),
    sub_string(P1Name, 0, 5, _, P1Name5),
    sub_string(P2Name, 0, 5, _, P2Name5),
    format('    ~w.     ~w.', [P1Name5, P2Name5]).

suffix(MatchName, 2, ''):-
    match(MatchName, _, _, P1Name, P2Name, _, _, _, _),
    get_player_score(MatchName, P1Name, ScoreP1),
    get_player_score(MatchName, P2Name, ScoreP2),
    format('    ~|~`0t~d~3+ pt     ~|~`0t~d~3+ pt    ', [ScoreP1, ScoreP2]).

suffix(_, 11, '   :C   sair            ').
suffix(_, 12, '   :?   manual          ').
suffix(_, 13, '   :!   pular vez       ').
suffix(_, 14, '   :*X  trocar letra x  ').
suffix(_, _, '').


letterScoreArray(Letters, Scores):- maplist(letter_score, Letters, Scores).

print_board(MatchName):-
    match(MatchName, BoardName, MatchTurn, _, _, RemainingLetters, _, _, _),
    getCurTiles(BoardName, CurTiles),
    get_turn_player_name(MatchName, TurnPlayerName),
    player(MatchName, TurnPlayerName, TurnPlayerLetters, _),
    length(RemainingLetters, RL),
    writef('  PARTIDA: %20r\n\n',[MatchName]),
    %writef('%w - %w pts            %w - %w pts\n\n', [P1Name, ScoreP1, P2Name, ScoreP2]),
    writef('  A B C D E F G H I J K L M N O\n'),
    print_matrix(MatchName, CurTiles),
    writef('\n\n'),
    print_list(TurnPlayerLetters),
    writef('       Letras Restantes: %w',[RL]),
    nl,
    letterScoreArray(TurnPlayerLetters, LetterScores),
    print_list(LetterScores),nl,!.


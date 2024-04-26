

print_matrix([]).
print_matrix([Row|Rest]) :-
    write('     '),
    print_list(Row),
    nl,
    print_matrix(Rest).

print_list([]).
print_list([X|Xs]) :-
    write(X),
    write(' '),
    print_list(Xs).



letterScoreArray(Letters, Scores):-maplist(letter_score, Letters, Scores).

print_board(MatchName):-
    Match = match(MatchName, BoardName, MatchTurn, P1Name, P2Name, RemainingLetters, _, _, _),
    getCurTiles(BoardName, CurTiles),
    get_turn_player_name(MatchName, TurnPlayerName),
    player(MatchName, TurnPlayerName, TurnPlayerLetters, _),
    get_player_score(MatchName, P1Name, ScoreP1),
    get_player_score(MatchName, P2Name, ScoreP2),
    length(RemainingLetters, RL),
    writef('Letras Restantes: %w\n',[RL]),
    writef('%w - %w pts            %w - %w pts\n\n', [P1Name, ScoreP1, P2Name, ScoreP2]),
    writef('     A B C D E F G H I J K L M N O\n'),
    print_matrix(CurTiles),
    writef('\n\n'),
    print_list(TurnPlayerLetters),
    letterScoreArray = letterScoreArray(TurnPlayerLetters, LetterScores),
    print_list(LetterScores).

dummyPrint(_):-
    create_board(dummy),
    getCurTiles(dummy, CurTiles),

    writef('Letras Restantes: %w\n',[0]),
    writef('%w - %w pts            %w - %w pts\n\n', [player1, 000, player2, 111]),
    writef('     A B C D E F G H I J K L M N O\n\n'),
    print_matrix(CurTiles),
    writef('\n\n'),
    print_list([a,b,c,d,e,f,g]),
    nl,
    letterScoreArray = letterScoreArray(['a',b,c,d,e,f,g], LetterScores),
    print_list(LetterScores).
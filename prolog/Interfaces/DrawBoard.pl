include:-

print_board([]).
print_board([Row|Rest]) :-
    write('     '),
    print_row(Row),
    nl,
    print_board(Rest).

print_row([]).
print_row([X|Xs]) :-
    write(X),
    write(' '),
    print_row(Xs).

buildBoard(MatchName):-
    writef('~w - ~w pts            ~w - ~w pts\n\n', [P1, ScoreP1, P2, ScoreP2]),
    writef('     A B C D E F G H I J K L M N O\n'),
    print_board(Board).

testing:-

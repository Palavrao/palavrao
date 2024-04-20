use_module(library(clpfd)).

startBoard(CurTiles, WorkTiles):-   CurTiles = [['#', '~', '~', '*', '~', '~', '~', '#', '~', '~', '~', '*', '~', '~', '#'],
                                                ['~', '-', '~', '~', '~', '!', '~', '~', '~', '!', '~', '~', '~', '-', '~'],
                                                ['~', '~', '-', '~', '~', '~', '*', '~', '*', '~', '~', '~', '-', '~', '~'],
                                                ['*', '~', '~', '-', '~', '~', '~', '*', '~', '~', '~', '-', '~', '~', '*'],
                                                ['~', '~', '~', '~', '-', '~', '~', '~', '~', '~', 'S', '~', '~', '~', '~'],
                                                ['~', '!', '~', '~', 'C', 'A', 'S', 'A', '~', 'S', 'A', '~', '~', '!', '~'],
                                                ['~', '~', '*', '~', '~', 'V', '*', '~', '*', 'O', 'L', 'A', '*', '~', '~'],
                                                ['#', '~', '~', '*', '~', 'E', 'S', 'C', 'O', 'L', 'A', '*', '~', '~', '#'],
                                                ['~', '~', '*', '~', '~', '~', '*', '~', '*', '~', '~', '~', '*', '~', '~'],
                                                ['~', '!', '~', '~', '~', '!', '~', '~', '~', '!', '~', '~', '~', '!', '~'],
                                                ['~', '~', '~', '~', '-', '~', '~', '~', '~', '~', '-', '~', '~', '~', '~'],
                                                ['*', '~', '~', '-', '~', '~', '~', '*', '~', '~', '~', '-', '~', '~', '*'],
                                                ['~', '~', '-', '~', '~', '~', '*', '~', '*', '~', '~', '~', '-', '~', '~'],
                                                ['~', '-', '~', '~', '~', '!', '~', '~', '~', '!', '~', '~', '~', '-', '~'],
                                                ['#', '~', '~', '*', '~', '~', '~', '#', '~', '~', '~', '*', '~', '~', '#']]
                                                
                                                ,
                                   WorkTiles = [['#', '~', '~', '*', '~', '~', '~', '#', '~', '~', '~', '*', '~', '~', '#'],
                                                ['~', '-', '~', '~', '~', '!', '~', '~', '~', '!', '~', '~', '~', '-', '~'],
                                                ['~', '~', '-', '~', '~', '~', '*', '~', '*', '~', '~', '~', '-', '~', '~'],
                                                ['*', '~', '~', '-', '~', '~', '~', '*', '~', '~', '~', '-', '~', '~', '*'],
                                                ['~', '~', '~', '~', '-', '~', '~', '~', '~', '~', '-', '~', '~', '~', '~'],
                                                ['~', '!', '~', '~', '~', '!', '~', '~', '~', '!', '~', '~', '~', '!', '~'],
                                                ['~', '~', '*', '~', '~', '~', '*', '~', '*', '~', '~', '~', '*', '~', '~'],
                                                ['#', '~', '~', '*', '~', '~', '~', '-', '~', '~', '~', '*', '~', '~', '#'],
                                                ['~', '~', '*', '~', '~', '~', '*', '~', '*', '~', '~', '~', '*', '~', '~'],
                                                ['~', '!', '~', '~', '~', '!', '~', '~', '~', '!', '~', '~', '~', '!', '~'],
                                                ['~', '~', '~', '~', '-', '~', '~', '~', '~', '~', '-', '~', '~', '~', '~'],
                                                ['*', '~', '~', '-', '~', '~', '~', '*', '~', '~', '~', '-', '~', '~', '*'],
                                                ['~', '~', '-', '~', '~', '~', '*', '~', '*', '~', '~', '~', '-', '~', '~'],
                                                ['~', '-', '~', '~', '~', '!', '~', '~', '~', '!', '~', '~', '~', '-', '~'],
                                                ['#', '~', '~', '*', '~', '~', '~', '#', '~', '~', '~', '*', '~', '~', '#']].


transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).


getElement(Matrix, X, Y, Element) :-
    nth0(Y, Matrix, Row),
    nth0(X, Row, Element).

setElement(Matrix, X,Y, Element, NewMatrix):-
    nth0(Y, Matrix, Row, RemainderRows),
    nth0(X, Row, _, RemainderLine),
    nth0(X, ResultLine, Element, RemainderLine),
    nth0(Y,NewMatrix,ResultLine, RemainderRows).

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


length_1(X) :-
    string_length(X, 1).

getWords(Matrix, Words):- getWordsHorizontal(Matrix, W1), getWordsVertical(Matrix,W2), append(W1,W2,Words).

getWordsHorizontal([], []).
getWordsHorizontal([Row|Rest], NewList):- 
    atomic_list_concat(Row, StringRow), 
    atomic_list_concat(Words0, ' ', StringRow), 
    exclude(=(''), Words0, Words1),
    exclude(length_1, Words1, Words2),
    getWordsHorizontal(Rest, RestWords),
    append(Words2,RestWords, NewList).

getWordsVertical([],[]).
getWordsVertical(M, Words):-
    transpose(M,M2),
    getWordsHorizontal(M2,Words).
    

replaceToken(Token,' '):- member(Token, [!,-,~,*,-,#]).
replaceToken(Token,Token).

replaceTokens([], []).
replaceTokens([Token|Rest], [R|Result]) :-
    replaceToken(Token, R),
    replaceTokens(Rest, Result).

replaceTokensMatrix([], []).
replaceTokensMatrix([Row|Rest], [NewRow|NewRest]) :-
    replaceTokens(Row, NewRow),
    replaceTokensMatrix(Rest, NewRest).

placeWord(X, Y, IsHorizontal, Word0, InitialBoard, ResultBoard) :-
    atom_chars(Word0, Word),
    (   IsHorizontal
    ->  placeLetters(true, X, Y, Word, InitialBoard, ResultBoard)
    ;   placeLetters(false, X, Y, Word, InitialBoard, ResultBoard)
    ).

placeLetters(_, _, _, [], Board, Board).
placeLetters(true, X, Y, [H|T], Board, ResultBoard) :-
    X1 is X + 1,
    setElement(Board, X, Y, H, NewBoard),
    placeLetters(true, X1, Y, T, NewBoard, ResultBoard).

placeLetters(false, X, Y, [H|T], Board, ResultBoard) :-
    Y1 is Y + 1,
    setElement(Board, Y, X, H, NewBoard),
    placeLetters(false, X, Y1, T, NewBoard, ResultBoard).


getTiles(IsHorizontal, Matrix, X, Y, N, Elements) :-
    nth0(X, Matrix, Column),
    sublist(IsHorizontal, Column, Y, N, Elements).


sublist(List, Start, End, Sublist) :-
    length(Prefix, Start),
    append(Prefix, Rest, List),
    length(Sublist, EndStart),
    append(Sublist, _, Rest),
    End is EndStart + Start.


main:-
    startBoard(B,_),
    placeWord(0,0,false,"house",B,N),
    print_board(N),
    getVertical(N, 0,0,0, E),
    print(E),
    getVertical(N, 0,0,5, E),
    print(E).
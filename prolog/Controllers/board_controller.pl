:- include('../Utils/utils.pl').
:- include('../Constants/paths.pl').
:- dynamic(board/3).


create_board(BoardName) :-
    board(BoardName, _,_);
    CurTiles = [['#', '~', '~', '*', '~', '~', '~', '#', '~', '~', '~', '*', '~', '~', '#'],
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
    WorkTiles =[['#', '~', '~', '*', '~', '~', '~', '#', '~', '~', '~', '*', '~', '~', '#'],
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
                ['#', '~', '~', '*', '~', '~', '~', '#', '~', '~', '~', '*', '~', '~', '#']],
    
    boards_path(BoardsPath),
    
    inc_fact_file(BoardsPath, board(BoardName, CurTiles, WorkTiles)).

getCurTiles(BoardName, CurTiles):-
    board(BoardName, CurTiles, _).

getWorkTiles(BoardName, WorkTiles):-
    board(BoardName, _, WorkTiles).

getBoard(BoardName, Board):-
    board(BoardName, CurTiles, WorkTiles),
    Board = board(BoardName, CurTiles, WorkTiles).

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
    setElement(Board, X,Y, H, NewBoard),
    placeLetters(false, X, Y1, T, NewBoard, ResultBoard).


getTiles(true, Matrix, X, Y, N, Elements) :-
    nth0(Y, Matrix, Line),
    slice2(Line,X,N,R1),
    reverse(R1,Elements).


getTiles(false, Matrix, X, Y, N, Elements) :-
    transpose(Matrix, Matrix2),
    nth0(X, Matrix2, Column),
    slice2(Column,Y,N,R1), 
    reverse(R1,Elements).

slice2(List, StartIndex, 1, [R1]):- nth0(StartIndex, List, R1),!.
    
slice2(List, StartIndex, N, [R1|R3]):-
    N2 is N -1,
    IDX is StartIndex + N2, 
    nth0(IDX, List, R1),
    slice2(List, StartIndex, N2, R3).


getPointsWord(Tiles, Word, Score):- 
    (wordBonuses(Tiles, Word, WB)), 
    (getPointsLetter(Tiles, Word, PL)), 
    (bingo(Tiles, B)), 
    Score is WB * PL + B.


getPointsLetter([], [], 0).
getPointsLetter(['*'|TBoard], [HWord|TWord], Points) :-
    letterValue(HWord, LetterValue),
    getPointsLetter(TBoard, TWord, PointsTail),
    Points is 2 * LetterValue + PointsTail.

getPointsLetter(['!'|TBoard], [HWord|TWord], Points) :-
    letterValue(HWord, LetterValue),
    getPointsLetter(TBoard, TWord, PointsTail),
    Points is 3 * LetterValue + PointsTail.

getPointsLetter([HBoard|TBoard], [HWord|TWord], Points) :-
    letterValue(HWord, LetterValue),
    getPointsLetter(TBoard, TWord, PointsTail),
    Points is LetterValue + PointsTail.


wordBonuses([], [], 1). % Base case: empty lists contribute a bonus of 1
wordBonuses(['-'|TBoard], [HWord|TWord], Bonus) :-
    wordBonuses(TBoard, TWord, BonusTail), % Recursively calculate the bonus for the rest of the lists
    Bonus is 2 * BonusTail. % Double the bonus

wordBonuses(['#'|TBoard], [HWord|TWord], Bonus) :-
    wordBonuses(TBoard, TWord, BonusTail), % Recursively calculate the bonus for the rest of the lists
    Bonus is 3 * BonusTail. % Triple the bonus

wordBonuses([HBoard|TBoard], [HWord|TWord], Bonus) :-
    letterValue(HWord, LetterValue),
    wordBonuses(TBoard, TWord, BonusTail),
    Bonus is BonusTail.

main:-
    create_board(name),
    getCurTiles(name,C),
    placeWord(10,1,true,word,C,R),
    print_board(C),
    write("\n"),
    print_board(R),
    write("\n"),
    placeWord(0,1,false,word,R,R2),
    write("\n"),
    print_board(R2),
    getTiles(true, R2, 10, 1, 4, Elements),
    write(Elements),
    getTiles(false, R2, 0, 2, 4, Elements2),
    write(Elements2).





    
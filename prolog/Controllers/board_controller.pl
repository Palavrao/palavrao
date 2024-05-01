board_exists(BoardName) :-
    current_predicate(board/3),
    board(BoardName, _, _), !.


create_board(BoardName) :-
    \+ board_exists(BoardName),
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
                ['#', '~', '~', '*', '~', '~', '~', '#', '~', '~', '~', '*', '~', '~', '#']],
    
    boards_path(BoardsPath),
    
    inc_fact_file(BoardsPath, board(BoardName, CurTiles, WorkTiles), board).

get_cur_tiles(BoardName, CurTiles):-
    board(BoardName, CurTiles, _).

get_work_tiles(BoardName, WorkTiles):-
    board(BoardName, _, WorkTiles).

get_board(BoardName, Board):-
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


get_element(Matrix, X, Y, Element) :-
    nth0(Y, Matrix, Row),
    nth0(X, Row, Element).

set_element(Matrix, X,Y, Element, NewMatrix):-
    nth0(Y, Matrix, Row, RemainderRows),
    nth0(X, Row, _, RemainderLine),
    nth0(X, ResultLine, Element, RemainderLine),
    nth0(Y,NewMatrix,ResultLine, RemainderRows).



length_1(X) :-
    string_length(X, 1).

get_words(Matrix, Words):-
    replace_tokens_matrix(Matrix, Rep),
    get_wordsHorizontal(Rep, W1), get_wordsVertical(Rep,W2), append(W1,W2,Words), !.

get_wordsHorizontal([], []).
get_wordsHorizontal([Row|Rest], NewList):- 
    atomic_list_concat(Row, StringRow), 
    atomic_list_concat(Words0, ' ', StringRow), 
    exclude(=(''), Words0, Words1),
    exclude(length_1, Words1, Words2),
    get_wordsHorizontal(Rest, RestWords),
    append(Words2,RestWords, NewList).

get_wordsVertical([],[]).
get_wordsVertical(M, Words):-
    transpose(M,M2),
    get_wordsHorizontal(M2,Words).
    

replace_token(Token,' '):- member(Token, [!,-,~,*,-,#]).
replace_token(Token,Token).

replace_tokens([], []).
replace_tokens([Token|Rest], [R|Result]) :-
    replace_token(Token, R),
    replace_tokens(Rest, Result).

replace_tokens_matrix([], []).
replace_tokens_matrix([Row|Rest], [NewRow|NewRest]) :-
    replace_tokens(Row, NewRow),
    replace_tokens_matrix(Rest, NewRest).

place_word(X, Y, IsHorizontal, Word0, InitialBoardName, ResultBoard) :-
    get_work_tiles(InitialBoardName, WorkTiles),
    atom_chars(Word0, Word),
    (   IsHorizontal
    ->  place_letters(true, X, Y, Word, WorkTiles, ResultBoard)
    ;   place_letters(false, X, Y, Word, WorkTiles, ResultBoard)
    ).

place_letters(_, _, _, [], Board, Board).
place_letters(true, X, Y, [H|T], Board, ResultBoard) :-
    X1 is X + 1,
    set_element(Board, X, Y, H, NewBoard),
    place_letters(true, X1, Y, T, NewBoard, ResultBoard).

place_letters(false, X, Y, [H|T], Board, ResultBoard) :-
    Y1 is Y + 1,
    set_element(Board, X,Y, H, NewBoard),
    place_letters(false, X, Y1, T, NewBoard, ResultBoard).

update_cur_tiles(InitialBoardName):-
    boards_path(BoardsPath),
    get_board(InitialBoardName, B),
    get_work_tiles(InitialBoardName, WorkTiles),
    NewBoard = board(InitialBoardName, WorkTiles, WorkTiles),
    update_fact_file(BoardsPath, B, NewBoard, board).

update_work_tiles(InitialBoardName, NewWorkTiles):-
    boards_path(BoardsPath),
    get_board(InitialBoardName, B),
    get_cur_tiles(InitialBoardName, CurTiles),
    NewBoard = board(InitialBoardName, CurTiles, NewWorkTiles),
    update_fact_file(BoardsPath, B, NewBoard, board).

reset_work_tiles(InitialBoardName):-
    boards_path(BoardsPath),
    get_board(InitialBoardName, B),
    get_cur_tiles(InitialBoardName, CurTiles),
    NewBoard = board(InitialBoardName, CurTiles, CurTiles),
    update_fact_file(BoardsPath, B, NewBoard, board).

get_tiles(true, Matrix, X, Y, N, Elements) :-
    nth0(Y, Matrix, Line),
    slice2(Line,X,N,R1),
    reverse(R1,Elements).


get_tiles(false, Matrix, X, Y, N, Elements) :-
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


get_points_word(Tiles, Word, Score):- 
    (word_bonuses(Tiles, Word, WB)), 
    (get_points_letter(Tiles, Word, PL)), 
    (bingo(Tiles, B)), 
    Score is WB * PL + B.


get_points_letter([], [], 0).
get_points_letter(['*'|TBoard], [HWord|TWord], Points) :-
    letter_score(HWord, LetterScore),
    get_points_letter(TBoard, TWord, PointsTail),
    Points is 2 * LetterScore + PointsTail.

get_points_letter(['!'|TBoard], [HWord|TWord], Points) :-
    letter_score(HWord, LetterScore),
    get_points_letter(TBoard, TWord, PointsTail),
    Points is 3 * LetterScore + PointsTail.

get_points_letter([_|TBoard], [HWord|TWord], Points) :-
    letter_score(HWord, LetterScore),
    get_points_letter(TBoard, TWord, PointsTail),
    Points is LetterScore + PointsTail.


word_bonuses([], [], 1).
word_bonuses(['-'|TBoard], [_|TWord], Bonus) :-
    word_bonuses(TBoard, TWord, BonusTail), 
    Bonus is 2 * BonusTail. 

word_bonuses(['#'|TBoard], [_|TWord], Bonus) :-
    word_bonuses(TBoard, TWord, BonusTail),
    Bonus is 3 * BonusTail.

word_bonuses([_|TBoard], [_|TWord], Bonus):-
    word_bonuses(TBoard, TWord, BonusTail),
    Bonus is BonusTail.


bingo(Tiles, Score) :-
    count_played_letters(Tiles, PlayedLetters),
    (PlayedLetters > 6 -> Score = 20 ; Score = 0).


count_played_letters(Tiles, PlayedLetters) :-
    include(is_empty_tile, Tiles, PlayedTiles),
    write(PlayedTiles),
    length(PlayedTiles, PlayedLetters).


is_empty_tile(Char) :-
    member(Char, ['-','~','#','!','*']).

testing:-
    create_board(name),
    get_cur_tiles(name,C),
    place_word(10,1,true,word,C,R),
    print_board(C),
    write("\n"),
    print_board(R),
    write("\n"),
    place_word(0,1,false,word,R,R2),
    write("\n"),
    print_board(R2),
    get_tiles(true, R2, 10, 1, 4, Elements),
    write(Elements),
    get_tiles(false, R2, 0, 2, 4, Elements2),
    write(Elements2),
    get_points_word([~,~,~,*,~,~,~], ['S','S','S','S','S','S','S'], WP),
    write(WP).





    
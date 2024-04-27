validation(MatchName, InputLine, PortugueseWords) :-
    get_match_board_name(MatchName, BoardName),

    read_input(InputLine, Info),
    nth0(0, Info, X),
    nth0(1, Info, Y),
    nth0(2, Info, WordLetters),
    nth0(3, Info, IsHorizontal),

    % Lógica que verifica se o jogador tem as letras da palavra

    get_turn_player(MatchName, PlayerName),
    get_player_letters(MatchName, PlayerName, PlayerLetters),
    msort(PlayerLetters, LetrasPlayer),
    msort(WordLetters, LetrasWord),
    player_has_letters(LetrasPlayer, LetrasWord),

    % Lógica de validação da palavra

    word_fits_in_space(X, Y, WordLetters, IsHorizontal),
    word_tiles_validation(BoardName, WordLetters, X, Y, IsHorizontal),
    center_tile_validation(BoardName, X, Y, IsHorizontal, WordLetters),
    word_existence_validation(WordLetters, PortugueseWords),
    getCurTiles(BoardName, CurTiles), getWords(CurTiles, BoardWords),
    all_words_exist(BoardWords, PortugueseWords).

read_input(InputLine, Info) :-
    string_upper(InputLine, InputLineUpper),
    split_string(InputLineUpper, " ", "", Input), length(Input, 3),
    nth0(0, Input, InputCoord),
    nth0(1, Input, Direction), (Direction = "H", ! ; Direction = "V"),
    nth0(2, Input, InputWord), string_chars(InputWord, WordLetters), 
    length(WordLetters, Len), (Len >= 2, Len =< 7), maplist(is_alpha, WordLetters),

    string_chars(InputCoord, [H|T]), length([H|T], 3),
    char_code(H, CharCode), 
    nth0(0, T, A1), char_type(A1, digit),
    nth0(1, T, A2), char_type(A2, digit),
    atom_concat(A1, A2, Concat),
    atom_string(Concat, NewCoord),
    
    X is CharCode - 65, (X >= 0, X =< 14),
    number_codes(Y, NewCoord), (Y >= 0, Y =< 14),

    (Direction = "H" ->
        IsHorizontal = true
    ; IsHorizontal = false),

    Info = [X, Y, WordLetters, IsHorizontal].

is_alpha(Char) :-
    char_type(Char, alpha).

player_has_letters(_, []) :- !.
player_has_letters([], _) :- false, !.
player_has_letters([H|T], [H|Ts]) :-
    player_has_letters(T, Ts), !.
player_has_letters([_|T], WordLetters) :-
    player_has_letters(T, WordLetters).

word_fits_in_space(X, Y, WordLetters, IsHorizontal) :-
    length(WordLetters, WordLength), 
    
    (IsHorizontal -> 
        (X =< 15 - WordLength)
    ; (Y =< 15 - WordLength)).

word_tiles_validation(BoardName, WordLetters, X, Y, IsHorizontal) :-
    getCurTiles(BoardName, CurTiles), 
    length(WordLetters, WordLength),

    (IsHorizontal -> 
        To is X + WordLength,
        take_up_to(CurTiles, X, To, Y, IsHorizontal, SublistTiles)
    ; To is Y + WordLength,
        take_up_to(CurTiles, Y, To, X, IsHorizontal, SublistTiles)),

    letter_overlap_validation(WordLetters, SublistTiles),
    not(maplist(is_alpha, SublistTiles)).

letter_overlap_validation([], []) :- !.
letter_overlap_validation([H|T], [H|Y]) :-
    letter_overlap_validation(T, Y), !.
letter_overlap_validation([_|T], [X|Y]) :-
    not(is_alpha(X)),
    letter_overlap_validation(T, Y), !.

take_up_to(_, To, To, _, _, []) :- !.
take_up_to(Matrix, From, To, ListIndex, IsHorizontal, Sublist) :-
    (IsHorizontal ->
        nth0(ListIndex, Matrix, List),
        nth0(From, List, Char)
    ; nth0(From, Matrix, List),
        nth0(ListIndex, List, Char)),

    NewFrom is From + 1,
    take_up_to(Matrix, NewFrom, To, ListIndex, IsHorizontal, TempSublist),
    append([Char], TempSublist, Sublist).

center_tile_validation(BoardName, X, Y, IsHorizontal, WordLetters) :-
    getCurTiles(BoardName, CurTiles),
    take_up_to(CurTiles, 7, 8, 7, [CenterTile]),
    length(WordLetters, WordLength),

    ((is_alpha(CenterTile), !) ; 
    
    (IsHorizontal -> 
        WordLastInd is X + WordLength - 1,
        Y =:= 7, X =< 7, WordLastInd >= 7
    ; WordLastInd is Y + WordLength - 1,
        X =:= 7, Y =< 7, WordLastInd >= 7)).

word_existence_validation(WordLetters, PortugueseWords) :-
    atom_string(WordLetters, WordUpper),
    string_lower(WordUpper, Word),
    member(Word, PortugueseWords).

all_words_exist(BoardWords, PortugueseWords) :-
    subset(BoardWords, PortugueseWords).
validator(MatchName, InputLine) :-
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
    word_tiles_validator(BoardName, WordLetters, X, Y, IsHorizontal).

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

word_fits_in_space(X, _, WordLetters, true) :-
    length(WordLetters, Len), (X =< 15 - Len), !.
word_fits_in_space(_, Y, WordLetters, false) :-
    length(WordLetters, Len), (Y =< 15 - Len).

word_tiles_validator(CurTiles, WordLetters, X, Y, true) :-
    length(WordLetters, WordLen),
    % getCurTiles(BoardName, CurTiles), 
    To is X + WordLen,
    get_sublist_row(CurTiles, X, To, Y, SublistTiles),
    letter_overlap_validator(WordLetters, SublistTiles), not(maplist(is_alpha, SublistTiles)), !.
word_tiles_validator(CurTiles, WordLetters, X, Y, false) :-
    length(WordLetters, WordLen),
    % getCurTiles(BoardName, CurTiles), 
    To is Y + WordLen,
    take_sublist_col(CurTiles, Y, To, X, SublistTiles),
    letter_overlap_validator(WordLetters, SublistTiles), not(maplist(is_alpha, SublistTiles)).

letter_overlap_validator([], []) :- !.
letter_overlap_validator([H|T], [H|Y]) :-
    letter_overlap_validator(T, Y), !.
letter_overlap_validator([_|T], [X|Y]) :-
    not(is_alpha(X)),
    letter_overlap_validator(T, Y), !.

get_sublist_row(_, To, To, _, []) :- !.
get_sublist_row(Matrix, From, To, Y, Sublist) :-
    nth0(Y, Matrix, Row),
    nth0(From, Row, Char),
    NewFrom is From + 1,
    get_sublist_row(Matrix, NewFrom, To, Y, TempSublist),
    append([Char], TempSublist, Sublist).

get_sublist_col(_, To, To, _, []) :- !.
get_sublist_col(Matrix, From, To, X, Sublist) :-
    nth0(From, Matrix, Row),
    nth0(X, Row, Char),
    NewFrom is From + 1,
    get_sublist_col(Matrix, NewFrom, To, X, TempSublist),
    append([Char], TempSublist, Sublist).

valid_name(Name) :-
    \+ Name = "",
    \+ contains_space(Name),
    \+ contains_special_chars_helper(Name).

contains_space(Name) :-
    sub_string(Name, _, _, _, ' ').

contains_special_chars(Name) :-
    writeln("Checking for special characters"),
    atom_chars(Name, Chars),
    contains_special_chars_helper(Chars).

contains_special_chars_helper([]).
contains_special_chars_helper([Char|Rest]) :-
    special_char(Char),
    !.
contains_special_chars_helper([_|Rest]) :-
    contains_special_chars_helper(Rest).

special_char(Char) :-
    member(Char, ['!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '-', '_', '=', '+', '[', ']', '{', '}', ';', ':', "'", '"', '<', '>', ',', '.', '/', '?', '\\', '|',
                  'á', 'à', 'ã', 'â', 'é', 'è', 'ẽ', 'ê', 'í', 'ì', 'ĩ', 'î', 'ó', 'ò', 'õ', 'ô', 'ú', 'ù', 'ũ', 'û', 'ç']).

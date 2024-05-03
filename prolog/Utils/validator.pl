report(0, Report) :- Report = [false, 0, [], [], []].
report(1, Points, ValidLetters, InvalidLetters, InvalidWords, Report) :- Report = [true, Points, ValidLetters, InvalidLetters, InvalidWords], !.

validation(MatchName, InputLine, Report) :-
    get_match_board_name(MatchName, BoardName),

    (read_input(InputLine, Info) ->

        nth0(0, Info, X),
        nth0(1, Info, Y),
        nth0(2, Info, WordLetters),
        nth0(3, Info, IsHorizontal),

        % Lógica que verifica se o jogador tem as letras da palavra
        get_turn_player_name(MatchName, PlayerName),
        get_player_letters(MatchName, PlayerName, PlayerLetters),
        length(WordLetters, WordLength),

        (IsHorizontal -> N is X + WordLength ; N is Y + WordLength), get_work_tiles(BoardName, WorkTiles),
        take_up_to(WorkTiles, X, Y, N, IsHorizontal, BoardTiles),
        player_has_letters(WordLetters, PlayerLetters, BoardTiles, ValidLetters, InvalidLetters),
        get_points_word(BoardTiles, WordLetters, Points),

        % Lógica de validação da palavra

        ((word_fits_in_space(X, Y, WordLetters, IsHorizontal)) ->
            ((word_tiles_validation(WorkTiles, WordLetters, X, Y, IsHorizontal) )->

                (center_tile_validation(WorkTiles, X, Y, IsHorizontal, WordLetters) ->

                    atomic_list_concat(WordLetters, Word),
                    place_word(X, Y, IsHorizontal, Word, BoardName, NewBoard),
                    get_words(NewBoard, BoardWords),

                    (all_words_exist(BoardWords, InvalidWords) ->
                        report(1, Points, ValidLetters, InvalidLetters, InvalidWords, Report),
                        update_work_tiles(BoardName, NewBoard),!
                    )
                )
            )
        )
    )

    ;

    report(0, Report).

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

player_has_letters([], _, _, [], []).
player_has_letters([WL|WLs], PlayerLetters, [WL|BTs], ValidLetters, InvalidLetters) :-
    player_has_letters(WLs, PlayerLetters, BTs, ValidLetters, InvalidLetters), !.
player_has_letters([WL|WLs], PlayerLetters, [_|BTs], ValidLetters, InvalidLetters) :-
    (member(WL, PlayerLetters) ->
        select(WL, PlayerLetters, NewPlayerLetters), !
    ; member('<', PlayerLetters) ->
        select('<', PlayerLetters, NewPlayerLetters)),
    player_has_letters(WLs, NewPlayerLetters, BTs, TempValidLetters, InvalidLetters),
    ValidLetters = [WL|TempValidLetters], !.
player_has_letters([WL|WLs], _, [_|BTs], ValidLetters, InvalidLetters) :-
    player_has_letters(WLs, _, BTs, ValidLetters, TempInvalidLetters),
    InvalidLetters = [WL|TempInvalidLetters].

word_fits_in_space(X, Y, WordLetters, IsHorizontal) :-
    length(WordLetters, WordLength),

    (IsHorizontal ->
        (X =< 15 - WordLength)
    ; (Y =< 15 - WordLength)).

word_tiles_validation(WorkTiles, WordLetters, X, Y, IsHorizontal) :-
    length(WordLetters, WordLength),

    (IsHorizontal ->
        N is X + WordLength
    ;
        N is Y + WordLength),

    take_up_to(WorkTiles, X, Y, N, IsHorizontal, SublistTiles),
    letter_overlap_validation(WordLetters, SublistTiles),
    not(maplist(is_alpha, SublistTiles)).

letter_overlap_validation([], []) :- !.
letter_overlap_validation([H|T], [H|Y]) :-
    letter_overlap_validation(T, Y), !.
letter_overlap_validation([_|T], [X|Y]) :-
    not(is_alpha(X)),
    letter_overlap_validation(T, Y), !.

take_up_to(_, N, _, N, true, []) :- !.
take_up_to(_, _, N, N, false, []) :- !.
take_up_to(Matrix, X, Y, N, IsHorizontal, BoardTiles) :-
    nth0(Y, Matrix, List),
    nth0(X, List, Char),

    (IsHorizontal ->
        NewX is X + 1,
        take_up_to(Matrix, NewX, Y, N, IsHorizontal, TempBoardTiles)
    ;
        NewY is Y + 1,
        take_up_to(Matrix, X, NewY, N, IsHorizontal, TempBoardTiles)),

    append([Char], TempBoardTiles, BoardTiles).

center_tile_validation(WorkTiles, X, Y, IsHorizontal, WordLetters) :-
    take_up_to(WorkTiles, 7, 7, 8, IsHorizontal, [CenterTile]),
    length(WordLetters, WordLength),

    ((is_alpha(CenterTile), !) ;

    (IsHorizontal ->
        WordLastInd is X + WordLength - 1,
        Y =:= 7, X =< 7, WordLastInd >= 7
    ; WordLastInd is Y + WordLength - 1,
        X =:= 7, Y =< 7, WordLastInd >= 7)).

all_words_exist([], []).
all_words_exist([H|T], InvalidWords) :-
    all_words(AW),
    downcase_atom(H, Lowercase),
    (memberchk(Lowercase,AW) ->
        all_words_exist(T, InvalidWords), !
    ;
        all_words_exist(T, TempInvalidWords),
        InvalidWords = [H|TempInvalidWords]), !.

get_sublist_col(_, To, To, _, []) :- !.
get_sublist_col(Matrix, From, To, X, Sublist) :-
    nth0(From, Matrix, Col),
    nth0(X, Col, Char),
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
contains_special_chars_helper([Char|_]) :-
    special_char(Char),
    !.
contains_special_chars_helper([_|Rest]) :-
    contains_special_chars_helper(Rest).

special_char(Char) :-
    member(Char, ['!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '-', '_', '=', '+', '[', ']', '{', '}', ';', ':', "'", '"', '<', '>', ',', '.', '/', '?', '\\', '|',
                  'á', 'à', 'ã', 'â', 'é', 'è', 'ẽ', 'ê', 'í', 'ì', 'ĩ', 'î', 'ó', 'ò', 'õ', 'ô', 'ú', 'ù', 'ũ', 'û', 'ç']).

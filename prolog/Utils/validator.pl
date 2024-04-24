validator(MatchName, InputLine) :-
    read_input(InputLine, Info),
    nth0(3, Info, WordLetters),

    % Lógica que verifica se o jogador tem as letras da palavra

    get_turn_player(MatchName, PlayerName),
    get_player_letters(MatchName, PlayerName, PlayerLetters),
    msort(PlayerLetters, LetrasPlayer),
    msort(WordLetters, LetrasWord),
    player_has_letters(LetrasPlayer, LetrasWord).

player_has_letters(_, []) :- !.
player_has_letters([], _) :- false, !.
player_has_letters([H|T], [H|Y]) :-
    player_has_letters(T, Y), !.
player_has_letters([_|T], WordLetters) :-
    player_has_letters(T, WordLetters).

read_input(InputLine, Info) :-
    string_upper(InputLine, InputLineUpper),
    split_string(InputLineUpper, " ", "", Input), length(Input, 3),
    nth0(0, Input, InputCoord),
    nth0(1, Input, Direction), (Direction = "H", ! ; Direction = "V"),
    nth0(2, Input, InputWord), string_chars(InputWord, WordLetters), maplist(is_alpha, WordLetters),

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

    Info = [X, Y, IsHorizontal, WordLetters].

is_alpha(Char) :-
    char_type(Char, alpha).
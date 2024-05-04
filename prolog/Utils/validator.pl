report(0, Report) :- Report = [false, 0, [], [], []].
report(1, Points, ValidLetters, InvalidLetters, InvalidWords, Report) :- Report = [true, Points, ValidLetters, InvalidLetters, InvalidWords], !.


% Encapsula lógica de validação
% Recebe: o nome de uma match
% Recebe: uma string de input
% Retorna: uma lista com: [True se a coordenada e posicionamento forem válidos,
%                            A quantidade de pontos obtidos,
%                            Uma lista de letras usadas pelo jogador,
%                            Uma lista de letras que o jogador não possui,
%                            Uma lista de palavra inválidas]
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
        get_element(WorkTiles, 7, 7, MiddleElement),
        (not(maplist(special_char, BoardTiles)) ; special_char(MiddleElement)),
        player_has_letters(WordLetters, PlayerLetters, BoardTiles, ValidLetters, InvalidLetters),
        not(maplist(is_alpha, BoardTiles)), letter_overlap_validation(WordLetters, BoardTiles), 
        get_points_word(BoardTiles, WordLetters, Points),

        % Lógica de validação da palavra

        ((word_fits_in_space(X, Y, WordLetters, IsHorizontal)) ->
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

    ;

    report(0, Report).

% Encapsula o tratamento da linha de entrada do jogador
% Recebe a string de entrada
% Retorna: uma lista com as informações tratadas: [Coordenada X, Coordenada Y, 
%                                                   Uma lista com os caracteres da palavra,
%                                                   True se a direção é horizontal]
read_input(InputLine, Info) :-
    string_upper(InputLine, InputLineUpper),
    split_string(InputLineUpper, " ", "", Input), length(Input, 3),
    nth0(0, Input, InputCoord),
    nth0(1, Input, Direction), (Direction = "H", ! ; Direction = "V"),
    nth0(2, Input, InputWord), string_chars(InputWord, WordLetters), 
    length(WordLetters, Len), Len >= 2, maplist(is_alpha, WordLetters),

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

% Verifica se o jogador tem as letras da palavra jogada
% Recebe: A lista com os caracteres da palavra
% Recebe: As letras do jogador
% Recebe: Uma sublista do espaço do tabuleiro em que a palavra possivelmente será inserida
% Retorna: Uma lista de letras válidas utilizadas
% Retorna: Uma lista de letras inválidas
player_has_letters([], _, _, [], []).
player_has_letters([WL|WLs], PlayerLetters, [WL|BTs], ValidLetters, InvalidLetters) :-
    player_has_letters(WLs, PlayerLetters, BTs, ValidLetters, InvalidLetters), !.
player_has_letters([WL|WLs], PlayerLetters, [_|BTs], ValidLetters, InvalidLetters) :-
    (member(WL, PlayerLetters) ->
        select(WL, PlayerLetters, NewPlayerLetters),
        player_has_letters(WLs, NewPlayerLetters, BTs, TempValidLetters, InvalidLetters),
        ValidLetters = [WL|TempValidLetters], !
    ; member('<', PlayerLetters) ->
        select('<', PlayerLetters, NewPlayerLetters),
        player_has_letters(WLs, NewPlayerLetters, BTs, TempValidLetters, InvalidLetters),
        ValidLetters = ['<'|TempValidLetters]), !.
player_has_letters([WL|WLs], _, [_|BTs], ValidLetters, InvalidLetters) :-
    player_has_letters(WLs, _, BTs, ValidLetters, TempInvalidLetters),
    InvalidLetters = [WL|TempInvalidLetters].

% Verifica se o a palavra cabe na coordenada passada
% Recebe: Coordenada X
% Recebe: Coordenada Y
% Recebe: Lista de caracteres da palavra
% Recebe: Booleano para a direção horizontal
word_fits_in_space(X, Y, WordLetters, IsHorizontal) :-
    length(WordLetters, WordLength),

    (IsHorizontal ->
        (X =< 15 - WordLength)
    ; (Y =< 15 - WordLength)).

% Verifica se a sobreposição da palavra sobre o tabuleiro é válida
% Recebe: Lista com os caracteres da palavra
% Recebe: Sublista do espaço do tabuleiro em que a palavra possivelmente será inserida
letter_overlap_validation([], []) :- !.
letter_overlap_validation([H|T], [H|Y]) :-
    letter_overlap_validation(T, Y), !.
letter_overlap_validation([_|T], [X|Y]) :-
    not(is_alpha(X)),
    letter_overlap_validation(T, Y), !.

% Obtém as tiles presentes no board no intervalo especificado
% Recebe: uma matriz
% Recebe: coordenadas x e y
% Recebe: o tamanho do intervalo
% Recebe: o booleano se informa se é horizontal
% Retorna: as tiles no intervalo especificado
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

% Verifica se a primeira jogada utilizou a tile central
% Recebe: uma matriz
% Recebe: s coordenadas X e Y
% Recebe: booleano para a direção horizontal
% Recebe: lista de caracteres da palavra
center_tile_validation(WorkTiles, X, Y, IsHorizontal, WordLetters) :-
    take_up_to(WorkTiles, 7, 7, 8, IsHorizontal, [CenterTile]),
    length(WordLetters, WordLength),

    ((is_alpha(CenterTile), !) ;

    (IsHorizontal ->
        WordLastInd is X + WordLength - 1,
        Y =:= 7, X =< 7, WordLastInd >= 7
    ; WordLastInd is Y + WordLength - 1,
        X =:= 7, Y =< 7, WordLastInd >= 7)).

% Verifica se todas as palavras formas no tabuleiro existem na língua portuguesa
% Recebe: Lista de palavras do tabuleiro
% Retorna: Lista de palavras inválidas
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
    member(Char, ['!', '@', '#', '~', '$', '%', '^', '&', '*', '(', ')', '-', '_', '=', '+', '[', ']', '{', '}', ';', ':', "'", '"', '<', '>', ',', '.', '/', '?', '\\', '|',
                  'á', 'à', 'ã', 'â', 'é', 'è', 'ẽ', 'ê', 'í', 'ì', 'ĩ', 'î', 'ó', 'ò', 'õ', 'ô', 'ú', 'ù', 'ũ', 'û', 'ç']).

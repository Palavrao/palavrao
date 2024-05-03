
clear_screen :- (current_prolog_flag(windows, true) -> shell('cls'); shell('clear')).

make_data_folder :-
    data_path(Path),
    (exists_directory(Path) ;
    make_directory(Path)).


start_persistence :- 
    make_data_folder,
    accs_path(AccsPath),
    matches_path(MatchesPath),
    players_path(PlayersPath),
    boards_path(BoardsPath),
    make_facts_file(AccsPath),
    make_facts_file(BoardsPath),
    make_facts_file(MatchesPath),
    make_facts_file(PlayersPath).


make_facts_file(Path) :-
    exists_file(Path) ; 
    open(Path, write, Stream),
    close(Stream).


save_fact_file(Path, Pred) :-
    tell(Path),
    listing(Pred),
    told.


inc_fact_file(Path, NewFact, Pred) :- 
    assertz(NewFact),
    save_fact_file(Path, Pred).


update_fact_file(Path, CurrentFact, NewFact, Pred) :-
    retract(CurrentFact),
    assertz(NewFact),
    save_fact_file(Path, Pred).


del_fact_file(Path, DelFact, Pred) :-
    retract(DelFact),
    save_fact_file(Path, Pred).


remove_one_element([H|T], 0, H, T) :- !.
remove_one_element([H|T], Quantity, RemovedElement, UpdatedElements) :- 
    NewQuantity is Quantity - 1,
    remove_one_element(T, NewQuantity, RemovedElement, OtherElements),
    UpdatedElements = [H|OtherElements].


pop_random_elements([], _, [], []) :- !.
pop_random_elements(Elements, 0, [], Elements) :- !.
pop_random_elements(Elements, Quantity, [RemovedElement|RemovedElements], UpdatedElements) :- 
    length(Elements, ElementsLen),
    Len is ElementsLen - 1,
    random_between(0, Len, RandIndex),
    remove_one_element(Elements, RandIndex, RemovedElement, TempUpdatedElements),

    NewQuantity is Quantity - 1,
    pop_random_elements(TempUpdatedElements, NewQuantity, RemovedElements, UpdatedElements).


remove_elements([], _, []).
remove_elements([H|T], ToRemove, Updated) :-
    (   member(H, ToRemove) ->
        remove_one_element(ToRemove, _, H, NewToRemove),
        remove_elements(T, NewToRemove, Updated)
    ;   Updated = [H|OtherElements],
        remove_elements(T, ToRemove, OtherElements)
    ).


no_period_input(Input):- 
    read_line_to_codes(user_input, K),
    string_to_atom(K, AtomInput),
    atom_string(AtomInput, StringInput),
    string_upper(StringInput, UpperInput),
    string_to_atom(UpperInput, Input).


too_long(Start,End) :-
    K is End - Start,
    K > 60.


compare_second(>, [_,X], [_,Y]) :- X @> Y.
compare_second(>, [A,X], [B,X]) :- A @> B.
compare_second(<, [_,X], [_,Y]) :- X @< Y.
compare_second(=, [A,X], [A,X]).

sort_by_second(Pairs, Sorted) :-
    predsort(compare_second, Pairs, Sorted), !.


get_last_elements(Elements, Qtd, LastElements) :- length(Elements, Len), Len =:= Qtd, LastElements = Elements.
get_last_elements([_|T], Qtd, LastElements) :- get_last_elements(T, Qtd, LastElements), !.


% Regras do jogo
regras :-
    ansi_format([bold, fg(yellow)],'\nPalavrão! ',[]),
    writeln('é um jogo estratégico double player de formação de palavras em um tabuleiro matricial, baseado no popular "Scrabble".\n'),

    ansi_format([bold, fg(blue)],'Objetivo do jogo:\n',[]),
    writeln('O objetivo do jogo é acumular a maior quantidade de pontos possível a partir da formação de novas palavras horizontal ou verticalmente, e adjacentes às palavras já dispostas no tabuleiro.\n'),

    ansi_format([bold, fg(blue)], 'Funcionamento do jogo:',[]),
    writeln('
    - Cada jogador recebe 7 letras, que podem ser trocadas por letras aleatórias do saco de letras, fazendo-o perder a vez.
    - O primeiro jogador coloca ao menos duas letras no tabuleiro formando a primeira palavra.
    - Em seguida os jogadores tomam turnos adicionando letras adjacentes às letras dispostas no tabuleiro para formar novas palavras.
    - Para formar novas palavras, o jogador deve indicar a coordenada da célula onde deseja adicionar a primeira letra e se a palavra deve ser disposta horizontal ou verticalmente no tabuleiro.
    - O tabuleiro irá entender quando as letras da palavra já estiverem presentes na posição necessária, então basta escrever a palavra normalmente com as coordenadas.
    - Dentre as letras dos jogadores pode haver peças curinga (<), que os permitem utilizar letras que não estão em mãos nem no tabuleiro. Cada letra ausente consome um curinga.
    - A pontuação para cada rodada é a soma dos valores das letras em cada palavra formada ou modificada + os pontos adicionais obtidos de células e ocasiões especiais.
    - As rodadas se alternam com um limite de tempo de 5 minutos para cada jogada. Passado esse tempo, o jogador perde a vez.\n
    - O jogador pode pular a sua vez.
    '),

    ansi_format([bold, fg(blue)], 'Pontuação das letras:\n',[]),
    writeln('
    0   |   < (Curinga)
    1   |   A, E, I, O, S, U, M, R, T
    2   |   D, L, C, P
    3   |   N, B, Ç
    4   |   F, G, H, V
    5   |   J
    6   |   Q
    7   |   X, Z
    '),

    print_short_rules.


print_short_rules:- 
    ansi_format([bold, fg(green)],' \nPontuações especiais:\n\n', []),

    ansi_format([bold, fg(blue)], '    * ', []),
    write('-> Dobra a pontuação da letra sobre a célula.\n'),
    ansi_format([bold, fg(green)], '    ! ', []),
    write('-> Triplica a pontuação da letra sobre a célula.\n'),
    ansi_format([bold, fg(magenta)], '    - ', []),
    write('-> Dobra a pontuação de toda a palavra cuja letra está sobre a célula.\n'),
    ansi_format([bold, fg(red)],'    # ',[]),
    write('-> Triplica a pontuação de toda a palavra cuja letra está sobre a célula.\n\n'),
    
    ansi_format([bold, fg(magenta)], 'Bingo! ', []),
    write('Se um jogador usar 7 letras para formar uma nova palavra, a pontuação dela é incrementada em 20 pontos.\n\n'),
    ansi_format([bold, fg(green)],'Fim de jogo:\n\n',[]),
    write('  O jogo termina quando não há mais peças no saco ou os jogadores realizam, em conjunto, 4 trocas de peças ou saltos de vez seguidos. Em caso de empate, o jogador com a menor soma na pontuação das letras em sua mão vence.\n\n'),
    ansi_format([bold, fg(blue)],'Enter para voltar\n\n',[]),
    no_period_input(_).
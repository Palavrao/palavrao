:- consult('Interfaces/boxes_menu.pl').
:- consult('Utils/validator.pl').
:- consult('Utils/utils.pl').
:- consult('Controllers/matches_controller.pl').
:- consult('Controllers/accs_controller.pl').

:- dynamic(current_screen/1).
:- dynamic(screen/2).
screen(redimension_screen, []).

update_menu(Action, CurrentMenu, UpdatedMenu) :-
    (menu(Action, NovoMenu) -> UpdatedMenu = NovoMenu ; UpdatedMenu = CurrentMenu).

writeln_menu([]).
writeln_menu([Line|Rest]) :-
    writeln(Line),
    writeln_menu(Rest).

show_menu(Action) :-
    clear_screen,
    update_menu(Action, [], UpdatedMenu),
    writeln_menu(UpdatedMenu).

get_input(Key) :-
    get_single_char(Code),
    char_code(Key, Code).

% fluxo de menu inicial
process_input('0') :-
    current_screen(redimension_screen),
    back_to_start_menu.

process_input('1') :-
    current_screen(start_menu),
    retract(current_screen(_)),
    assertz(current_screen(new_game)),
    show_menu(new_game),
    write_new_match(NewMatchName, Player1, Player2).

process_input('2') :-
    current_screen(start_menu),
    retract(current_screen(_)),
    assertz(current_screen(continue_game)),
    show_menu(continue_game).

process_input('3') :-
    current_screen(start_menu),
    clear_screen,
    regras,
    read_line_to_codes(user_input, Codes),
    string_codes(_, Codes),
    back_to_start_menu.

process_input('4') :-
    current_screen(start_menu),
    rank,
    read_line_to_codes(user_input, Codes),
    string_codes(_, Codes),
    back_to_start_menu.

process_input('5') :-
    current_screen(start_menu),
    writeln('saindo...'), halt.

% fluxo de novo jogo
process_input('1') :-
    current_screen(new_game),
    clear_screen,
    write_match(NewMatchName).

process_input('2') :-
    current_screen(new_game),
    back_to_start_menu.

% fluxo de continuar jogo
process_input('1') :-
    current_screen(continue_game),
    write_existing_match(MatchName).

process_input('2') :-
    current_screen(continue_game),
    list_matches,
    read_line_to_codes(user_input, Codes),
    string_codes(Out, Codes),
    retract(current_screen(_)),
    assertz(current_screen(continue_game)),
    show_menu(continue_game).

process_input('3') :-
    current_screen(continue_game),
    back_to_start_menu.

process_input(_) :-
    writeln('opção inválida!').

write_existing_match(MatchName) :-
    writeln('digite o nome da partida (ou digite 0 para voltar)>'),
    read_line_to_codes(user_input, Codes),
    string_codes(MatchName, Codes),

    (MatchName = "0" ->
        retract(current_screen(_)),
        assertz(current_screen(continue_game)),
        show_menu(continue_game), fail;
        (match_exists(MatchName) ->
            %ir pra partida
            writeln("partida existe.");
            writeln("partida não existe, tente novamente."), write_existing_match(MatchesNames))).

write_new_match(NewMatchName, Player1, Player2) :-
    write_match(NewMatchName),
    write_player(1, Player1),
    write_player(2, Player2).

write_match(NewMatchName) :-
    writeln('digite o nome da nova partida (ou digite 0 para voltar)>'),
    read_line_to_codes(user_input, Codes),
    string_codes(NewMatchName, Codes),
    check_lowercase(NewMatchName, LowerNewMatchName),

    (LowerNewMatchName = "0" ->
        back_to_start_menu, fail;
        (valid_name(LowerNewMatchName) ->
            (match_exists(LowerNewMatchName) ->
                (writeln("partida com esse nome já existe, tente novamente, ou digite 0 para voltar."),
                 write_match(NewMatchName));
                writeln("partida cadastrada"));
            (writeln("partida com nome invalido, insira um nome válido."),
            write_match(NewMatchName)))).

write_player(PlayerNumber, PlayerName) :-
    format("digite o nome do player ~w (ou digite 0 para voltar)>\n", [PlayerNumber]),
    read_line_to_codes(user_input, Codes),
    string_codes(PlayerInput, Codes),
    check_lowercase(PlayerInput, LowerPlayerInput),

    (LowerPlayerInput = "0" ->
        back_to_start_menu, fail;
        (valid_name(LowerPlayerInput) ->
            (acc_exists(LowerPlayerInput) ->
                writeln("jogador logado com sucesso.");
                (create_acc(LowerPlayerInput),
                 writeln("conta criada com sucesso.")));
            writeln("nome inválido, insira um nome válido."),
            write_player(PlayerNumber, PlayerName))).

rank :-
    writeln('\n------- rank -------'),
    get_accs_rank(AccRank),
    print_accounts(AccRank),
    writeln('--------------------'),
    writeln('digite qualquer numero para voltar>').

list_matches :-
    writeln('\n----- partidas criadas -----'),
    get_match_names(MatchesNames),
    print_matches(MatchesNames),
    writeln('----------------------------'),
    writeln('digite qualquer numero para voltar>').

print_matches(List) :-
    print_matches(List, 1).

print_matches([], _).
print_matches([Head|Rest], Order) :-
    format('~w - ~w~n', [Order, Head]),
    NextOrder is Order + 1,
    print_matches(Rest, NextOrder).

print_accounts([]).
print_accounts(Accounts) :-
    print_accounts(Accounts, 1).

print_accounts([], _).
print_accounts([[AccName, AccScore]|Rest], Order) :-
    format('~w - ~w: ~w~n', [Order, AccName, AccScore]),
    NextOrder is Order + 1,
    print_accounts(Rest, NextOrder).

setup_game(NewMatchName, Player1, Player2) :-
    create_acc(samuel),
    create_acc(gabriel),
    create_match(samuel_x_gabriel, samuel, gabriel).

back_to_start_menu :-
    retract(current_screen(_)),
    assertz(current_screen(start_menu)),
    show_menu(start_menu).
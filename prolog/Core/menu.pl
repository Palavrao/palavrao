:- consult('Interfaces/boxes_menu.pl').
:- consult('Utils/validator.pl').
:- consult('Utils/utils.pl').

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

% Define o fluxo de start_menu
process_input('0') :-
    current_screen(redimension_screen),
    retract(current_screen(_)),
    assertz(current_screen(start_menu)),
    show_menu(start_menu).

process_input('1') :-
    current_screen(start_menu),
    retract(current_screen(_)),
    assertz(current_screen(login)),
    show_menu(login).

process_input('2') :-
    current_screen(start_menu),
    retract(current_screen(_)),
    assertz(current_screen(continue_game)),
    show_menu(continue_game).

process_input('3') :-
    current_screen(start_menu),
    retract(current_screen(_)),
    assertz(current_screen(rules)),
    show_menu(rules).

process_input('4') :-
    current_screen(start_menu),
    writeln('Saindo...'), halt.

% Define o fluxo de new_game
process_input('1') :-
    current_screen(new_game),
    clear_screen,
    writeln("digite o nome da partida>"),
    read(Match),
    writeln("valeu"),
    writeln(Match).
    %verificar se partida ja existe, caso exista
    %pedir nova entrada, caso nao vai pro novo jogo
    %,process_new_account_input(Match).

process_input('2') :-
    current_screen(new_game),
    retract(current_screen(_)),
    assertz(current_screen(start_menu)),
    show_menu(start_menu).

% Define o fluxo de continue_game
process_input('1') :-
    current_screen(continue_game),
    retract(current_screen(_)),
    assertz(current_screen(create_account)),
    show_menu(create_account).

process_input('2') :-
    current_screen(continue_game),
    retract(current_screen(_)),
    assertz(current_screen(start_menu)),
    show_menu(start_menu).

% Define fluxo de login
process_input('1') :-
    current_screen(login),
    clear_screen,
    write_player_name(1, Player1),
    write_player_name(2, Player2),
    retract(current_screen(_)),
    assertz(current_screen(new_game)),
    show_menu(new_game),
    writeln("os nomes foram registrados com sucesso.").

process_input('2') :-
    current_screen(login),
    retract(current_screen(_)),
    assertz(current_screen(start_menu)),
    show_menu(start_menu).

process_input(_) :-
    writeln('Opção inválida!').

write_player_name(PlayerNumber, PlayerName) :-
format("digite o nome do player ~w> ", [PlayerNumber]),
    read_line_to_codes(user_input, Codes),
    string_codes(PlayerName, Codes),
    %verificar se ja existe, caso sim, loga
    %caso nao, cria nova conta
    %salvar nome do jogador
    (valid_name(PlayerName) ->
        writeln("nome registrado com sucesso");
        (writeln("nome inválido, insira um nome válido"),
         write_player_name(PlayerNumber, PlayerName))).

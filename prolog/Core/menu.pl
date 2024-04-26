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
    retract(current_screen(_)),
    assertz(current_screen(start_menu)),
    show_menu(start_menu).

process_input('1') :-
    current_screen(start_menu),
    retract(current_screen(_)),
    assertz(current_screen(new_game)),
    show_menu(new_game).

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
    string_codes(Out, Codes),
    retract(current_screen(_)),
    assertz(current_screen(start_menu)),
    show_menu(start_menu).

process_input('4') :-
    current_screen(start_menu),
    writeln('saindo...'), halt.

% fluxo de novo jogo
process_input('1') :-
    current_screen(new_game),
    clear_screen,
    writeln("digite o nome da partida>"),
    read(Match),
    writeln("valeu"),
    retract(current_screen(_)),
    assertz(current_screen(login)),
    show_menu(login).
    %verificar se partida ja existe, caso exista
    %pedir nova entrada, caso nao vai pro novo jogo

process_input('2') :-
    current_screen(new_game),
    retract(current_screen(_)),
    assertz(current_screen(start_menu)),
    show_menu(start_menu).

% fluxo de continuar jogo
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

% fluxo de login
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
    writeln('opção inválida!').

write_existing_match(MatchName) :-
    writeln('digite o nome da partida (ou digite 0 para voltar)>'),
    read_line_to_codes(user_input, Codes),
    string_codes(MatchName, Codes),

    (MatchName = "0" ->
        writeln('saindo...'), fail;
        (valid_name(MatchName), match_exists(MatchName) ->
            %ir pra partida
            writeln("partida existe.");
            writeln("partida não existe, tente novamente, ou digite 0 para voltar"),
            write_existing_match(MatchName))).

write_player(PlayerNumber, PlayerName) :-
    format("digite o nome do player ~w (ou digite 0 para voltar)>", [PlayerNumber]),
    read_line_to_codes(user_input, Codes),
    string_codes(PlayerInput, Codes),

    (PlayerInput = "0" ->
        writeln('saindo...'), fail;
        (valid_name(PlayerInput) ->
            (acc_exists(PlayerInput) ->
                writeln("jogador logado com sucesso.");
                (create_acc(PlayerInput),
                 writeln("conta criada com sucesso.")));
            writeln("nome inválido, insira um nome válido."),
            write_player(PlayerNumber, PlayerName))).

back_to_start_menu :-
    retract(current_screen(_)),
    assertz(current_screen(start_menu)),
    show_menu(start_menu).
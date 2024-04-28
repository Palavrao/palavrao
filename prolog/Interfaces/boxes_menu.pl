game_action(new_game).
game_action(continue_game).
game_action(login).
game_action(register).
game_action(register_match).
game_action(start_menu).
game_action(redimension_screen).
game_action(rank).
game_action(invalid_action).
game_action(before_game).
game_action(finish_match).
game_action(matches).

menu(redimension_screen,
     ["                 ┌─────────────────────────────┐                ",
      "                 │                             │                ",
      "                 │    Redimensione para que    │                ",
      "                 │  a Line caiba no terminal!  │                ",
      "                 │                             │                ",
      "<-------------------------------------------------------------->",
      "                 │ > Pressione 0 p/ continuar  │                ",
      "                 │                             │                ",
      "                 └─────────────────────────────┘                "]).

menu(start_menu,
    ["    ┌───────────────────────────────┐   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │           PALAVRÃO            │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │      1.  novo jogo            │   ",
     "    │      2.  continuar jogo       │   ",
     "    │      3.  regras               │   ",
     "    │      4.  rank                 │   ",
     "    │      5.  sair                 │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    └───────────────────────────────┘   "]).

menu(new_game,
    ["    ┌───────────────────────────────┐   ",
     "    │                               │   " ,
     "    │                               │   ",
     "    │           PALAVRÃO            │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │      1. criar conta           │   ",
     "    │      2. login                 │   ",
     "    │      3. voltar                │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │      iniciando novo jogo      │   ",
     "    └───────────────────────────────┘   "]).

menu(continue_game,
    ["    ┌───────────────────────────────┐   ",
     "    │                               │   " ,
     "    │                               │   ",
     "    │           PALAVRÃO            │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │      1. nome da partida       │   ",
     "    │      2. partidas criadas      │   ",
     "    │      3. voltar                │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │        continuar jogo         │   ",
     "    └───────────────────────────────┘   "]).


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
    assertz(current_screen(new_game)),
    show_menu(new_game). 

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
    retract(current_screen(_)),
    assertz(current_screen(rank)),
    show_menu(rank).

process_input('5') :- 
    current_screen(start_menu),
    writeln('Saindo...'), halt.     

% Define o fluxo de new_game 
process_input('1') :- 
    current_screen(new_game),
    retract(current_screen(_)),
    assertz(current_screen(create_account)),
    show_menu(create_account). 

process_input('2') :- 
    current_screen(new_game),
    retract(current_screen(_)),
    assertz(current_screen(login)),
    show_menu(login).

process_input('3') :- 
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
    assertz(current_screen(continue_game)),
    show_menu(continue_game).

process_input('3') :- 
    current_screen(continue_game),
    retract(current_screen(_)),
    assertz(current_screen(start_menu)),
    show_menu(start_menu).

process_input(_) :- 
    writeln('Opção inválida!').
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
      "                 │           > Enter           │                ",
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

update_menu(Action, CurrentMenu, UpdatedMenu) :-
    (menu(Action, NovoMenu) -> UpdatedMenu = NovoMenu ; UpdatedMenu = CurrentMenu).

writeln_menu([]).
writeln_menu([Line|Rest]) :-
    writeln(Line),
    writeln_menu(Rest).

show_menu(Action) :-
    update_menu(Action, [], UpdatedMenu),
    writeln_menu(UpdatedMenu).

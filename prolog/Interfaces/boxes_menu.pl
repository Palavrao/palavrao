:- consult('Utils/utils.pl').
:- consult('Utils/validator.pl').

game_action(new_game).
game_action(continue_game).
game_action(login).
game_action(register_match).
game_action(start_menu).
game_action(redimension_screen).
game_action(finish_match).

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
     "    │      4.  sair                 │   ",
     "    │                               │   ",
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
     "    │      1. nome da partida       │   ",
     "    │      2. voltar                │   ",
     "    │                               │   ",
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
     "    │      2. voltar                │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │        continuar jogo         │   ",
     "    └───────────────────────────────┘   "]).

menu(login,
    ["    ┌───────────────────────────────┐   ",
     "    │                               │   " ,
     "    │                               │   ",
     "    │           PALAVRÃO            │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │      1. nome dos players      │   ",
     "    │      2. voltar                │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │       login/criar conta       │   ",
     "    └───────────────────────────────┘   "]).
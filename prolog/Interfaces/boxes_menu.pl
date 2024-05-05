game_action(new_game).
game_action(continue_game).
game_action(login).
game_action(register_match).
game_action(start_menu).
game_action(redimension_screen).
game_action(finish_match).


% Tela inicial do jogo, que mostra ao usuário os limites do terminal que devem ser seguidos
menu(redimension_screen,
     ["                 ┌─────────────────────────────┐                ",
      "                 │                             │                ",
      "                 │    Redimensione para que    │                ",
      "                 │  a linha caiba no terminal! │                ",
      "                 │                             │                ",
      "<-------------------------------------------------------------->",
      "                 │ > Pressione 0 p/ continuar  │                ",
      "                 │                             │                ",
      "                 └─────────────────────────────┘                "]).

% Tela de menu inicial
menu(start_menu,
    ["    ┌───────────────────────────────┐   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │            PALAVRÃO           │   ",
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

% Tela de iniciação de novo jogo
menu(new_game,
    ["    ┌───────────────────────────────┐   ",
     "    │                               │   " ,
     "    │                               │   ",
     "    │            PALAVRÃO           │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │    digite o nome da partida   │   ",
     "    │        e dos jogadores        │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │      iniciando novo jogo      │   ",
     "    └───────────────────────────────┘   "]).

% Tela de iniciação de continuar jogo
menu(continue_game,
    ["    ┌───────────────────────────────┐   ",
     "    │                               │   " ,
     "    │                               │   ",
     "    │            PALAVRÃO           │   ",
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

% Tela de iniciação de login de conta
menu(login,
    ["    ┌───────────────────────────────┐   ",
     "    │                               │   " ,
     "    │                               │   ",
     "    │            PALAVRÃO           │   ",
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
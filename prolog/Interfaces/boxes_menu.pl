:- include('Utils/utils.pl').
:- use_module(library(ansi_term)).
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

menu(register, 
    ["    ┌───────────────────────────────┐   ",
     "    │                               │   ",
     "    │           PALAVRÃO            │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │     1. digitar nome           │   ",
     "    │     2. voltar                 │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │         criando conta         │   ",
     "    └───────────────────────────────┘   "]).

menu(login, 
    ["    ┌───────────────────────────────┐   ",
     "    │                               │   ",
     "    │           PALAVRÃO            │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │     1. digitar nome           │   ",
     "    │     2. voltar                 │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │                               │   ",
     "    │            login              │   ",
     "    └───────────────────────────────┘   "]).

menu(rank,_) :-
  colorText("\nRANKING!", red),
  colorText("\n\nDigite 0 p/ voltar.\n", blue).

menu(rules, _) :-
   colorText("\nPalavrão!", yellow),
   writeln(" é um jogo estratégico double player de formação de palavras em um tabuleiro matricial, baseado no popular Scrabble.\n"),
   colorText("Objetivo do jogo:\n\n", green),
   writeln("O objetivo do jogo é acumular a maior quantidade de pontos possível a partir da formação de novas palavras horizontal ou verticalmente, e adjacentes às palavras já dispostas no tabuleiro.\n"),
   colorText("Funcionamento do jogo:\n\n", green),
   writeln("- Cada jogador recebe 7 letras, que podem ser trocadas por letras aleatórias do saco de letras, fazendo-o perder a vez.\n"),
   writeln("- O primeiro jogador coloca ao menos duas letras no tabuleiro formando a primeira palavra.\n"),
   writeln("- Em seguida os jogadores tomam turnos adicionando letras adjacentes às letras dispostas no tabuleiro para formar novas palavras.\n"),
   writeln("- Para formar novas palavras, o jogador deve indicar a coordenada da célula onde deseja adicionar a primeira letra e se a palavra deve ser disposta horizontal ou verticalmente no tabuleiro.\n"),
   writeln("- O tabuleiro irá entender quando as letras da palavra já estiverem presentes na posição necessária, então basta escrever a palavra normalmente com as coordenadas.\n"),
   writeln("- Dentre as letras dos jogadores pode haver peças curinga (<), que os permitem utilizar letras que não estão em mãos nem no tabuleiro. Cada letra ausente consome um curinga.\n"),
   writeln("- A pontuação para cada rodada é a soma dos valores das letras em cada palavra formada ou modificada + os pontos adicionais obtidos de células e ocasiões especiais.\n"),
   writeln("- As rodadas se alternam com um limite de tempo de 5 minutos para cada jogada. Passado esse tempo, o jogador perde a vez.\n"),
   writeln("- O jogador pode pular a sua vez."),
   colorText("\nPontuação das letras:\n\n", green),
   colorText("    0   ", blue),
   writeln("|   < (Curinga)"),
   colorText("    1   ", blue),
   writeln("|   A, E, I, O, S, U, M, R, T"),
   colorText("    2   ", blue),
   writeln("|   D, L, C, P"),
   colorText("    3   ", blue),
   writeln("|   N, B, Ç"),
   colorText("    4   ", blue),
   writeln("|   F, G, H, V"),
   colorText("    5   ", blue),
   writeln("|   J"),
   colorText("    6   ", blue),
   writeln("|   Q"),
   colorText("    7   ", blue),
   writeln("|   X, Z"),
   colorText("\n Pontuações especiais:\n", green),
   colorText("    ■ ", blue),
   writeln("-> Dobra a pontuação da letra sobre a célula."),
   colorText("    ■ ", green),
   writeln("-> Triplica a pontuação da letra sobre a célula."),
   colorText("    ■ ", magenta),
   writeln("-> Dobra a pontuação de toda a palavra cuja letra está sobre a célula."),
   colorText("    ■ ", red),
   writeln("-> Triplica a pontuação de toda a palavra cuja letra está sobre a célula."),
   colorText("\nBingo! ", magenta),
   writeln("Se um jogador usar 7 letras para formar uma nova palavra, a pontuação dela é incrementada em 20 pontos."),
   colorText("\nFim de jogo:\n", green),
   writeln("  O jogo termina quando não há mais peças no saco ou ambos os jogadores realizam 4 trocas de peças seguidas. Em caso de empate, o jogador com a menor soma na pontuação das letras em sua mão vence."),
   colorText("\n\nDigite 0 p/ voltar.\n", blue).

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

% Registrar nova conta
process_input('1') :- 
    current_screen(new_game),
    retract(current_screen(_)),
    assertz(current_screen(register)),
    show_menu(register).

process_input('2') :- 
    current_screen(register),
    retract(current_screen(_)),
    assertz(current_screen(new_game)),
    show_menu(new_game).

% Login
process_input('2') :- 
    current_screen(new_game),
    retract(current_screen(_)),
    assertz(current_screen(login)),
    show_menu(login).

process_input('2') :- 
    current_screen(login),
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

process_input('0') :-
    current_screen(rules),
    retract(current_screen(_)),
    assertz(current_screen(start_menu)),
    show_menu(start_menu).

process_input('4') :- 
    current_screen(start_menu),
    retract(current_screen(_)),
    assertz(current_screen(rank)),
    show_menu(rank).

process_input('0') :-
    current_screen(rank),
    retract(current_screen(_)),
    assertz(current_screen(start_menu)),
    show_menu(start_menu).

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

colorText(Text, Color) :-
    ansi_format([fg(Color)], '~w', [Text]).
:- include('Interfaces/boxes_menu.pl').
:- include('Utils/utils.pl').
:- include('Utils/validator.pl').
:- include('Constants/paths.pl').
:- include('Controllers/accs_controller.pl').
:- include('Controllers/matches_controller.pl').
:- include('Controllers/board_controller.pl').
:- include('Controllers/letters_controller.pl').
:- include('Controllers/players_controller.pl').
:- include('Core/menu.pl').
:- include('data/accounts.pl').
:- include('data/matches.pl').
:- include('data/boards.pl').
:- include('data/players.pl').
:- include('Core/game.pl').
:- include('Interfaces/draw_board.pl').
:- include('Words/words.pl').
:- use_module(library(dialect/sicstus/system)).
:- use_module(library(ansi_term)).


main :-
    assertz(current_screen(redimension_screen)),
    show_menu(redimension_screen),
    main_loop.

main_loop :-
    get_input(Input),
    process_input(Input),
    main_loop.

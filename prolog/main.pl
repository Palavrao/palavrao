:- include('Core/menu.pl').
:- consult('Utils/validator.pl').
% :- include('Utils/utils.pl').
% :- include('Constants/paths.pl').
% :- include('Controllers/accs_controller.pl').
% :- include('Controllers/matches_controller.pl').
% :- include('Controllers/board_controller.pl').
% :- include('Controllers/letters_controller.pl').
% :- include('Controllers/players_controller.pl').
% :- include('data/accounts.pl').
% :- include('data/matches.pl').
% :- include('data/boards.pl').
% :- include('data/players.pl').

main :-
    % create_acc(samuel),
    % create_acc(gabriel),
    % create_match(samuel_x_gabriel, samuel, gabriel),
    % inc_player_score(samuel_x_gabriel, 10),
    % finish_match(samuel_x_gabriel).

    % create_acc(samuel),
    % create_acc(gabriel),
    % create_match(samuel_x_gabriel, samuel, gabriel),
    % inc_player_score(samuel_x_gabriel, 10).
    % finish_match(samuel_x_gabriel).
    assertz(current_screen(redimension_screen)),
    show_menu(redimension_screen),
    main_loop.  

main_loop :-
    get_input(Input),
    process_input(Input),
    main_loop.

:- include('Utils/utils.pl').
:- include('Controllers/accs_controller.pl').
:- include('Controllers/matches_controller.pl').
:- include('data/accounts.pl').
:- include('data/matches.pl').

main :-
    create_acc(samuel),
    create_acc(gabriel),

    create_match(samuel_x_gabriel, samuel, gabriel),
    
    inc_match_used_words(samuel_x_gabriel, [palavra, foda]),
    toggle_player_turn(samuel_x_gabriel),
    skip_player_turn(samuel_x_gabriel),
    update_match_timer(samuel_x_gabriel, 200),
    update_player_letters(samuel_x_gabriel). 

:- include('../Utils/utils.pl').
:- include('../Constants/paths.pl').
:- include('./letters_controller.pl').
:- include('./accs_controller.pl').
:- include('./players_controller.pl').
:- dynamic(match/9).


get_match(MatchName, Match) :- 
    match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, MatchTimer, MatchSkips),
    Match = match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, MatchTimer, MatchSkips).


get_match_board_name(MatchName, MatchBoardName) :-
    match(MatchName, MatchBoardName,_,_,_,_,_,_,_).


get_match_turn(MatchName, MatchTurn) :- 
    match(MatchName, _, MatchTurn, _, _, _, _, _, _).


get_match_p1_name(MatchName, P1Name) :- 
    match(MatchName, _, _, P1Name, _, _, _, _, _).


get_match_p2_name(MatchName, P2Name) :- 
    match(MatchName, _, _, _, P2Name, _, _, _, _).


get_match_letters(MatchName, MatchLetters) :- 
    match(MatchName, _, _, _, _, MatchLetters, _, _, _).


get_match_words(MatchName, MatchWords) :- 
    match(MatchName, _, _, _, _, _, MatchWords, _, _).


get_match_timer(MatchName, MatchTimer) :- 
    match(MatchName, _, _, _, _, _, _, MatchTimer, _).


get_match_skips(MatchName, MatchSkips) :- 
    match(MatchName, _, _, _, _, _, _, _, MatchSkips).


create_match(MatchName, Acc1Name, Acc2Name) :- 
    match(MatchName,_,_,_,_,_,_,_,_);
    matches_path(MatchesPath),
    atom_concat(MatchName, board, BoardName),
    start_letters(StartLetters),
    inc_fact_file(MatchesPath, match(MatchName, BoardName, false, P1Name, P2Name, StartLetters, [], 300, 0)).


del_match(MatchName) :-
    get_match(MatchName, Match),
    del_fact_file(Path, Match).


finist_match(MatchName) :- 
    get_player_score(MatchName, P1Name, P1Score),
    get_player_score(MatchName, P2Name, P2Score),
    inc_acc_score(P1Name, P1Score),
    inc_acc_score(P2Name, P2Score),

    del_match(MatchName).


update_match_timer(MatchName, NewTimer) :- 
    matches_path(MatchesPath),

    get_match(MatchName, Match),
    get_match_board_name(MatchName, BoardName),
    get_match_turn(MatchName, MatchTurn),
    get_match_p1_name(MatchName, P1Name),
    get_match_p2_name(MatchName, P2Name),
    get_match_letters(MatchName, MatchLetters),
    get_match_words(MatchName, MatchWords),
    get_match_skips(MatchName, MatchSkips),

    update_fact_file(MatchesPath, Match, match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, NewTimer, MatchSkips)).


get_matches(Matches) :- 
    matches_path(MatchesPath),
    read_facts_file(MatchesPath, Matches).


inc_player_score(MatchName, PlayerScore) :- 
    get_match_turn(MatchName, MatchTurn),

    (MatchTurn -> 
        get_match_p1_name(MatchName, PlayerName);
        get_match_p2_name(MatchName, PlayerName)),

    inc_score(MatchName, PlayerName, PlayerScore).

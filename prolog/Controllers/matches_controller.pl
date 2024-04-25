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


get_turn_player_name(MatchName, PlayerName) :-
    get_match_turn(MatchName, MatchTurn),

    (MatchTurn -> 
        get_match_p2_name(MatchName, PlayerName);
        get_match_p1_name(MatchName, PlayerName)).


create_match(MatchName, P1Name, P2Name) :- 
    matches_path(MatchesPath),

    create_player(MatchName, P1Name),
    create_player(MatchName, P2Name),

    atom_concat(MatchName, board, BoardName),

    start_letters(StartLetters),
    inc_fact_file(MatchesPath, match(MatchName, BoardName, false, P1Name, P2Name, StartLetters, [], 300, 0), match),
    update_player_letters(MatchName),
    toggle_player_turn(MatchName),
    update_player_letters(MatchName),
    toggle_player_turn(MatchName).

del_match(MatchName) :-
    matches_path(MatchesPath),
    players_path(PlayersPath),

    get_match(MatchName, Match),
    get_match_p1_name(MatchName, P1Name),
    get_match_p2_name(MatchName, P2Name),
    get_player(MatchName, P1Name, P1),
    get_player(MatchName, P2Name, P2),

    del_fact_file(PlayersPath, P1, player),
    del_fact_file(PlayersPath, P2, player),
    del_fact_file(MatchesPath, Match, match).


finish_match(MatchName) :- 
    get_player_score(MatchName, P1Name, P1Score),
    get_player_score(MatchName, P2Name, P2Score),
    inc_acc_score(P1Name, P1Score),
    inc_acc_score(P2Name, P2Score),

    del_match(MatchName).


update_match_timer(MatchName, NewTimer) :- 
    matches_path(MatchesPath),

    get_match(MatchName, Match),
    match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, _, MatchSkips),

    update_fact_file(MatchesPath, Match, match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, NewTimer, MatchSkips), match).


update_match_letters(MatchName, NewLetters) :-
    matches_path(MatchesPath),

    get_match(MatchName, Match),
    match(MatchName, BoardName, MatchTurn, P1Name, P2Name, _, MatchWords, MatchTimer, MatchSkips),

    update_fact_file(MatchesPath, Match, match(MatchName, BoardName, MatchTurn, P1Name, P2Name, NewLetters, MatchWords, MatchTimer, MatchSkips), match).


get_matches(Matches) :- 
    findall(match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, MatchTimer, MatchSkips),
            match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, MatchTimer, MatchSkips),
            Matches).


inc_player_score(MatchName, PlayerScore) :- 
    get_turn_player_name(MatchName, PlayerName),

    inc_score(MatchName, PlayerName, PlayerScore).


update_player_letters(MatchName) :- 
    get_match_letters(MatchName, MatchLetters),
    get_turn_player_name(MatchName, PlayerName),

    get_player_letters(MatchName, PlayerName, PlayerLetters),
    length(PlayerLetters, PlayerLettersLength),
    LettersQuantity is 7 - PlayerLettersLength,

    pop_random_elements(MatchLetters, LettersQuantity, RemovedLetters, UpdatedLetters),

    update_match_letters(MatchName, UpdatedLetters),
    add_letters(MatchName, PlayerName, RemovedLetters).


skip_player_turn(MatchName) :- 
    matches_path(MatchesPath),
    
    get_match(MatchName, Match),
    match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, MatchTimer, MatchSkips),
    
    NewMatchSkips is MatchSkips + 1,
    
    update_fact_file(MatchesPath, Match, match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, MatchTimer, NewMatchSkips), match),
    toggle_player_turn(MatchName).


reset_match_skips(MatchName) :- 
    matches_path(MatchesPath),
    
    get_match(MatchName, Match),
    match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, MatchTimer, _),

    update_fact_file(MatchesPath, Match, match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, MatchTimer, 0), match).


toggle_player_turn(MatchName) :- 
    matches_path(MatchesPath),

    get_match(MatchName, Match),
    match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, _, MatchSkips),

    (MatchTurn -> 
        NewMatchTurn = false;
        NewMatchTurn = true),

    update_fact_file(MatchesPath, Match, match(MatchName, BoardName, NewMatchTurn, P1Name, P2Name, MatchLetters, MatchWords, 300, MatchSkips), match).


inc_match_used_words(MatchName, UsedWords) :- 
    matches_path(MatchesPath),

    get_match(MatchName, Match),
    match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, MatchTimer, MatchSkips),
    
    append(MatchWords, UsedWords, NewMatchWords),

    update_fact_file(MatchesPath, Match, match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, NewMatchWords, MatchTimer, MatchSkips), match).


switch_player_letter(MatchName, Letter) :- 
    matches_path(MatchesPath),

    get_match_letters(MatchName, MatchLetters),
    pop_random_elements(MatchLetters, 1, NewLetter, UpdatedLetters),

    get_turn_player_name(MatchName, PlayerName),

    get_player_letters(MatchName, PlayerName, PlayerLetters),
    remove_one_element(PlayerLetters, _, Letter, UpdatedPlayerLetters),

    append(UpdatedPlayerLetters, NewLetter, FinalPlayerLetters),
    append(UpdatedLetters, [Letter], FinalMatchLetters),
    
    update_match_letters(MatchName, FinalMatchLetters),
    update_letters(MatchName, PlayerName, FinalPlayerLetters).


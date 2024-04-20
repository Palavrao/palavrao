:- include('../Utils/utils.pl').
:- include('../Constants/paths.pl').
:- include('./letters_controller.pl').
:- include('./accs_controller.pl').
:- include('./players_controller.pl').
:- dynamic(match/9).


get_match(MatchName, match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, MatchTimer, MatchSkips)).


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
        get_match_p1_name(MatchName, PlayerName);
        get_match_p2_name(MatchName, PlayerName)).


create_match(MatchName, P1Name, P2Name) :- 
    match(MatchName,_,_,_,_,_,_,_,_);
    matches_path(MatchesPath),

    create_player(MatchName, P1Name),
    create_player(MatchName, P2Name),

    atom_concat(MatchName, board, BoardName),

    start_letters(StartLetters),
    inc_fact_file(MatchesPath, match(MatchName, BoardName, false, P1Name, P2Name, StartLetters, [], 300, 0)).


del_match(MatchName) :-
    get_match(MatchName, Match),
    del_fact_file(Path, Match).


finish_match(MatchName) :- 
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


update_match_letters(MatchName, NewLetters) :-
    matches_path(MatchesPath),

    get_match(MatchName, Match),
    get_match_board_name(MatchName, BoardName),
    get_match_turn(MatchName, MatchTurn),
    get_match_p1_name(MatchName, P1Name),
    get_match_p2_name(MatchName, P2Name),
    get_match_words(MatchName, MatchWords),
    get_match_timer(MatchName, MatchTimer),
    get_match_skips(MatchName, MatchSkips),

    update_fact_file(MatchesPath, Match, match(MatchName, BoardName, MatchTurn, P1Name, P2Name, NewLetters, MatchWords, MatchTimer, MatchSkips)).


get_matches(Matches) :- 
    matches_path(MatchesPath),
    read_facts_file(MatchesPath, Matches).


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
    update_letters(MatchName, PlayerName, RemovedLetters).


skip_player_turn(MatchName) :- 
    matches_path(MatchesPath),

    get_match(MatchName, Match),
    get_match_board_name(MatchName, BoardName),
    get_match_turn(MatchName, MatchTurn),
    get_match_p1_name(MatchName, P1Name),
    get_match_p2_name(MatchName, P2Name),
    get_match_letters(MatchName, MatchLetters),
    get_match_words(MatchName, MatchWords),
    get_match_timer(MatchName, MatchTimer),
    get_match_skips(MatchName, MatchSkips),

    NewMatchSkips is MatchSkips + 1,

    update_fact_file(MatchesPath, Match, match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, MatchTimer, NewMatchSkips)),
    toggle_player_turn(MatchName).


toggle_player_turn(MatchName) :- 
    matches_path(MatchesPath),

    get_match(MatchName, Match),
    get_match_board_name(MatchName, BoardName),
    get_match_turn(MatchName, MatchTurn),
    get_match_p1_name(MatchName, P1Name),
    get_match_p2_name(MatchName, P2Name),
    get_match_letters(MatchName, MatchLetters),
    get_match_words(MatchName, MatchWords),
    get_match_skips(MatchName, MatchSkips),

    (MatchTurn -> 
        NewMatchTurn = false;
        NewMatchTurn = true),

    update_fact_file(MatchesPath, Match, match(MatchName, BoardName, NewMatchTurn, P1Name, P2Name, MatchLetters, MatchWords, 300, MatchSkips)).


inc_match_used_words(MatchName, UsedWords) :- 
    matches_path(MatchesPath),

    get_match(MatchName, Match),
    get_match_board_name(MatchName, BoardName),
    get_match_turn(MatchName, MatchTurn),
    get_match_p1_name(MatchName, P1Name),
    get_match_p2_name(MatchName, P2Name),
    get_match_letters(MatchName, MatchLetters),
    get_match_words(MatchName, MatchWords),
    get_match_timer(MatchName, MatchTimer),
    get_match_skips(MatchName, MatchSkips),
    
    append(MatchWords, UsedWords, NewMatchWords),

    update_fact_file(MatchesPath, Match, match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, NewMatchWords, MatchTimer, MatchSkips)).


% switch_player_letter(MatchName, Letter) :- 
%     matches_path(MatchesPath),

%     pop_random_elements(MatchLetters, 1, RemovedLetter, UpdatedLetters),

%     get_turn_player_name(MatchName, PlayerName),

%     get_player_letters(MatchName, PlayerName, PlayerLetters),
%     remove_one_element(PlayerLetters, _, RemovedLetter, ),

%     update_match_letters(MatchName, UpdatedLetters),
%     update_letters(MatchName, PlayerName, RemovedLetters).


% switchPlayerLetter :: Match -> Letter -> IO Match
% switchPlayerLetter match letter = do
%     (newLetter, updatedLetters) <- UT.popRandomElements (mLetters match) 1

%     let playerLetters = UT.removeOneElement (pLetters player) letter
%     let updatedPlayer = updateLetters player playerLetters 

%     let updatedMatch = _updateMatchLetters match (letter:updatedLetters)
%     let updatedPlayer' = addLetters updatedPlayer newLetter

%     return $ _updateMatchPlayer updatedMatch updatedPlayer'
%     where
%         player
%             | mTurn match = mP2 match
%             | otherwise = mP1 match
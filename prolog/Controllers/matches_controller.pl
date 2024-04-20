:- include('../Utils/utils.pl').
:- include('../Constants/paths.pl').
:- include('./letters_controller.pl').
:- dynamic(match/9).

get_match_board_name(MatchName, MatchBoardName) :-
    match(MatchName, MatchBoardName,_,_,_,_,_,_,_).


create_match(MatchName, Acc1Name, Acc2Name) :- 
    match(MatchName,_,_,_,_,_,_,_,_);
    matches_path(MatchesPath),
    atom_concat(MatchName, board, BoardName),
    start_letters(StartLetters),
    inc_fact_file(MatchesPath, match(MatchName, BoardName, false, Acc1Name, Acc2Name, StartLetters, [], 300, 0)).

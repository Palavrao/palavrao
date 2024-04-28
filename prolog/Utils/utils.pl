

clear_screen :- (current_prolog_flag(windows, true) -> shell('cls'); shell('clear')).

make_data_folder :-
    data_path(Path),
    (exists_directory(Path) ;
    make_directory(Path)).


start_persistence :- 
    make_data_folder,
    accs_path(AccsPath),
    matches_path(MatchesPath),
    players_path(PlayersPath),
    boards_path(BoardsPath),
    make_facts_file(AccsPath),
    make_facts_file(BoardsPath),
    make_facts_file(MatchesPath),
    make_facts_file(PlayersPath).


make_facts_file(Path) :-
    exists_file(Path) ; 
    open(Path, write, Stream),
    close(Stream).


save_fact_file(Path, Pred) :-
    tell(Path),
    listing(Pred),
    told.


inc_fact_file(Path, NewFact, Pred) :- 
    assertz(NewFact),
    save_fact_file(Path, Pred).


update_fact_file(Path, CurrentFact, NewFact, Pred) :-
    retract(CurrentFact),
    assertz(NewFact),
    save_fact_file(Path, Pred).


del_fact_file(Path, DelFact, Pred) :-
    retract(DelFact),
    save_fact_file(Path, Pred).


remove_one_element([H|T], 0, H, T) :- !.
remove_one_element([H|T], Quantity, RemovedElement, UpdatedElements) :- 
    NewQuantity is Quantity - 1,
    remove_one_element(T, NewQuantity, RemovedElement, OtherElements),
    UpdatedElements = [H|OtherElements].


pop_random_elements([], _, [], []) :- !.
pop_random_elements(Elements, 0, [], Elements) :- !.
pop_random_elements(Elements, Quantity, [RemovedElement|RemovedElements], UpdatedElements) :- 
    length(Elements, ElementsLen),
    Len is ElementsLen - 1,
    random_between(0, Len, RandIndex),
    remove_one_element(Elements, RandIndex, RemovedElement, TempUpdatedElements),

    NewQuantity is Quantity - 1,
    pop_random_elements(TempUpdatedElements, NewQuantity, RemovedElements, UpdatedElements).


remove_elements([], _, []).
remove_elements([H|T], ToRemove, Updated) :-
    (member(H, ToRemove) -> 
        remove_one_element(ToRemove, _, _, NewToRemove),
        remove_elements(T, NewToRemove, Updated);
    remove_elements(T, ToRemove, OtherElements),
    Updated = [H|OtherElements]).


getWordList(WordList) :-
    words_path(WP),
    read_file_to_string(WP, Base, []),
    atomic_list_concat(LineList, '\n', Base),
    maplist(atom_string, WordList, LineList).


no_period_input(Input):- 
    read_line_to_codes(user_input, K),
    string_to_atom(K, AtomInput),
    atom_string(AtomInput, StringInput),
    string_upper(StringInput, UpperInput),
    string_to_atom(UpperInput, Input).

too_long(Start,End) :-
    K is End - Start,
    write(K),
    K > 300.
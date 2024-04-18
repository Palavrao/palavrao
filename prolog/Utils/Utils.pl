:- include('../Constants/paths.pl').

make_data_folder :- 
    data_path(Path),
    (exists_directory(Path) ;
    make_directory(Path)).


make_facts_file(Path) :-
    exists_file(Path) ; 
    open(Path, write, Stream),
    close(Stream).


start_persistence :- 
    make_data_folder,
    accs_path(AccsPath),
    matches_path(MatchesPath),
    make_facts_file(AccsPath),
    make_facts_file(MatchesPath).


inc_facts_file(Path, NewFacts) :- 
    open(Path, append, Stream),
    write(Stream, NewFacts),
    nl(Stream),
    close(Stream).

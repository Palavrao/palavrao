:- include('../Constants/paths.pl').

make_data_folder :- 
    data_path(Path),
    (exists_directory(Path) ;
    make_directory(Path)).


make_facts_file(Path, InitialContent) :-
    exists_file(Path) ; 
    open(Path, write, Stream),
    write(Stream, InitialContent),
    nl(Stream),
    close(Stream).


start_persistence :- 
    make_data_folder,
    accs_path(AccsPath),
    matches_path(MatchesPath),
    make_facts_file(AccsPath,'account(\'\',0).'),
    make_facts_file(MatchesPath, 'match(\'\').').


clean_fact_file(Path) :-
    open(Path, write, Stream),
    write(Stream, ''),
    close(Stream).

inc_fact_file(Path, NewFact) :- 
    NewFact == end_of_file;
    open(Path, append, Stream),
    writeq(Stream, NewFact),
    write(Stream, '.'),
    nl(Stream),
    close(Stream),
    assertz(NewFact).


read_facts_file(Path, Facts) :-
    open(Path, read, Stream),
    get_file_facts(Stream, Facts),
    close(Stream).

get_file_facts(Stream, []) :-
    at_end_of_stream(Stream),
    !.
get_file_facts(Stream, [Fact|Facts]) :-
    \+ at_end_of_stream(Stream),
    (Fact == end_of_file);
    read(Stream, Fact),
    get_file_facts(Stream, Facts).


update_fact_file(Path, CurrentFact, NewFact) :-
    read_facts_file(Path, Facts),
    clean_fact_file(Path),
    update_fact_file(Path, CurrentFact, NewFact, Facts).


update_fact_file(_, _, _, []).
update_fact_file(Path, CurrentFact, NewFact, [CurrentFact|T]) :- 
    inc_fact_file(Path, NewFact),
    update_fact_file(Path, CurrentFact, NewFact, T). 
update_fact_file(Path, CurrentFact, NewFact, [H|T]) :- 
    inc_fact_file(Path, H),
    update_fact_file(Path, CurrentFact, NewFact, T).
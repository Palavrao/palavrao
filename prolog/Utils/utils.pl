:- include('../Constants/paths.pl').

make_data_folder :- 
    data_path(Path),
    (exists_directory(Path) ;
    make_directory(Path)).


make_facts_file(Path, InitialContent) :-
    exists_file(Path) ; 
    open(Path, write, Stream),
    write(Stream, InitialContent),
    close(Stream).


start_persistence :- 
    make_data_folder,
    accs_path(AccsPath),
    matches_path(MatchesPath),
    make_facts_file(AccsPath,'account(\'\', 0).'),
    make_facts_file(MatchesPath, 'match(\'\').').


inc_fact_file(Path, NewFact) :- 
    open(Path, append, Stream),
    writeq(Stream, NewFact),
    write(Stream, '.'),
    nl(Stream),
    close(Stream),
    assertz(NewFact).


read_facts_file(Path, Facts) :-
    open(Path, read, Stream),
    read_stream_to_codes(Stream, Codes),
    close(Stream),
    string_codes(String, Codes),
    split_string(String, "\n", "", Lines),
    maplist(atom_string, Facts, Lines).
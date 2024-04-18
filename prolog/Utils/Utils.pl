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


atom_to_single_quoted_string(Atom, String) :-
    atom_concat('\'', Atom, TempString),
    atom_concat(TempString, '\'', String).
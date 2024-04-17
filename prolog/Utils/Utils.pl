:- use_module(library(http/json)).


data_path('data').
accs_path('./data/accounts.json').
matches_path('./data/matches.json').


make_data_folder :- 
    data_path(Path),
    (exists_directory(Path) ;
    make_directory(Path)).


make_json_file(Path) :-
    exists_file(Path) ; 
    open(Path, write, Stream),
    write(Stream, '[]'),
    close(Stream).


start_persistence :- 
    make_data_folder,
    accs_path(AccsPath),
    matches_path(MatchesPath),
    make_json_file(AccsPath),
    make_json_file(MatchesPath).


read_json_file(Path, Json) :- 
    open(Path, read, Stream),
    json_read_dict(Stream, Json),
    close(Stream).


write_json_file(Path, Json) :- 
    open(Path, write, Stream),
    json_write(Stream, Json),
    close(Stream).


inc_json_file(Path, Json) :- 
    read_json_file(Path, Contents),
    append(Json, Contents, UpdatedContents),
    write_json_file(Path, UpdatedContents).


test :- 
    start_persistence,
    accs_path(AccsPath),

    inc_json_file(AccsPath, [json{accName:"b",accScore:0}, json{accName:"a",accScore:0}]).
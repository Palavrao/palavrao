:- include('Utils/utils.pl').
:- include('data/accounts.pl').
:- include('data/matches.pl').

main :- 
    accs_path(AccsPath),
    inc_fact_file(AccsPath, account("samuel", 20)),

    account("samuel", Pontos),
    write(Pontos).
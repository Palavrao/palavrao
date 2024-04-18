:- include('Utils/utils.pl').
:- include('Controllers/accs_controller.pl').
:- include('data/accounts.pl').
:- include('data/matches.pl').

main :- 
    accs_path(AccsPath),
    create_acc('samuel'),

    account('samuel', Score),
    write(Score). 
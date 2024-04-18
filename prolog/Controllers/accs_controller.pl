:- include('../Utils/utils.pl').
:- include('../Constants/paths.pl').

create_acc(AccName) :-
    accs_path(AccsPath),
    
    inc_fact_file(AccsPath, account(AccName, 0)).
:- include('../Utils/utils.pl').
:- include('../Constants/paths.pl').
:- dynamic(account/2).


get_acc(AccName, Acc) :- 
    account(AccName, AccScore),
    Acc = account(AccName, AccScore).


create_acc(AccName) :-
    account(AccName, _);
    accs_path(AccsPath),
    
    inc_fact_file(AccsPath, account(AccName, 0)).


inc_acc_score(AccName, IncScore) :- 
    accs_path(AccsPath),
    get_acc(AccName, Acc),

    account(AccName, OldScore),
    NewScore is OldScore+IncScore,

    update_fact_file(AccsPath, Acc, account(AccName, NewScore)).

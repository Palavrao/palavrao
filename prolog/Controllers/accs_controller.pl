:- dynamic(account/2).


get_acc(AccName, account(AccName, AccScore)).


get_acc_score(AccName, AccScore) :-
    account(AccName, AccScore).


create_acc(AccName) :-
    accs_path(AccsPath),
    
    inc_fact_file(AccsPath, account(AccName, 0), account).


inc_acc_score(AccName, IncScore) :- 
    accs_path(AccsPath),
    get_acc(AccName, Acc),

    get_acc_score(AccName,OldScore),
    NewScore is OldScore+IncScore,

    update_fact_file(AccsPath, Acc, account(AccName, NewScore), account).

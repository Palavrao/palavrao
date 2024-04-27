acc_exists(AccName) :-
    current_predicate(account/2),
    account(AccName, _), !.


get_acc(AccName, Account) :- 
    account(AccName, AccScore),
    Account = account(AccName, AccScore).


get_acc_score(AccName, AccScore) :-
    account(AccName, AccScore).


create_acc(AccName) :-
    \+ acc_exists(AccName),
    accs_path(AccsPath),
    
    inc_fact_file(AccsPath, account(AccName, 0), account), !.


inc_acc_score(AccName, IncScore) :- 
    accs_path(AccsPath),
    get_acc(AccName, Acc),

    get_acc_score(AccName,OldScore),
    NewScore is OldScore+IncScore,

    update_fact_file(AccsPath, Acc, account(AccName, NewScore), account).

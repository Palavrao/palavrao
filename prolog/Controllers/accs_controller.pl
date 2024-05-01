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


get_accs(Accs) :-
    findall(account(AccName, AccScore), account(AccName, AccScore), Accs).


get_accs_pairs(AccsPairs) :- 
    get_accs(Accs),
    get_accs_pairs(Accs, AccsPairs).


get_accs_pairs([], []).
get_accs_pairs([H|T], AccsPairs) :- 
    get_accs_pairs(T, AnotherAccsPairs),
    H =.. [_|[AccName, AccScore | _]],
    AccPair = [AccName, AccScore],
    AccsPairs = [AccPair | AnotherAccsPairs].


get_accs_rank(AccRank) :-
    get_accs_pairs(Accs),
    sort_by_second(Accs, SortedAccs),

    length(SortedAccs, Len),
    AccsQtd is min(5, Len),

    get_last_elements(SortedAccs, AccsQtd, AccRank).

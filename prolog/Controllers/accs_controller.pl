:- include('../Utils/utils.pl').
:- include('../Constants/paths.pl').
:- dynamic(account/2).

create_acc(AccName) :-
    (
        account(AccName, _) -> write('Account already created!')
        ;
        accs_path(AccsPath),
        
        inc_fact_file(AccsPath, account(AccName, 0))
    ).

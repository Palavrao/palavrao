:- include('Utils/utils.pl').
:- include('Controllers/accs_controller.pl').
:- include('Controllers/board_controller.pl').
:- include('data/accounts.pl').
:- include('data/matches.pl').
:- include('data/boards.pl').




     


main :- 
    accs_path(AccsPath),

    create_acc(samuel),

    inc_acc_score(samuel, 20),

    account(samuel,Score),

    create_board(bname),
    board(bname,C,_),

    write(C).
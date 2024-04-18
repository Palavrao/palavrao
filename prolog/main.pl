:- include('Utils/utils.pl').

main :- 
    start_persistence,
    accs_path(AccsPath),

    inc_facts_file(AccsPath, 'account("Samuel", 20)').
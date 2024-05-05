:- consult('Utils/utils.pl').

% Verifica se a conta existe 
% Recebe: Nome da conta
acc_exists(AccName) :-
    current_predicate(account/2),
    account(AccName, _), !.


% Retorna o átomo da conta
% Recebe: Nome da conta
% Retorna: O átomo no modelo account(name, score)
get_acc(AccName, Account) :- 
    account(AccName, AccScore),
    Account = account(AccName, AccScore).


% Retorna o score de uma conta
% Recebe: Nome da conta
% Retorna: Score da conta
get_acc_score(AccName, AccScore) :-
    account(AccName, AccScore).


% Cria a conta a partir de um nome, e salva na persistencia
% Recebe: Nome da conta a ser criada
create_acc(AccName) :-
    \+ acc_exists(AccName),
    accs_path(AccsPath),
    
    inc_fact_file(AccsPath, account(AccName, 0), account), !.


% Adiciona pontuação no score da conta especificada pelo nome
% Recebe: Nome e score para ser adicionado na conta
inc_acc_score(AccName, IncScore) :- 
    accs_path(AccsPath),
    get_acc(AccName, Acc),

    get_acc_score(AccName,OldScore),
    NewScore is OldScore+IncScore,

    update_fact_file(AccsPath, Acc, account(AccName, NewScore), account).


% Retorna todas as contas da persistência
% Retorna: Todas as contas registradas
get_accs(Accs) :-
    (current_predicate(account/2) ->
            findall(account(AccName, AccScore), account(AccName, AccScore), Accs);
            Accs = []).


% Retorna os pares das contas no modelo "[NomeDaConta, ScoreDaConta]"
% Retorna: Todos os pares representando as contas registradas
get_accs_pairs(AccsPairs) :- 
    get_accs(Accs),
    get_accs_pairs(Accs, AccsPairs).
get_accs_pairs([], []).
get_accs_pairs([H|T], AccsPairs) :- 
    get_accs_pairs(T, AnotherAccsPairs),
    H =.. [_|[AccName, AccScore | _]],
    AccPair = [AccName, AccScore],
    AccsPairs = [AccPair | AnotherAccsPairs].


% Retorna os pares representando as contas dos top 5 melhores jogadores
% Retorna: As 5 contas com melhores pontuações registradas, sendo representadas por pares,
% e caso os usuarios tenham suas pontuações zeradas, retorna todos os usuarios
get_accs_rank(AccRank) :-
    get_accs_pairs(Accs),
    (all_scores_zero(Accs) ->
        AccRank = Accs;
        (sort_by_second(Accs, SortedAccs),
        length(SortedAccs, Len),
        AccsQtd is min(5, Len),
        get_last_elements(SortedAccs, AccsQtd, AccRank))).
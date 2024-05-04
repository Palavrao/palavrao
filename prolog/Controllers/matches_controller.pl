
% Verifica se a partida selecionada existe
% Recebe: Nome da partida
match_exists(MatchName) :-
    current_predicate(match/9),
    match(MatchName, _, _, _, _, _, _, _, _), !.


% Retorna o átomo da partida no fomato match(NomeDaPartida, NomeDoBoardDaPartida, TurnoDaPartida, NomeDoPlayer1, NomeDoPlayer2, [LetrasDaPartida], [PalavrasUsadasNaPartida], TimerDaPartida, QuantidadeDeSkipsDaPartida)
% Recebe: Nome da partida
% Retorna: Átomo da partida
get_match(MatchName, Match) :- 
    match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, MatchTimer, MatchSkips),
    Match = match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, MatchTimer, MatchSkips).


% Retorna o nome do board da partida
% Recebe: Nome da partida
% Retorna: Nome do board da partida
get_match_board_name(MatchName, MatchBoardName) :-
    match(MatchName, MatchBoardName,_,_,_,_,_,_,_).


% Retorna o turno em que a partida está, false se for do player1, true para o player2
% Recebe: Nome da partida
% Retorna: Turno da partida
get_match_turn(MatchName, MatchTurn) :- 
    match(MatchName, _, MatchTurn, _, _, _, _, _, _).


% Retorna o nome do player1 da partida
% Recebe: Nome da partida
% Retorna: Nome do player1 da partida
get_match_p1_name(MatchName, P1Name) :- 
    match(MatchName, _, _, P1Name, _, _, _, _, _).


% Retorna o nome do player2 da partida
% Recebe: Nome da partida
% Retorna: Nome do player2 da partida
get_match_p2_name(MatchName, P2Name) :- 
    match(MatchName, _, _, _, P2Name, _, _, _, _).


% Retorna as letras da partida
% Recebe: Nome da partida
% Retorna: Letras da partida em formato de "array"
get_match_letters(MatchName, MatchLetters) :- 
    match(MatchName, _, _, _, _, MatchLetters, _, _, _).


% Retorna as palavras ja usadas na partida
% Recebe: Nome da partida
% Retorna: Palavras usadas na partida
get_match_words(MatchName, MatchWords) :- 
    match(MatchName, _, _, _, _, _, MatchWords, _, _).


% Retorna o timer da partida
% Recebe: Nome da partida
% Retorna: O timer da partida
get_match_timer(MatchName, MatchTimer) :- 
    match(MatchName, _, _, _, _, _, _, MatchTimer, _).


% Retorna a quantidade de skips da partida
% Recebe: Nome da partida
% Retorna: A quantidade de skips da partida
get_match_skips(MatchName, MatchSkips) :- 
    match(MatchName, _, _, _, _, _, _, _, MatchSkips).


% Retorna o nome do player da rodada
% Recebe: Nome da partida
% Retorna: Nome do player da rodada
get_turn_player_name(MatchName, PlayerName) :-
    get_match_turn(MatchName, MatchTurn),

    (MatchTurn -> 
        get_match_p2_name(MatchName, PlayerName);
        get_match_p1_name(MatchName, PlayerName)).


% Cria uma partida com as letras iniciais, cria os players, e o board, além de adicionar as letras iniciais pra cada jogador, adicionando à persistência
% Recebe: Nome da partida e dos players
create_match(MatchName, P1Name, P2Name) :- 
    \+ match_exists(MatchName),
    acc_exists(P1Name),
    acc_exists(P2Name),
    
    matches_path(MatchesPath),

    create_player(MatchName, P1Name),
    create_player(MatchName, P2Name),
    
    atom_concat(MatchName, board, BoardName),
    create_board(BoardName),

    start_letters(StartLetters),
    inc_fact_file(MatchesPath, match(MatchName, BoardName, false, P1Name, P2Name, StartLetters, [], 300, 0), match),
    update_player_letters(MatchName),
    toggle_player_turn(MatchName),
    update_player_letters(MatchName),
    toggle_player_turn(MatchName), !.


% Deleta uma partida selecionada pelo nome da persistencia e do jogo
% Recebe: Nome da partida a ser deletada
del_match(MatchName) :-
    matches_path(MatchesPath),
    players_path(PlayersPath),
    boards_path(BoardsPath),

    get_match(MatchName, Match),
    get_match_board_name(MatchName, BoardName),
    get_board(BoardName, Board),
    get_match_p1_name(MatchName, P1Name),
    get_match_p2_name(MatchName, P2Name),
    get_player(MatchName, P1Name, P1),
    get_player(MatchName, P2Name, P2),

    del_fact_file(PlayersPath, P1, player),
    del_fact_file(PlayersPath, P2, player),
    del_fact_file(MatchesPath, Match, match),
    del_fact_file(BoardsPath, Board, board).


% Finaliza uma partida, adicionando os pontos recebidos aos jogadores e apagando a partida da persistencia
% Recebe: Nome da partida a ser finalizada
finish_match(MatchName) :- 
    match(MatchName, _, _, P1Name, P2Name, _, _, _, _),
    
    get_player_score(MatchName, P1Name, P1Score),
    get_player_score(MatchName, P2Name, P2Score),
    inc_acc_score(P1Name, P1Score),
    inc_acc_score(P2Name, P2Score),

    del_match(MatchName),!.


% Atualiza o timer da partida
% Recebe: Nome da partida e o timer a ser armazenado por ela
update_match_timer(MatchName, NewTimer) :- 
    matches_path(MatchesPath),

    get_match(MatchName, Match),
    match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, _, MatchSkips),

    update_fact_file(MatchesPath, Match, match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, NewTimer, MatchSkips), match).


% Atualiza as letras da partida
% Recebe: Nome da partida e letras a serem armazenadas por ela
update_match_letters(MatchName, NewLetters) :-
    matches_path(MatchesPath),

    get_match(MatchName, Match),
    match(MatchName, BoardName, MatchTurn, P1Name, P2Name, _, MatchWords, MatchTimer, MatchSkips),

    update_fact_file(MatchesPath, Match, match(MatchName, BoardName, MatchTurn, P1Name, P2Name, NewLetters, MatchWords, MatchTimer, MatchSkips), match).


% Retorna todas as partidas registradas na persistência em formato de átomos
% Retorna: Todas as partidas registradas em formato de átomos
get_matches(Matches) :- 
    findall(match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, MatchTimer, MatchSkips),
            match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, MatchTimer, MatchSkips),
            Matches).


% Retorna os nomes de todas as partidas registradas na persistência
% Retorna: Todas as partidas registradas em formato de "array" de "strings"
get_match_names(MatchesNames) :-
    get_matches(Matches),
    get_match_names(Matches, MatchesNames).
get_match_names([], []).
get_match_names([H|T], MatchesNames) :-
    get_match_names(T, AnotherMatchesNames),
    H =.. [_|[MatchName |_]],
    MatchesNames = [MatchName|AnotherMatchesNames].


% Aumenta o score do player do turno da partida
% Recebe: Nome da partida e score a ser adicionado ao player
inc_player_score(MatchName, PlayerScore) :- 
    get_turn_player_name(MatchName, PlayerName),

    inc_score(MatchName, PlayerName, PlayerScore), !.


% Atualiza as letras do player da rodada, removendo-as das letras da partida e preenchendo as do player até darem 7 letras
% Recebe: Nome da partida para as letras serem atualizadas
update_player_letters(MatchName) :- 
    get_match_letters(MatchName, MatchLetters),
    get_turn_player_name(MatchName, PlayerName),

    get_player_letters(MatchName, PlayerName, PlayerLetters),
    length(PlayerLetters, PlayerLettersLength),
    LettersQuantity is 7 - PlayerLettersLength,

    pop_random_elements(MatchLetters, LettersQuantity, RemovedLetters, UpdatedLetters),

    update_match_letters(MatchName, UpdatedLetters),
    add_letters(MatchName, PlayerName, RemovedLetters).


% Pula o turno do player da vez, adicionando +1 a quantidade de skips da partida e passando para o turno do proximo jogador
% Recebe: Nome da partida a ter o turno pulado
skip_player_turn(MatchName) :- 
    matches_path(MatchesPath),
    
    get_match(MatchName, Match),
    match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, MatchTimer, MatchSkips),
    
    NewMatchSkips is MatchSkips + 1,
    
    update_fact_file(MatchesPath, Match, match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, MatchTimer, NewMatchSkips), match),
    toggle_player_turn(MatchName).


% Retorna a quantidade de skips da partida para 0
% Recebe: Nome da partida a ter os skips retornado para 0
reset_match_skips(MatchName) :- 
    matches_path(MatchesPath),
    
    get_match(MatchName, Match),
    match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, MatchTimer, _),

    update_fact_file(MatchesPath, Match, match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, MatchTimer, 0), match).


% Passa o turno da partida para o próximo jogador
% Recebe: Nome da partida a ter o turno passado
toggle_player_turn(MatchName) :- 
    matches_path(MatchesPath),

    get_match(MatchName, Match),
    match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, _, MatchSkips),

    (MatchTurn -> 
        NewMatchTurn = false;
        NewMatchTurn = true),

    update_fact_file(MatchesPath, Match, match(MatchName, BoardName, NewMatchTurn, P1Name, P2Name, MatchLetters, MatchWords, 300, MatchSkips), match).


% Adiciona palavras as palavras usadas na partida
% Recebe: Nome da partida a ter as palavras adicionadas e as palavras a serem adicionadas
inc_match_used_words(MatchName, UsedWords) :- 
    matches_path(MatchesPath),

    get_match(MatchName, Match),
    match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, MatchWords, MatchTimer, MatchSkips),
    
    append(MatchWords, UsedWords, NewMatchWords),

    update_fact_file(MatchesPath, Match, match(MatchName, BoardName, MatchTurn, P1Name, P2Name, MatchLetters, NewMatchWords, MatchTimer, MatchSkips), match).


% Troca a letra do player da rodada por alguma aleatoria da partida
% Recebe: Nome da partida a ter a letra trocada pelo player e a letra que foi trocada pelo player
switch_player_letter(MatchName, Letter) :- 
    get_match_letters(MatchName, MatchLetters),
    pop_random_elements(MatchLetters, 1, NewLetter, UpdatedLetters),

    get_turn_player_name(MatchName, PlayerName),

    get_player_letters(MatchName, PlayerName, PlayerLetters),
    selectchk(Letter, PlayerLetters, UpdatedPlayerLetters),

    append(UpdatedPlayerLetters, NewLetter, FinalPlayerLetters),
    append(UpdatedLetters, [Letter], FinalMatchLetters),
    
    update_match_letters(MatchName, FinalMatchLetters),
    update_letters(MatchName, PlayerName, FinalPlayerLetters).


% Remove as letras passadas do jogador do turno atual da partida
% Recebe: Nome da partida e as letras que serão removidas do player
remove_player_letters(MatchName, ToRemove) :-
    get_turn_player_name(MatchName, PlayerName),

    get_player_letters(MatchName, PlayerName, PlayerLetters),

    remove_elements(PlayerLetters, ToRemove, Updated),

    update_letters(MatchName, PlayerName, Updated), !.

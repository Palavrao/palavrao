
% Verifica se o player com o nome e partida passados existe
% Recebe: Nome da partida e do player
player_exists(MatchName, PlayerName) :-
    current_predicate(player/4),
    player(MatchName, PlayerName, _, _), !.


% Cria um player que será reconhecido por seu nome e o nome da partida em que está, e salva na persistência
% Recebe: Nome da partida e do player
create_player(MatchName, PlayerName) :- 
    \+ player_exists(MatchName, PlayerName),
    players_path(PlayersPath),
    inc_fact_file(PlayersPath, player(MatchName, PlayerName, [], 0), player), !.


% Retorna o átomo do player no formato player(NomeDaPartida, NomeDoPlayer, [LetrasDoPlayer], ScoreDoPlayer)
% Recebe: Nome da partida e do player
% Retorna: Átomo do player
get_player(MatchName, PlayerName, Player) :- 
    player(MatchName, PlayerName, PlayerLetters, PlayerScore),
    Player = player(MatchName, PlayerName, PlayerLetters, PlayerScore).


% Retorna as letras do player selecionado
% Recebe: Nome da partida e do player
% Retorna: Array de letras do player
get_player_letters(MatchName, PlayerName, PlayerLetters) :- 
    player(MatchName, PlayerName, PlayerLetters, _).


% Retorna o score do player selecionado
% Recebe: Nome da partida e do player
% Retorna: Score do player
get_player_score(MatchName, PlayerName, PlayerScore) :-
    player(MatchName,PlayerName, _, PlayerScore).


% Aumenta o score do player selecionado
% Recebe: Nome da partida e do player e o score a ser adicionado ao player
inc_score(MatchName, PlayerName, IncScore) :- 
    players_path(PlayersPath),

    get_player(MatchName, PlayerName, Player),
    player(MatchName, PlayerName, PlayerLetters, PlayerScore),

    NewScore is PlayerScore + IncScore,

    update_fact_file(PlayersPath, Player, player(MatchName, PlayerName, PlayerLetters, NewScore), player).


% Atualiza as letras de um player
% Recebe: Nome da partida e do player e as novas letras que serão armazenadas pelo player
update_letters(MatchName, PlayerName, NewLetters) :- 
    players_path(PlayersPath),

    get_player(MatchName, PlayerName, Player),
    player(MatchName, PlayerName, _, PlayerScore),

    update_fact_file(PlayersPath, Player, player(MatchName, PlayerName, NewLetters, PlayerScore), player).


% Adicona novas letras nas letras armazenadas pelo jogador
% Recebe: Nome da partida e do player e as letras que serão adicionadas nas letras armazenadas pelo jogador
add_letters(MatchName, PlayerName, NewLetters) :- 
    players_path(PlayersPath),

    get_player(MatchName, PlayerName, Player),
    player(MatchName, PlayerName, PlayerLetters, PlayerScore),

    append(PlayerLetters, NewLetters, UpdatedLetters),

    update_fact_file(PlayersPath, Player, player(MatchName, PlayerName, UpdatedLetters, PlayerScore), player).

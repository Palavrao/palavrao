create_player(MatchName , PlayerName) :- 
    players_path(PlayersPath),
    inc_fact_file(PlayersPath, player(MatchName, PlayerName, [], 0), player), !.


get_player(MatchName, PlayerName, Player) :- 
    player(MatchName, PlayerName, PlayerLetters, PlayerScore),
    Player = player(MatchName, PlayerName, PlayerLetters, PlayerScore).


get_player_letters(MatchName, PlayerName, PlayerLetters) :- 
    player(MatchName, PlayerName, PlayerLetters, _).


get_player_score(MatchName, PlayerName, PlayerScore) :-
    player(MatchName,PlayerName, _, PlayerScore).


inc_score(MatchName, PlayerName, IncScore) :- 
    players_path(PlayersPath),

    get_player(MatchName, PlayerName, Player),
    player(MatchName, PlayerName, PlayerLetters, PlayerScore),

    NewScore is PlayerScore + IncScore,

    update_fact_file(PlayersPath, Player, player(MatchName, PlayerName, PlayerLetters, NewScore), player).


update_letters(MatchName, PlayerName, NewLetters) :- 
    players_path(PlayersPath),

    get_player(MatchName, PlayerName, Player),
    player(MatchName, PlayerName, _, PlayerScore),

    update_fact_file(PlayersPath, Player, player(MatchName, PlayerName, NewLetters, PlayerScore), player).


add_letters(MatchName, PlayerName, NewLetters) :- 
    players_path(PlayersPath),

    get_player(MatchName, PlayerName, Player),
    player(MatchName, PlayerName, PlayerLetters, PlayerScore),

    append(PlayerLetters, NewLetters, UpdatedLetters),

    update_fact_file(PlayersPath, Player, player(MatchName, PlayerName, UpdatedLetters, PlayerScore), player).

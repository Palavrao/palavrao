:- include('../Utils/utils.pl').
:- dynamic(player/4).


create_player(MatchName , PlayerName) :- 
    players_path(PlayersPath),
    inc_fact_file(PlayersPath, player(MatchName, PlayerName, [], 0)).


get_player(MatchName, PlayerName, Player) :- 
    player(MatchName, PlayerName, [], 0),
    Player is player(MatchName, PlayerName, [], 0).


get_player_letters(MatchName, PlayerName, PlayerScore) :- 
    player(MatchName, PlayerName, _, PlayerScore).


get_player_score(MatchName, PlayerName, PlayerScore) :-
    player(MatchName,PlayerName, _, PlayerScore).


inc_score(MatchName, PlayerName, IncScore) :- 
    players_path(PlayersPath),

    get_player(MatchName, PlayerName, Player),
    get_player_letters(MatchName, PlayerName, PlayerLetters),
    get_player_score(MatchName, PlayerName, PlayerScore),

    NewScore is PlayerScore + IncScore,

    update_fact_file(PlayersPath, Player, player(MatchName, PlayerName, PlayerLetters, NewScore)).


update_letters(MatchName, PlayerName, NewLetters) :- 
    players_path(PlayersPath),

    get_player(MatchName, PlayerName, Player),
    get_player_score(MatchName, PlayerName, PlayerScore),

    update_fact_file(PlayersPath, Player, player(MatchName, PlayerName, NewLetters, NewScore)).


add_letters(MatchName, PlayerName, NewLetters) :- 
    players_path(PlayersPath),

    get_player(MatchName, PlayerName, Player),
    get_player_letters(MatchName, PlayerName, PlayerLetters),
    get_player_score(MatchName, PlayerName, PlayerScore),

    append(PlayerLetters, NewLetters, UpdatedLetters),

    update_fact_file(PlayersPath, Player, player(MatchName, PlayerName, NewLetters, UpdatedLetters)).
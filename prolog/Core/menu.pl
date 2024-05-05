:- consult('Interfaces/boxes_menu.pl').
:- consult('Utils/validator.pl').
:- consult('Utils/utils.pl').
:- consult('Controllers/matches_controller.pl').
:- consult('Controllers/accs_controller.pl').

:- dynamic(current_screen/1).
:- dynamic(screen/2).
screen(redimension_screen, []).

% recebe a acao feita, o menu atual exibido e o proximo, e atualiza o menu mostrado
update_menu(Action, CurrentMenu, UpdatedMenu) :-
    (menu(Action, NovoMenu) -> UpdatedMenu = NovoMenu ; UpdatedMenu = CurrentMenu).

% recebe o menu (uma lista de strings) e imprime ele na tela
writeln_menu([]).
writeln_menu([Line|Rest]) :-
    writeln(Line),
    writeln_menu(Rest).

% recebe uma acao e atualiza o menu mostrado pro usuario no terminal
show_menu(Action) :-
    clear_screen,
    update_menu(Action, [], UpdatedMenu),
    writeln_menu(UpdatedMenu).

% processa a entrada recebida pelo usuario para transicao de telas
get_input(Key) :-
    get_single_char(Code),
    char_code(Key, Code).

% fluxo de entrada no jogo
% ao receber 0 como entrada, entra no menu inicial do jogo
process_input('0') :-
    current_screen(redimension_screen),
    back_to_start_menu.

% fluxo de menu inicial
% ao receber 1 como entrada, transiciona para a tela de criacao de novo jogo, onde
% faz interacao com o usuario para receber nome de partida e de jogadores, iniciando um jogo
process_input('1') :-
    current_screen(start_menu),
    retract(current_screen(_)),
    assertz(current_screen(new_game)),
    show_menu(new_game),
    write_new_match(NewMatchName, Player1, Player2).

% ao receber 2 como entrada, transiciona para a tela de continuacao de jogo
process_input('2') :-
    current_screen(start_menu),
    retract(current_screen(_)),
    assertz(current_screen(continue_game)),
    show_menu(continue_game).

% ao receber 3 como entrada, imprime no terminal as regras do jogo
process_input('3') :-
    current_screen(start_menu),
    clear_screen,
    regras,
    back_to_start_menu.

% ao receber 4 como entrada, imprime o ranking dos 5 jogadores com as maiores pontuacoes
process_input('4') :-
    current_screen(start_menu),
    rank,
    read_line_to_codes(user_input, Codes),
    string_codes(_, Codes),
    back_to_start_menu.

% ao receber 5 como entrada, finaliza a execucao do programa
process_input('5') :-
    current_screen(start_menu),
    writeln('saindo...'), halt.

% fluxo de continuar jogo
% ao receber 1 como entrada, faz interacao com o usuario para
% receber nome da partida existente, continuando o jogo
process_input('1') :-
    current_screen(continue_game),
    write_existing_match(MatchName).

% ao receber 2 como entrada, imprime o nome das partidas criadas ate o momento no jogo
process_input('2') :-
    current_screen(continue_game),
    list_matches,
    read_line_to_codes(user_input, Codes),
    string_codes(Out, Codes),
    retract(current_screen(_)),
    assertz(current_screen(continue_game)),
    show_menu(continue_game).

% ao receber 3 como entrada, retorna ao menu inicial
process_input('3') :-
    current_screen(continue_game),
    back_to_start_menu.

% ao receber alguma outra entrada nao definida, nao executa acao alguma
process_input(_).

% lida com entrada do usuario para continuacao de jogo:
% recebe entrada, faz validacao de letras minusculas e caracteres,
% verifica se a partida existe para entrar no loop,
% e caso o usuario digite 0, ele volta pro menu de continuacao
write_existing_match(MatchName) :-
    writeln('digite o nome da partida (ou digite 0 para voltar)>'),
    read_line_to_codes(user_input, Codes),
    string_codes(MatchName, Codes),
    check_lowercase(MatchName, LowerMatchName),

    (LowerMatchName = "0" ->
        retract(current_screen(_)),
        assertz(current_screen(continue_game)),
        show_menu(continue_game), fail;
        (match_exists(LowerMatchName) ->
            (clear_screen,
            game_loop(LowerMatchName, ''),
            clear_screen,
            back_to_start_menu);
            writeln("partida não existe, tente novamente."), write_existing_match(MatchesNames))).

% pega retorno com nome de partida e com os jogadores
% e passa pro setup do jogo
write_new_match(NewMatchName, Player1, Player2) :-
    write_match(NewMatchName),
    write_player(1, Player1),
    write_player(2, Player2),
    setup_game(NewMatchName, Player1, Player2).

% recebe entrada, valida caracteres,
% verifica se a partida nao existe ainda para poder ser criada,
write_match(NewMatchName) :-
    read_match_input(LowerNewMatchName),

	(valid_name(LowerNewMatchName) ->
		(match_exists(LowerNewMatchName) ->
			(writeln("partida com esse nome já existe."),
                  write_match(LowerMatchName));
                writeln("partida ok, agora digite os jogadores"));
            (writeln("partida com nome invalido."),
             write_match(LowerMatchName))).

% lida com entrada do usuario para novo jogo:
% recebe entrada, valida letras minusculas,
% e caso o usuario digite 0, ele volta pro menu de continuacao
read_match_input(LowerNewMatchName) :-
    writeln('digite o nome da nova partida (ou digite 0 para voltar)>'),
    read_line_to_codes(user_input, Codes),
    string_codes(NewMatchName, Codes),
    check_lowercase(NewMatchName, LowerNewMatchName),
    (LowerNewMatchName = "0" ->
        back_to_start_menu, fail;
        true).

% lida com entrada do usuario para login/criacao de jogador:
% recebe entrada, faz validacao de letras minusculas e caracteres,
% se a conta com o nome dado existe o usuario é logado,
% caso nao exista uma nova conta é criada,
% e caso o usuario digite 0, ele volta pro menu de continuacao
write_player(PlayerNumber, Player) :-
    format("digite o nome do player ~w (ou digite 0 para voltar)>\n", [PlayerNumber]),
    read_line_to_codes(user_input, Codes),
    string_codes(PlayerInput, Codes),
    check_lowercase(PlayerInput, LowerPlayerInput),

    (LowerPlayerInput = "0" ->
        back_to_start_menu, fail;
        (valid_name(LowerPlayerInput) ->
            (acc_exists(LowerPlayerInput) ->
                (get_acc(LowerPlayerInput, Player),
                 writeln("jogador logado com sucesso."));
                (create_acc(LowerPlayerInput),
                 writeln("conta criada com sucesso."),
                 get_acc(LowerPlayerInput, Player)));
            writeln("nome inválido, insira um nome válido."),
            write_player(PlayerNumber, Player))).

% listagem de partidas para o usuario
list_matches :-
    writeln('\n----- partidas criadas -----'),
    print_matches,
    writeln('----------------------------'),
    writeln('Enter para voltar>').

% pega as partidas criadas, verifica se ha partidas, e retorna a listagem
print_matches :-
    get_match_names(MatchesNames),
    (length(MatchesNames, 0) ->
        writeln('nao ha partidas criadas');
        format_matches(MatchesNames, 1)).

% auxiliar para a listagem de partidas, formata saida para mostrar ordem e nome
format_matches([], _).
format_matches([Head|Rest], Order) :-
    format('~w - ~w~n', [Order, Head]),
    NextOrder is Order + 1,
    format_matches(Rest, NextOrder).

% listagem de rank para o usuario
rank :-
    writeln('\n------- rank -------'),
    print_accounts,
    writeln('--------------------'),
    writeln('Enter para voltar>').

% pega o rank das contas, verifica se ha contas criadas, e retorna a listagem
print_accounts :-
    get_accs_rank(AccRank),
    (length(AccRank, 0) ->
        writeln('nao ha contas criadas');
        (reverse_list(AccRank, Reversed), format_accounts(Reversed, 1))).

% auxiliar para a listagem do rank, formata saida para mostrar posicao,
% nome de usuario e pontuacao obtida
format_accounts([], _).
format_accounts([[AccName, AccScore]|Rest], Order) :-
    format('~w - ~w: ~w~n', [Order, AccName, AccScore]),
    NextOrder is Order + 1,
    format_accounts(Rest, NextOrder).

% recebe o nome da nova partida e os dois jogadores dela
% pega o nome dos usuarios, cria nova partida e entra no loop dela
% ao finalizar, volta ao menu inicial
setup_game(NewMatchName, Player1, Player2) :-
    get_account_name(Player1, Player1Name),
    get_account_name(Player2, Player2Name),
    create_match(NewMatchName, Player1Name, Player2Name),
    clear_screen,
    writeln("partida cadastrada, vamos jogar!"),
    game_loop(NewMatchName, ''),
    clear_screen,
    back_to_start_menu.

% limpa conteudo da tela e mostra o menu inicial do jogo
back_to_start_menu :-
    retract(current_screen(_)),
    assertz(current_screen(start_menu)),
    show_menu(start_menu).
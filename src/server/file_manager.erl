-module(file_manager).
-export([start/0, 
		stop/0,
		create_account/2, 
		close_account/2, 
		login/2, 
		logout/1,
		online/0,
		test/0,
		handle/2,
		level_up/1,
		level_down/1,
		check_level/1,
		start_game/2,
		end_game/2]).

%Inicia o servidor e o levels_manager como processos que correm determinadas funções.

%documentar melhor, so esbocei

start() ->
	%{error, enoent} quando não existe o ficheiro
	case file:read_file("passwords") of
		{error, enoent} -> file:open("passwords", [write, binary]), Passwords = #{};
		{ok, <<>>} -> Passwords = #{};
		{ok, PassBin} -> Passwords = erlang:binary_to_term(PassBin)
	end,
	case file:read_file("levels") of
		{error, enoent} -> file:open("levels", [write, binary]), Levels = #{};
		{ok, <<>>} -> Levels = #{};
		{ok, LevelsBin} -> Levels = erlang:binary_to_term(LevelsBin)
	end,
	register(file_manager, spawn(fun() -> loop(Passwords) end)), 
	register(level_manager, spawn(fun() -> loop_levels(Levels) end)),
	{Passwords, Levels}.
	%se o file_manager for um processo pode ser que não funcione se for ultrapassado
	%register(file_manager, spawn(fun() -> file_manager() end)).

%Envia uma certa mensagem ao servidor esperando uma resposta do mesmo.
invoke(Request) ->
	file_manager ! {Request, self()},
	receive {Res, file_manager} -> Res end.

%Permite a um cliente criar uma conta.
create_account(Username, Passwd) ->
	invoke({create_account, Username, Passwd}).
	
%Informa o level_manager que um jogador deve subir de nível. Recebe como resposta o novo nível do jogador.
level_up(Username) ->
	level_manager ! {level_up, Username, self()},
	receive {Status, New_Level, level_manager} -> {Status, New_Level} end.

%Informa o level_manager que um jogador deve descer de nível. Recebe como resposta o novo nível do jogador.
level_down(Username) ->
	level_manager ! {level_down, Username, self()},
	receive {Status, New_Level, level_manager} -> {Status, New_Level} end.

%Permite-nos saber em que nível se encontra um determinado jogador.
check_level(Username) ->
	level_manager ! {check_level, Username, self()},
	receive {Status, Level, level_manager} -> {Status, Level} end.

%Informa o level_manager que se pretende iniciar um jogo entre dois jogadores. Obtém como resposta os níveis dos dois jogadores que devem ser iguais para que o jogo se inicie.
start_game(FstUsername, SecUsername) ->
	level_manager ! {start_game, FstUsername, SecUsername, self()},
	receive {Status, FstLevel, SecLevel, level_manager} -> {Status, FstLevel, SecLevel} end.

%Informa o level_manager que um jogo terminou e indica o respetivo vencedor. Recebe como resposta os novos níveis dos dois jogadores.
end_game(Winner, Loser) ->
	level_manager ! {end_game, Winner, Loser, self()},
	receive {Status, WinnerLevel, LoserLevel, level_manager} -> {Status, WinnerLevel, LoserLevel} end.

%Permite fechar a conta de um jogador.
close_account(Username, Passwd) ->
	invoke({close_account, Username, Passwd}).

%Permite o login de um utilizador.
login(Username, Passwd) ->
	invoke({login, Username, Passwd}).

%Permite um jogador ficar offline.
logout(Username) ->
	invoke({logout, Username}).

%Devolve a lista dos jogadores que estão online.
online() ->
	invoke(online).

%Espera receber diversos tipos de pedido e retorna um novo estado a ser chamado pela função do servidor recursivamente.
handle(Request, Map) ->
	case Request of 
		{create_account, Username, Passwd} -> 
			case maps:find(Username, Map) of
				error -> 
					level_manager ! {set_level, Username, self()},
					{ok, Map#{Username => Passwd}};
				_ ->
					{user_exists, Map}
			end;
		{close_account, Username, Passwd} -> 
			case maps:find(Username, Map) of
				{ok, Passwd} -> 
					{ok, maps:remove(Username, Map)};
				{ok, Data} ->
					io:format("~p ~p\n", [Passwd, Data]),
					{wrong_password, Map};
				_ ->
					{invalid, Map}
			end;
		{login, Username, Passwd} ->
			io:format("~p ~p\n", [Username, Passwd]),
			case maps:find(Username, Map) of
				{ok, Passwd} -> 
					{ok, Map};
				{ok, _ } ->
					{invalid_password, Map};
				_ ->
					{invalid, Map}
			end
		%{logout, Username} -> 
		%	case maps:find(Username, Map) of
		%		{ok, {Passwd, true}} -> 
		%			{ok, maps:update(Username, {Passwd, false}, Map)};
		%		_ ->
		%			{invalid, Map}
		%	end
		%Não faz sentido ser aqui que se regista quem está ou não online, isso é problema do servidor, isto vai ser só gestão de contas
		%Só devia ser permitido chegar a estas funções se já estiver online.
		%online ->
		%	Res = [User || {User, {_, true}} <- maps:to_list(Map)], 
		%	{Res, Map}
	end.

%Função que corre no servidor. Conforma a mensagem recebida, obtém um novo estado do mapa na função handle, responde ao cliente e executa-se recursivamente.
loop(Map) ->
	receive
		{{create_account, Username, Passwd}, From} ->
			{Res, NextState} = handle({create_account, Username, Passwd}, Map),
			From ! {Res, file_manager},
			loop(NextState);

		{Request, From} ->
			{Res, NextState} = handle(Request, Map),
			From ! {Res, file_manager},
			loop(NextState);
		stop ->
			file:write_file("passwords", erlang:term_to_binary(Map))
	end.

%Função que corre recursivamente no levels_manager. Espera uma mensagem para manipulação de níveis/jogos e responde diretamente ao cliente. Chama-se recursivamente com possíevis novos estados.
loop_levels(Map) ->
	receive
		{set_level, Username, From} ->
			case maps:find(Username, Map) of
				error ->
					From ! {ok,1, level_manager}, 
					loop_levels(maps:put(Username, {1, 0}, Map));
				{ok, {Level,_}} ->
					From ! {user_exists, Level, level_manager},
					loop_levels(Map)
			end;
		{level_up, Username, From} ->
			case maps:find(Username, Map) of
				{ok, {Level,_}} ->
					From ! {ok, Level+1, level_manager}, 
					loop_levels(maps:put(Username, {Level+1, 0}, Map));
				error ->
					From ! {invalid_user, 0, level_manager},
					loop_levels(Map)
			end;
		{level_down, Username, From} ->
			case maps:find(Username, Map) of
				{ok, {1,_}} ->
					From ! {ok,1, level_manager}, 
					loop_levels(Map);
				{ok, {Level,_}} ->
					From ! {ok, Level-1, level_manager},
					loop_levels(maps:put(Username, {Level-1, 0}, Map));
				error ->
					From ! {invalid_user, 0, level_manager},
					loop_levels(Map)
			end;
		{check_level, Username, From} ->
			case maps:find(Username, Map) of 
				error ->
					From ! {invalid_user, 0, level_manager},
					loop_levels(Map);
				{ok, {Level, _}} ->
					From ! {ok, Level, level_manager},
					loop_levels(Map)
			end;
		{end_game, Winner, Loser, From} ->
			case maps:find(Winner, Map) of 
				error ->
					From ! {invalid_winner, 0, 0, level_manager},
					loop_levels(Map);
				{ok, {Level, Wins}} ->
					case maps:find(Loser, Map) of 
						error ->
							From ! {invalid_loser, 0, 0, level_manager},
							loop_levels(Map);
						{ok, {OtherLevel, _}} ->
							if Wins+1 == Level*2 -> NewLevel = Level+1, NewWins = 0 ;
							   true -> NewLevel = Level, NewWins = Wins+1
							end,
							From ! {ok, NewLevel, OtherLevel, level_manager},
							loop_levels(Map#{Winner => {NewLevel, NewWins}})
					end
			end;
		stop ->
			file:write_file("levels", erlang:term_to_binary(Map))
	end.


%Envia ao servidor e ao levels_manager uma mensagem para pararem a sua execução.
stop() ->
	file_manager ! stop,
	level_manager ! stop,
	ok.

test() ->
	start(),
	A = create_account("hugo_rocha", "pw123"),
	B = create_account("hugo_rocha_sec", "secondpw"),
	K = login("hugo_rocha", "pw123"),
	L = login("hugo_rocha_sec", "secondpw"),
	C = level_up("hugo_rocha"),
	D = level_up("hugo_rocha"),
	E = level_up("hugo_rocha"),
	F = level_up("hugo_rocha"),
	G = create_account("hugo_rocha", "xd"),
	H = level_up("hugo_rochada"),
	I = level_down("hugo_rocha"),
	level_up("hugo_rocha_sec"),
	level_up("hugo_rocha_sec"),
	level_up("hugo_rocha_sec"),
	J = start_game("hugo_rocha", "hugo_rocha_sec"),
	Z = stop(),
	{A,B,K,L,C,D,E,F,G,H,I,J,Z}.
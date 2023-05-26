-module(file_manager).
-export([start/0, 
		stop/0,
		create_account/2, 
		close_account/2, 
		login/2, 
		logout/1,
		online/0,
		handle/2,
		check_level/1,
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
invoke_file(Request) ->
	file_manager ! {Request, self()},
	receive {Res, file_manager} -> Res end.

invoke_level(Request) ->
	level_manager ! {Request, self()},
	receive {Res, level_manager} -> Res end.

%Permite a um cliente criar uma conta.
create_account(Username, Passwd) ->
	invoke_file({create_account, Username, Passwd}).

%Permite-nos saber em que nível se encontra um determinado jogador.
check_level(Username) ->
	invoke_level({check_level, Username}).

%Informa o level_manager que um jogo terminou e indica o respetivo vencedor. Recebe como resposta os novos níveis dos dois jogadores.
end_game(Winner, Loser) ->
	invoke_level({end_game, Winner, Loser}).

%Permite fechar a conta de um jogador.
close_account(Username, Passwd) ->
	invoke_file({close_account, Username, Passwd}).

%Permite o login de um utilizador.
login(Username, Passwd) ->
	invoke_file({login, Username, Passwd}).

%Permite um jogador ficar offline.
logout(Username) ->
	invoke_file({logout, Username}).

%Devolve a lista dos jogadores que estão online.
online() ->
	invoke_file(online).

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
					level_manager ! {close_account, Username, self()},
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
			end;
		_ -> ok
	end.

%Função que corre no servidor. Conforma a mensagem recebida, obtém um novo estado do mapa na função handle, responde ao cliente e executa-se recursivamente.
loop(Map) ->
	receive
		{Request, From} ->
			{Res, NextState} = handle(Request, Map),
			From ! {Res, file_manager},
			loop(NextState);
		stop ->
			file:write_file("passwords", erlang:term_to_binary(Map))
	end.

handle_levels(Request, Map) ->
	case Request of
		{close_account, Username} ->
			case maps:find(Username, Map) of
				error ->
					{user_not_exist, Map};
				{ok, _} ->
					{ok, maps:remove(Username, Map)}
			end;
		{set_level, Username} ->
			case maps:find(Username, Map) of
				error ->
					{ok, maps:put(Username, {1, 0}, Map)};
				{ok, _} ->
					{user_exists, Map#{Username => {1,0}}}
			end;
		{check_level, Username} ->
			case maps:find(Username, Map) of 
				error ->
					{invalid_user, Map};
				{ok, {Level, _}} ->
					{{ok, Level}, Map}
			end;
		{end_game, Winner, Loser} ->
			case maps:find(Winner, Map) of 
				error ->
					{{invalid_winner, 0, 0}, Map};
				{ok, {Level, Wins}} ->
					case maps:find(Loser, Map) of 
						error ->
							{{invalid_loser, 0, 0}, Map};
						{ok, {OtherLevel, _}} ->
							if Wins+1 == Level*2 -> NewLevel = Level+1, NewWins = 0 ;
							   true -> NewLevel = Level, NewWins = Wins+1
							end,
							{{ok, NewLevel, OtherLevel}, Map#{Winner => {NewLevel, NewWins}}}
					end
			end
	end.

%Função que corre recursivamente no levels_manager. Espera uma mensagem para manipulação de níveis/jogos e responde diretamente ao cliente. Chama-se recursivamente com possíevis novos estados.
loop_levels(Map) ->
	receive
		{Request, From} ->
			{Res, NextState} = handle_levels(Request, Map),
			From ! {Res, level_manager},
			loop_levels(NextState);
		stop ->
			file:write_file("levels", erlang:term_to_binary(Map))
	end.


%Envia ao servidor e ao levels_manager uma mensagem para pararem a sua execução.
stop() ->
	file_manager ! stop,
	level_manager ! stop,
	ok.
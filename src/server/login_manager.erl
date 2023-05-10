-module(login_manager).
-export([start/0, 
		create_account/2, 
		close_account/2, 
		login/2, 
		logout/1,
		online/0,
		test/0,
		handle/2]).


start() ->
	register(?MODULE, spawn(fun() -> loop(#{}) end)). 


invoke(Request) ->
	?MODULE ! {Request, self()},
	receive {Res, ?MODULE} -> Res end.


create_account(Username, Passwd) ->
	invoke({create_account, Username, Passwd}).

close_account(Username, Passwd) ->
	invoke({close_account, Username, Passwd}).

login(Username, Passwd) ->
	invoke({login, Username, Passwd}).

logout(Username) ->
	invoke({logout, Username}).

online() ->
	invoke(online).

handle(Request, Map) ->
	case Request of 
		{create_account, Username, Passwd} -> 
			case maps:find(Username, Map) of
				error -> 
					{ok, Map#{Username => {Passwd,false}}};
				_ ->
					{user_exists, Map}
			end;
		{close_account, Username, Passwd} -> 
			case maps:find(Username, Map) of
				{ok, {Passwd, _}} -> 
					{ok, maps:remove(Username, Map)};
				_ ->
					{invalid, Map}
			end;
		{login, Username, Passwd} ->
			case maps:find(Username, Map) of
				{ok, {Passwd, _}} -> 
					{ok, maps:update(Username, {Passwd, true}, Map)}; 
				_ ->
					{invalid, Map}
			end;
		{logout, Username} -> 
			case maps:find(Username, Map) of
				{ok, {Passwd, true}} -> 
					{ok, maps:update(Username, {Passwd, false}, Map)};
				_ ->
					{invalid, Map}
			end;
		online ->
			Res = [User || {User, {_, true}} <- maps:to_list(Map)], 
			{Res, Map}
	end.


loop(Map) ->
	receive
		{Request, From} ->
			{Res, NextState} = handle(Request, Map),
			From ! {Res, ?MODULE},
			loop(NextState)
	end.

test() ->
	start(),
	create_account("hugo_rocha", "pw123"),
	create_account("hugo_rocha_sec", "secondpw"),
	login("hugo_rocha", "pw123"),
	R = login("hugo_rocha_sec", "assdas"),
	R.




	

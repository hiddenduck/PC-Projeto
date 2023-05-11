-module(login_manager).
-export([start/0, 
		create_account/2, 
		close_account/2, 
		login/2, 
		logout/1,
		online/0,
		test/0,
		handle/2,
		level_up/1,
		level_down/1,
		start_match/2]).


start() ->
	register(?MODULE, spawn(fun() -> loop(#{}) end)), 
	register(levels, spawn(fun() -> loop_levels(#{}) end)).


invoke(Request) ->
	?MODULE ! {Request, self()},
	receive {Res, ?MODULE} -> Res end.


create_account(Username, Passwd) ->
	invoke({create_account, Username, Passwd}).
	
level_up(Username) ->
	levels ! {level_up, Username, self()},
	receive {Status, New_Level, levels} -> {Status, New_Level} end.

level_down(Username) ->
	levels ! {level_down, Username, self()},
	receive {Status, New_Level, levels} -> {Status, New_Level} end.

start_match(FstUsername, SecUsername) ->
	levels ! {start_match, FstUsername, SecUsername, self()},
	receive {Status, FstLevel, SecLevel, levels} -> {Status, FstLevel, SecLevel} end.

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
					levels ! {set_level, Username, self()},
					{ok, 1, Map#{Username => {Passwd,false}}};
				_ ->
					{user_exists, 0, Map}
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
		{{create_account, Username, Passwd}, From} ->
			{Res, Level, NextState} = handle({create_account, Username, Passwd}, Map),
			From ! {{Res,Level}, ?MODULE},
			loop(NextState);

		{Request, From} ->
			{Res, NextState} = handle(Request, Map),
			From ! {Res, ?MODULE},
			loop(NextState)
		
	end.

loop_levels(Map) ->
	receive
		{set_level, Username, From} ->
			case maps:find(Username, Map) of
				error ->
					From ! {ok,1, levels}, 
					loop_levels(maps:put(Username, 1, Map));
				{ok, Level} ->
					From ! {user_exists, Level, levels},
					loop_levels(Map)
			end;
		{level_up, Username, From} ->
			case maps:find(Username, Map) of
				{ok, Level} ->
					From ! {ok, Level+1, levels}, 
					loop_levels(maps:put(Username, Level+1, Map));
				error ->
					From ! {invalid_user, 0, levels},
					loop_levels(Map)
			end;
		{level_down, Username, From} ->
			case maps:find(Username, Map) of
				{ok, 1} ->
					From ! {ok,1, levels}, 
					loop_levels(Map);
				{ok, Level} ->
					From ! {ok, Level-1, levels},
					loop_levels(maps:put(Username, Level-1, Map));
				error ->
					From ! {invalid_user, 0, levels},
					loop_levels(Map)
			end;
		{start_match, FstUsername, SecUsername, From} ->
			case maps:find(FstUsername, Map) of 
				error ->
					From ! {invalid_fstuser, 0, 0, levels},
					loop_levels(Map);
				{ok, Level} ->
					case maps:find(SecUsername, Map) of 
						error ->
							From ! {invalid_secuser, 0, 0, levels},
							loop_levels(Map);
						{ok, Level} ->
							From ! {ok, Level, Level, levels},
							loop_levels(Map);
						{ok, OtherLevel} ->
							From ! {different_levels, Level, OtherLevel, levels},
							loop_levels(Map)

					end
			end
	end.


test() ->
	start(),
	create_account("hugo_rocha", "pw123"),
	create_account("hugo_rocha_sec", "secondpw"),
	login("hugo_rocha", "pw123"),
	login("hugo_rocha_sec", "secondpw"),
	R = level_up("hugo_rocha"),
	U = level_up("hugo_rocha"),
	I = level_up("hugo_rocha"),
	O = level_up("hugo_rocha"),
	S = create_account("hugo_rocha", "xd"),
	K = level_up("hugo_rochada"),
	D = level_down("hugo_rocha"),
	level_up("hugo_rocha_sec"),
	level_up("hugo_rocha_sec"),
	level_up("hugo_rocha_sec"),
	T = start_match("hugo_rocha", "hugo_rocha_sec"),
	{R,U,I,O,S,K,D, T}.




	

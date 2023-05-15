-module(space_server).
-export([start/1, stop/0]). % server:start(1234)
                            % nc localhost 1234
                            % netstat -p tcp -an | grep 1234

start(Port) -> register(?MODULE, spawn(fun() -> server(Port) end)).

stop() -> ?MODULE ! stop.

server(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [{packet, line}, {reuseaddr, true}]),
    Lobby = spawn(fun()-> lobby([]) end),
    register(game_manager, spawn(fun() -> game_manager(#{0=>Lobby}, []) end)),
    spawn(fun() -> acceptor(LSock, Lobby) end),
    receive stop -> ok end.

acceptor(LSock, Room) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock, Room) end),
    main_menu(Sock, Room).

main_menu(Sock, Room) ->
    receive
        {tcp, _, "create:" ++ Data} ->
            [Username, Password] = re:split(Data, "[:]"),
            case login_manager:create_account(Username, Password) of
                ok -> gen_tcp:send(Sock, "create:ok");
                user_exists -> gen_tcp:send(Sock, "create:error_user_exists")
            end,
            main_menu(Sock, Room);
        {tcp, _, "login:" ++ Data} -> 
            [Username, Password] = re:split(Data, "[:]"),
            case login_manager:login(Username, Password) of
                ok ->
                    Room ! {enter, "lobby", self()},
                    gen_tcp:send(Sock, "login:ok"),
                    user(Sock, Room, Username);
                invalid_password ->
                    gen_tcp:send(Sock, "login:error_invalid_password"),
                    main_menu(Sock, Room);
                _ ->
                    gen_tcp:send(Sock, "login:error_unknown_username"),
                    main_menu(Sock, Room)
            end;
        {tcp_error, _, _} -> ok;
        {tcp_closed, _, _} -> ok;
        _ -> gen_tcp:send(Sock, "login:error_unknown_command")
    end.

game_manager(Room_map, Game_Rooms) ->
    receive
        {unready, Level, User} -> 
            case maps:find(Level, Room_map) of
                {ok, {Room_pid, User}} ->
                    New_Map = maps:remove(Level, Room_map),
                    Room_pid ! {cancel, game_manager},
                    User ! {ok, game_manager};
                _ ->
                    %Se alguém estiver ready e não for este não se faz nada?
                    New_Map = Room_map,
                    User ! {error_not_ready, game_manager}    
            end,
            game_manager(New_Map, Game_Rooms);
        {ready, Level, User} ->
            case maps:find(Level, Room_map) of
                {ok, User} ->
                    User ! {error_already_ready, game_manager},
                    game_manager(Room_map, Game_Rooms);
                {ok, Fst} ->
                    %juntar-se ao jogo
                    New_Map = maps:remove(Level, Room_map),
                    User ! {ok, game_manager},
                    User ! {start_game, game_manager},
                    Fst ! {start_game, game_manager},
                    Room_pid = spawn(fun()-> game(Fst, User) end),
                    game_manager(New_Map, [Room_pid | Game_Rooms]);
                _ ->
                    %criar uma espera
                    User ! {ok, game_manager},
                    game_manager(Room_map#{Level => User}, Game_Rooms)
            end;
        {end_game, Game} ->
            game_manager(Room_map, Game_Rooms -- [Game])
    end.

game(FstPlayer, SndPlayer) ->
    ok.

lobby(Users) ->
    receive
        {enter, User} ->
            io:format("user entered ~p ~n", []),
            lobby([User | Users]);
        {line, Data} = Msg ->
            io:format("received  ~p ~n", [Data]),
            [User ! Msg || User <- Users],
            lobby(Users);
        {leave, User} ->
            io:format("user left ~n", []),
            lobby(Users -- [User])
    end.  

user(Sock, Room, Username) ->
    receive
        {line, Data} ->
            gen_tcp:send(Sock, "text:" ++ Data),
            user(Sock, Room, Username);
        {tcp, _, Data} ->
            case Data of
                "logout" -> 
                    Room ! {leave, self()},
                    gen_tcp:send(Sock, "logout:ok");
                "close:" ++ Data -> 
                    case login_manager:close_account(Username, Data) of
                        ok ->
                            Room ! {leave, self()},
                            gen_tcp:send(Sock, "close:ok");

                        wrong_password -> gen_tcp:send(Sock, "close:error_wrong_password"), user(Sock, Room, Username);
                        invalid -> gen_tcp:send(Sock, "close:error_invalid"), user(Sock, Room, Username)
                    end;
                "game:ready" ->
                    {ok, Level} = login_manager:check_level(Username),
                    game_manager ! {ready, Level, self()},
                    receive 
                        {ok, game_manager} -> gen_tcp:send(Sock, "game:ready"), user_ready(Sock,Room,Username)
                        %{error_already_ready, game_manager} -> gen_tcp:send(Sock, "game:error_already_ready"), user(Sock, Room, Username)
                    end;
                _ -> 
                    Room ! {line, Data},
                    user(Sock, Room, Username)
            end;
        {tcp_closed, _} ->
            Room ! {leave, self()};
        {tcp_error, _, _} ->
            Room ! {leave, self()}
    end.

unready(Username) ->
    {ok, Level} = login_manager:check_level(Username),
    game_manager ! {unready, Level, self()}.

user_ready(Sock, Room, Username) -> 
    receive
        {start_game, Game, game_manager} ->
            Room ! {leave, self()},
            gen_tcp:send(Sock, "game:start"),
            player(Sock, Game, Username);
        {tcp, _, Data} ->
            case Data of
                "logout" -> 
                    unready(Username),
                    Room ! {leave, self()},
                    gen_tcp:send(Sock, "logout:ok");
                "close:" ++ Data -> 
                    case login_manager:close_account(Username, Data) of
                        ok -> 
                            unready(Username),
                            gen_tcp:send(Sock, "close:ok"), 
                            Room ! {leave, self()};

                        wrong_password -> gen_tcp:send(Sock, "close:error_wrong_password"), user_ready(Sock, Room, Username);
                        invalid -> gen_tcp:send(Sock, "close:error_invalid"), user_ready(Sock, Room, Username)
                    end;
                "game:unready" -> 
                    unready(Username),
                    receive 
                        {ok, game_manager} -> gen_tcp:send(Sock, "game:unready"), user(Sock, Room, Username)
                        %{error_not_ready, game_manager} -> gen_tcp:send(Sock, "game:error_not_ready"), user_ready(Sock, Room, Username)
                    end;
                _ -> 
                    Room ! {line, Data},
                    user_ready(Sock, Room, Username)
            end;
        {tcp_closed, _} ->
            unready(Username),
            Room ! {leave, self()};
        {tcp_error, _, _} ->
            unready(Username),
            Room ! {leave, self()}
    end.

player(Sock, Game, Username) -> 
    receive
        {tcp, _, Data} -> ok;
        {tcp_closed, _} -> ok;
        {tcp_error, _, _} -> ok
    end.
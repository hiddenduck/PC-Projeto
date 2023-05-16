-module(space_server).
-export([start/1, stop/0]). % server:start(1234)
                            % nc localhost 1234
                            % netstat -p tcp -an | grep 1234

start(Port) -> register(?MODULE, spawn(fun() -> server(Port) end)).

stop() -> ?MODULE ! stop.

server(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [{packet, line}, {reuseaddr, true}]),
    register(lobby, spawn(fun()-> lobby([]) end)),
    register(game_manager, spawn(fun() -> game_manager(#{}, []) end)),
    spawn(fun() -> acceptor(LSock) end),
    receive stop -> ok end.

acceptor(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock) end),
    main_menu(Sock).

main_menu(Sock) ->
    receive
        {tcp, _, "create:" ++ Data} ->
            [Username, Password] = re:split(Data, "[:]"),
            case login_manager:create_account(Username, Password) of
                ok -> gen_tcp:send(Sock, "create:ok");
                user_exists -> gen_tcp:send(Sock, "create:error_user_exists")
            end,
            main_menu(Sock);
        {tcp, _, "login:" ++ Data} -> 
            [Username, Password] = re:split(Data, "[:]"),
            case login_manager:login(Username, Password) of
                ok ->
                    lobby ! {enter, "lobby", self()},
                    gen_tcp:send(Sock, "login:ok"),
                    user(Sock, Username);
                invalid_password ->
                    gen_tcp:send(Sock, "login:error_invalid_password"),
                    main_menu(Sock);
                _ ->
                    gen_tcp:send(Sock, "login:error_unknown_username"),
                    main_menu(Sock)
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
                {ok, {User, _}} ->
                    User ! {error_already_ready, game_manager},
                    game_manager(Room_map, Game_Rooms);
                {ok, {_, Game}} ->
                    %juntar-se ao jogo
                    User ! {ok, Game, game_manager},
                    Game ! {start, User, game_manager},
                    New_Map = maps:remove(Level, Room_map),
                    game_manager(New_Map, [Game | Game_Rooms]);
                _ ->
                    %criar uma espera
                    User ! {ok, game_manager},
                    Game = spawn(fun()-> game([User]) end),
                    game_manager(Room_map#{Level => Game}, Game_Rooms)
            end;
        {end_game, Game} ->
            game_manager(Room_map, Game_Rooms -- [Game])
    end.

game([FstPlayer]) ->
    receive 
        {abort, FstPlayer} -> ok;
        %Antes de começar o jogo é preciso verificar se ainda estão vivos os jogadores
        %Problemas de concorrência podem fazer com que o jogo comece mas um dos jogadores se desconecte antes de o saber
        {start, SndPlayer, game_manager} ->
            sync_up(FstPlayer, SndPlayer)
    end;

game([FstPlayer, SndPlayer]) -> 
    receive
        {abort, User} -> 
            %pontuar o outro jogador
            ok
    end.

sync_up(FstPlayer, SndPlayer) ->
    FstPlayer ! {start_game, self()},
    SndPlayer ! {start_game, self()},
    receive
        {ok, FstPlayer} -> 
            receive
                {ok, SndPlayer} -> game([FstPlayer, SndPlayer])
                after 6000 -> 
                    game_manager ! {end_game, self()},
                    SndPlayer ! {end_game, self()}
            end
        %1 minuto de espera para conexão parece justo, se não der é preciso avisar do fim do jogo
        after 6000 -> 
            game_manager ! {end_game, self()},
            SndPlayer ! {end_game, self()}
    end.

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

user(Sock, Username) ->
    receive
        {line, Data} ->
            gen_tcp:send(Sock, "text:" ++ Data),
            user(Sock, Username);
        {tcp, _, Data} ->
            case Data of
                "logout" -> 
                    lobby ! {leave, self()},
                    gen_tcp:send(Sock, "logout:ok");
                "close:" ++ Data -> 
                    case login_manager:close_account(Username, Data) of
                        ok ->
                            lobby ! {leave, self()},
                            gen_tcp:send(Sock, "close:ok");

                        wrong_password -> gen_tcp:send(Sock, "close:error_wrong_password"), user(Sock, Username);
                        invalid -> gen_tcp:send(Sock, "close:error_invalid"), user(Sock, Username)
                    end;
                "game:ready" ->
                    {ok, Level} = login_manager:check_level(Username),
                    game_manager ! {ready, Level, self()},
                    receive 
                        {ok, Game, game_manager} -> gen_tcp:send(Sock, "game:ready"), user_ready(Sock, Game, Username)
                        %{error_already_ready, game_manager} -> gen_tcp:send(Sock, "game:error_already_ready"), user(Sock, Room, Username)
                    end;
                _ -> 
                    lobby ! {line, Data},
                    user(Sock, Username)
            end;
        {tcp_closed, _} ->
            lobby ! {leave, self()};
        {tcp_error, _, _} ->
            lobby ! {leave, self()}
    end.

unready(Username, Game) ->
    Game ! {abort, self()},
    {ok, Level} = login_manager:check_level(Username),
    game_manager ! {unready, Level, self()}.

user_ready(Sock, Game, Username) -> 
    receive
        {start_game, Game, game_manager} ->
            lobby ! {leave, self()},
            gen_tcp:send(Sock, "game:start"),
            player(Sock, Game, Username);
        {tcp, _, Data} ->
            case Data of
                "logout" -> 
                    unready(Username, Game),
                    lobby ! {leave, self()},
                    gen_tcp:send(Sock, "logout:ok");
                "close:" ++ Data -> 
                    case login_manager:close_account(Username, Data) of
                        ok -> 
                            unready(Username, Game),
                            lobby ! {leave, self()},
                            gen_tcp:send(Sock, "close:ok");

                        wrong_password -> gen_tcp:send(Sock, "close:error_wrong_password"), user_ready(Sock, Game, Username);
                        invalid -> gen_tcp:send(Sock, "close:error_invalid"), user_ready(Sock, Game, Username)
                    end;
                "game:unready" -> 
                    unready(Username, Game),
                    receive 
                        {ok, game_manager} -> gen_tcp:send(Sock, "game:unready"), user(Sock,  Username)
                        %{error_not_ready, game_manager} -> gen_tcp:send(Sock, "game:error_not_ready"), user_ready(Sock, Room, Username)
                    end;
                _ -> 
                    lobby ! {line, Data},
                    user_ready(Sock, Game, Username)
            end;
        {tcp_closed, _} ->
            unready(Username, Game),
            lobby ! {leave, self()};
        {tcp_error, _, _} ->
            unready(Username, Game),
            lobby ! {leave, self()}
    end.

player(Sock, Game, Username) -> 
    receive
        {end_game, Game} -> user(Sock, Username);
        {tcp, _, Data} -> ok;
        {tcp_closed, _} -> ok;
        {tcp_error, _, _} -> ok
    end.
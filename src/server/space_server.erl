-module(space_server).
-export([start/1, stop/0]). % server:start(1234)
                            % nc localhost 1234
                            % netstat -p tcp -an | grep 1234

start(Port) -> register(?MODULE, spawn(fun() -> server(Port) end)).

stop() -> ?MODULE ! stop.

server(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [{packet, line}, {reuseaddr, true}]),
    register(room_manager, spawn(fun() -> room_manager(#{}) end)),
    Room = spawn(fun()-> room([]) end),
    spawn(fun() -> acceptor(LSock, Room) end),
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
                user_exists -> gen_tcp:send(Sock, "create:user_exists")
            end,
            main_menu(Sock, Room);
        {tcp, _, "login:" ++ Data} -> 
            [Username, Password] = re:split(Data, "[:]"),
            case login_manager:login(Username, Password) of
                ok -> 
                    Room ! {enter, "lobby", self()},
                    gen_tcp:send(Sock, "login:ok"),
                    user(Sock, Room);
                invalid_password ->
                    gen_tcp:send(Sock, "login:invalid_password"),
                    main_menu(Sock, Room);
                _ ->
                    gen_tcp:send(Sock, "login:unknown_username"),
                    main_menu(Sock, Room)
            end;
        {tcp_error, _, _} -> ok;
        {tcp_closed, _, _} -> ok;
        _ -> gen_tcp:send(Sock, "error:unknown_command")
    end.

room_manager(Room_map) ->
    receive
        {get_room, Room_name, User} -> 
            case maps:find(Room_name, Room_map) of
                {ok, Room_pid} ->
                    User ! {room, Room_pid, self()},
                    room_manager(Room_map);
                _ ->
                    Room_pid = spawn(fun()-> room([]) end),
                    User ! {room, Room_pid, self()},
                    room_manager(Room_map#{Room_name => Room_pid})
            end
    end.
    

room(Pids) ->
    receive
        {enter, Room_name, Pid} ->
            io:format("user entered ~p ~n", [Room_name]),
            room([Pid | Pids]);
        {line, Data} = Msg ->
            io:format("received  ~p ~n", [Data]),
            [Pid ! Msg || Pid <- Pids],
            room(Pids);
        {leave, Pid} ->
            io:format("user left ~n", []),
            room(Pids -- [Pid])
    end.

get_room(Name) ->
    room_manager ! {get_room, Name, self()},
    receive {room, R} -> R end.    

user(Sock, Room) ->
    receive
        {line, Data} ->
            gen_tcp:send(Sock, Data),
            user(Sock, Room);
        {tcp, _, Data} ->
            case Data of
                "/room " ++ Rest ->
                    New_room_name = Rest -- "\n",
                    NewRoom = get_room(New_room_name),
                    NewRoom ! {enter, New_room_name, self()},
                    Room ! {leave, self()},
                    user(Sock, NewRoom);
                _ -> 
                    Room ! {line, Data},
                    user(Sock, Room)
            end;
        {tcp_closed, _} ->
            Room ! {leave, self()};
        {tcp_error, _, _} ->
            Room ! {leave, self()}
    end.
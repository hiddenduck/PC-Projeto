-module(server).
-export([start/1, stop/0]). % server:start(1234)
                            % nc localhost 1234
                            % netstat -p tcp -an | grep 1234

start(Port) -> register(?MODULE, spawn(fun() -> server(Port) end)).

stop() -> ?MODULE ! stop.

server(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [{packet, line}, {reuseaddr, true}]),
    RM= spawn(fun() -> room_manager(#{}) end),
    Room = spawn(fun()-> room([]) end),
    spawn(fun() -> acceptor(LSock, Room, RM) end),
    receive stop -> ok end.

acceptor(LSock, Room, RM) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock, Room, RM) end),
    Room ! {enter, "lobby", self()},
    user(Sock, Room, RM).

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

get_room(Name, RM) ->
    RM ! {get_room, Name, self()},
    receive {room, R, RM} -> R end.    

user(Sock, Room, RM) ->
    receive
        {line, Data} ->
            gen_tcp:send(Sock, Data),
            user(Sock, Room, RM);
        {tcp, _, Data} ->
            case Data of
                "/room " ++ Rest ->
                    New_room_name = Rest -- "\n",
                    NewRoom = get_room(New_room_name, RM),
                    NewRoom ! {enter, New_room_name, self()},
                    user(Sock, NewRoom, RM);
                _ -> 
                    Room ! {line, Data},
                    user(Sock, Room, RM)
            end;
        {tcp_closed, _} ->
            Room ! {leave, self()};
        {tcp_error, _, _} ->
            Room ! {leave, self()}
    end.
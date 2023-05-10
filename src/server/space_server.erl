-module(space_server).

-export([start/1, stop/0]).

start(Port) -> register(?MODULE, spawn(fun() -> server(Port) end)).

stop() -> ?MODULE ! stop.

server(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [{packet, line}, {reuseaddr, true}]),
    Room = spawn(fun() -> room([]) end),
    RM = spawn(fun() -> rm(#{"Lobby" => Room}) end),
    spawn(fun() -> acceptor(LSock, Room, RM) end),
    receive stop -> ok end.

acceptor(LSock, Room, RM) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock, Room, RM) end),
    Room ! {enter, self()},
    user(Sock, Room, RM).

room(Pids) ->
    receive
        {enter, Pid} ->
            io:format("user entered~n", []),
            room([Pid | Pids]);
        {line, _} = Msg ->
            io:format("received ~p~n", [Msg]),
            [Pid ! Msg || Pid <- Pids], room(Pids);
        {leave, Pid} ->
            io:format("user left~n", []),
            room([Pids -- [Pid]])
    end.

user(Sock, Room, RM) ->
    receive
        {line, Data} ->
            gen_tcp:send(Sock, Data),
            user(Sock, Room, RM);
        {tcp, _, Data} ->
            case Data of
                "/room " ++ Rest -> 
                    RoomName = Rest -- "\n",
                    io:format("entered room ~n", []),
                    RM ! {get_room, RoomName, self()},
                    NewRoom = receive {room, R} -> R end;
                _ ->
                    Room ! {line, Data},
                    NewRoom = Room
            end,
            user(Sock, NewRoom, RM);
        {tcp_closed, _} ->
            Room ! {leave, self()};
        {tcp_error, _, _} ->
            Room ! {leave, self()}
    end.

rm(Rooms) ->
    receive
        {get_room, Name, From} ->
            case maps:find(Name, Rooms) of
                {ok, Room} ->
                    NewRooms = Rooms;
                error ->
                    Room = spawn(fun() -> room([From]) end),
                    NewRooms = maps:put(Name, Room, Rooms)
            end
    end,
    From ! {room, Room},
    rm(NewRooms).

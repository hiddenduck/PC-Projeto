-module(space_server).

-export([start/1, stop/0]).

start(Port) -> register(?MODULE, spawn(fun() -> server(Port) end)).

stop() -> ?MODULE ! stop.
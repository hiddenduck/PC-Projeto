-module(test).
-export([main/0]).

main() ->
    Map = #{a=>[b]},
    %file:write_file("test", erlang:term_to_binary(Map)),
    {ok, Binary} = file:read_file("test"),
    erlang:binary_to_term(Binary).
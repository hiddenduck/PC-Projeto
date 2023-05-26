-module(lab).

-export([test/0, plus/2]).

plus(A, B) ->
    A + B.





test() ->
    List = [1,1,1,1,1,1,1,1], % 8
    lists:foldl(fun plus/2, 0, List).

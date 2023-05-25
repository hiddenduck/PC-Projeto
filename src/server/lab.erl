-module(lab).

-export([check_player_colision/4]).

colision(X1, Y1, X2, Y2, Radius) ->
    (X1 - X2) * (X1 - X2) + (Y1 - Y2) * (Y1 - Y2) =< Radius*Radius.


check_player_colision({X1, Y1}, {X2, Y2}, Alfa1, Alfa2) ->
    Radius = 1,%TODO tune
    GuardCol = colision(X1, Y1, X2, Y2, Radius) and (abs(Alfa1 - Alfa2) < math:pi()/2),
    io:format("~w \n", [GuardCol]),
    if 
        GuardCol ->
           GuardPoint = (X2 - X1) * math:cos(Alfa2) + (Y2 - Y1) * math:sin(Alfa2) > 0,
           if GuardPoint ->
                io:format("Hit1\n"),
                hit1;
            true ->
                io:format("Hit2\n"),
                hit2
           end;
        true ->
            io:format("NoHit\n"),
            nohit
    end.

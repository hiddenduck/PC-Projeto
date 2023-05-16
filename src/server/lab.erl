-module(lab).

-export([update_deltas/3]).

colision(X1, Y1, X2, Y2, Radius) ->
    (X1 - X2) * (X1 - X2) + (Y1 - Y2) * (Y1 - Y2) =< Radius*Radius.

check_color({X, Y, C}, Sim) ->
    Delta = 1,%TODO check proper delta
    case C of
        blue ->
            {X, Y, C};
        green ->
            {X, Y, C};
        red ->
            {X, Y, C}
    end.
 
update_deltas({X1, Y1}, Powerups, Sim) ->
    Radius = 1, %TODO tune
    HitList = lists:filter(fun({X, Y, _}) -> colision(X1, Y1, X, Y, Radius) end, Powerups),
    lists:map(fun(X) -> check_color(X, Sim) end, HitList).

check_player_colision({X1, Y1}, {X2, Y2}, Alfa1, Alfa2) ->
    Radius = 1,%TODO tune
    GuardCol = colision(X1, Y1, X2, Y2, Radius) and abs(Alfa1) < Alfa2 - math:pi()/2,
    if
        GuardCol -> 
            GuardPoint = (X2-X1)*math:cos(Alfa2) + (Y2-Y1)*math:sin(Alfa2) > 0,
            if 
                GuardPoint -> hit1;
                true -> hit2
            end;
        true -> nohit
    end.

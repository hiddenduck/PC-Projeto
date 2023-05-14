-module(lab).

-export([update_speeds/4]).

col(X1, Y1, X2, Y2) ->
    Radius = 2,%TODO define nice radius
    (X1 - X2)*(X1 - X2) + (Y1 - Y2)*(Y1 - Y2) =< Radius*Radius.

check_color({_,_,C}, {A, AV}) ->
    Delta = 1,%TODO check proper delta
    BA = 1,   %TODO check it
    BAV = 1,  %TODO check it 
                        case C of
                            blue ->
                                {A, AV+Delta};
                            green ->
                                {A+Delta, AV};
                            red ->
                                {BA,BAV}
                        end.
    

update_speeds(A, AV, Pos, Powerups) ->
    {X1, Y1} = Pos,
    HitList = lists:filter(fun({X,Y,_}) -> col(X1,Y1,X,Y) end, Powerups),
    lists:foldl(fun(H, Acc) -> check_color(H, Acc) end, {A, AV}, HitList).

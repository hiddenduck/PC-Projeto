-module(simulation).

-export([start_game/0]).

%Game state -> Player 1:
%              x1, y1 <- position
%              vx1, vy1 <- x and y componets of speed
%              angle <- signifies direction

% Note- o cos e o sin recebem radianos %
start_game() ->
    P1 = {{1, 0}, {0,0}, 0, math:pi()},
    P2 = {{-1, 0}, {0, 0}, 0, math:pi()},
    State = spawn(fun() -> state({P1, P2}) end),
    Ticker = spawn(fun() -> ticker(State) end),
    Simulator = spawn(fun() -> simulator({P1, P2}) end).

sleep(T) ->
    receive
    after T ->
        true
end.

ticker(State) ->
    sleep(100),
    State ! {give_state, self()},
    receive
        {GameState, State} -> simulator(GameState),
    ticker(State)
end.

state({{X, Y}, {Vx, Vy}, Alfa}) -> 
    receive
        {give_state, From} -> 
            From ! {{{X, Y}, {Vx, Vy}, Alfa}, self()};
        {increase_speed_placeholder, Delta_x, Delta_y} ->
            state({{X, Y}, {Vx+Delta_x, Vy+Delta_y}, Alfa});
        {change_angle_placeholder, Delta} ->
            state({{X, Y}, {Vx, Vy}, Alfa+Delta})
    end.

simulator({P1, P2}) ->
    {{X1, Y1}, {Vx1, Vy1}, Alfa1} = P1,
    {{X2, Y2}, {Vx2, Vy2}, Alfa2} = P2,

    P1_ = {{X1+Vx1, Y1+Vy1}, {Vx1, Vy1}, Alfa1},
    P2_ = {{X2+Vx2, Y2+Vy2}, {Vx2, Vy2}, Alfa2},
    
    {P1_, P2_}.

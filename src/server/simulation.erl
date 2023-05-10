-module(simulation).

-export([start_game/0]).

%Game state -> Player 1:
%              x1, y1 <- position
%              vx1, vy1 <- x and y componets of speed
%              angle <- signifies direction

% Note- o cos e o sin recebem radianos %
start_game() ->
    P1 = {{0, 0}, math:pi()},
    P2 = {{0, 0}, math:pi()},
    Player1_sim = spawn(fun() -> simulator(P1) end),
    Player2_sim = spawn(fun() -> simulator(P2) end),
    spawn(fun() -> ticker({1,0}, {-1, 0}, Player1_sim, Player2_sim) end),
    {Player1_sim, Player2_sim}.

sleep(T) ->
    receive
    after T ->
        true
end.

ticker(Pos1, Pos2, Player1_sim, Player2_sim) ->
    
    sleep(10000),

    Player1_sim ! {return_state, self()},
    receive
        PlayerState1 ->
            {X1_, Y1_} = mv(Pos1, PlayerState1),
            io:format("State player1 ~p~n", [{{X1_, Y1_}, PlayerState1}])
    end,
    Player2_sim ! {return_state, self()},
    receive
        PlayerState2 ->
            {X2_, Y2_} = mv(Pos2, PlayerState2),
            io:format("State player2 ~p~n", [{{X2_, Y2_}, PlayerState2}])
    end,
    ticker({X1_, Y1_}, {X2_, Y2_}, Player1_sim, Player2_sim).

    

simulator(PlayerState) ->
    receive
        {speed_up, Delta_x, Delta_y} ->
            {{Vx, Vy}, Alfa} = PlayerState,
            NewPlayerState = {{Vx+Delta_x, Vy+Delta_y}, Alfa},
            simulator(NewPlayerState);
        {change_direction, Delta} ->
            {{Vx, Vy}, Alfa} = PlayerState,
            NewPlayerState = {{Vx, Vy}, Alfa+Delta},
            simulator(NewPlayerState);
        {return_state, From} ->
            From ! PlayerState,
            simulator(PlayerState)
    end.

mv(Pos, State) ->
    {X, Y} = Pos,
    {{Vx, Vy}, _} = State,
    {X+Vx, Y+Vy}.


-module(simulation).

-export([start_game/0]).

%start_game spawns a simulator for each player
%and spawns a ticker to start a game
start_game() ->
    P1 = {{0, 0}, math:pi()},
    P2 = {{0, 0}, math:pi()},
    Player1_sim = spawn(fun() -> simulator(P1, true) end),
    Player2_sim = spawn(fun() -> simulator(P2, true) end),
    spawn(fun() -> ticker({1,0}, {-1, 0}, Player1_sim, Player2_sim) end),
    {Player1_sim, Player2_sim}.

%sleep function yoinked from stor 
%may be better function in erlang
sleep(T) ->
    receive
    after T ->
        true
end.

%ticker controls the game ticks and the current position of the players 
%this position is updated every game tick by quering simulator about the speed and 
%direction of players
%it would in future be responsible for checking hits 
%and manging the powerups
%game ticks are defined by number in function sleep([ms])
%curently receives PlayerState raw and no rubber bun in future should make shure only simulator
%is allowed to send messages to it
%Player 1 may be barred from puting inputs but 2 no
ticker(Pos1, Pos2, Player1_sim, Player2_sim) ->
    
    sleep(5000),

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

    

%simulator saves and updates the current values for speed and the angle
%updates are done through messages to the process
%saves values of speed in separate coordenates to save work on mv()
%maybe better to change this and minimize bytes going in between processes
%NOTE Alfa is in radians because erlang is a chad language
%i decided to check for changes first and only after for return 
%is this better? i dont know
%TODO change return state priority to Max
simulator(PlayerState, Flag) ->
    receive
        {speed_up, Delta} when Flag ->
            {{Vx, Vy}, Alfa} = PlayerState,
            NewPlayerState = {{Vx+Delta*math:cos(Alfa), Vy+Delta*math:sin(Alfa)}, Alfa},
            simulator(NewPlayerState, false);
        {change_direction, Delta} when Flag->
            {{Vx, Vy}, Alfa} = PlayerState,
            NewPlayerState = {{Vx, Vy}, Alfa+Delta},
            simulator(NewPlayerState, false);
        {return_state, From} ->
            From ! PlayerState,
            simulator(PlayerState, true);
        _ -> 
            simulator(PlayerState, Flag)
    end.

%mv simply calculates the new positions for a player after one game tick
mv(Pos, State) ->
    {X, Y} = Pos,
    {{Vx, Vy}, _} = State,
    {X+Vx, Y+Vy}.



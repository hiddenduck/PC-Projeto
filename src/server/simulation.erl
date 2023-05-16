-module(simulation).

-export([start_game/0]).

%start_game spawns a simulator for each player
%and spawns a ticker to start a game
start_game() ->
    P1 = {{0, 0}, math:pi(), {1,1}},
    P2 = {{0, 0}, math:pi(), {1,1}},
    Player1_sim = spawn(fun() -> simulator(P1, 0) end),
    Player2_sim = spawn(fun() -> simulator(P2, 0) end),
    Game = spawn(fun() -> game({{1, 0}, {-1, 0}}, {Player1_sim, Player2_sim}, [], {0, 0}) end),
    spawn(fun() -> ticker(Game) end),
    {Player1_sim, Player2_sim}.

%sleep function yoinked from stor
%may be better function in erlang
sleep(T) ->
    receive after T ->
        true
    end.

ticker(Game) ->
    sleep(1000),
    Game ! tick.

new_pos({X, Y}, Sim) ->
    Sim ! {return_state, self()},
    receive
        {{Vx, Vy}, Alfa, _} ->
            {X + Vx, Y + Vy, Alfa}
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
game(Pos, Player_sims, Powerups, {P1, P2}) ->
    receive
        tick ->
            {Player1_sim, Player2_sim} = Player_sims,
            {Pos1, Pos2} = Pos,

            {X1_, Y1_, Alfa1} = new_pos(Pos1, Player1_sim),
            {X2_, Y2_, Alfa2} = new_pos(Pos2, Player2_sim),

            Boundx = 1,%TODO tune
            Boundy = 1,%TODO tune

            if % check players in bounds
               X1_ > Boundx, X1_ < Boundx; Y1_ > Boundy, Y1_ < Boundy ->
                   Player1_sim ! reset_param,
                   game({{0, 0}, {0, 0}}, Player_sims, Powerups, {P1, P2 + 1});
               X2_ > Boundx, X2_ < Boundx; Y2_ > Boundy, Y2_ < Boundy ->
                   Player2_sim ! reset_param,
                   game({{0, 0}, {0, 0}}, Player_sims, Powerups, {P1 + 1, P2});
               true -> % else check_player_colision
                   case check_player_colision(Pos1, Pos2, Alfa1, Alfa2) of
                       hit1 ->
                           Player1_sim ! reset_param,
                           game({{0, 0}, {0, 0}},
                                Player_sims,
                                Powerups,
                                {P1 + 1, P2}); % TODO inicializar os players a um valor random
                       hit2 ->
                           Player2_sim ! reset_param,
                           game({{0, 0}, {0, 0}},
                                Player_sims,
                                Powerups,
                                {P1, P2 + 1}); % TODO inicializar os players a um valor random
                       nohit ->
                           game({{X1_, Y1_},
                                 {X2_, Y2_}}, % if no hit call ticker after update_deltas
                                {Player1_sim, Player2_sim},
                                Powerups
                                -- update_deltas({X1_, Y1_}, Powerups, Player1_sim)
                                -- update_deltas({X2_, Y2_}, Powerups, Player2_sim),
                                {P1, P2})
                   end
            end
    end.

%simulator saves and updates the current values for speed and the angle
%updates are done through messages to the process
%saves values of speed in separate coordenates to save work on mv()
%maybe better to change this and minimize bytes going in between processes
%NOTE Alfa is in radians because erlang is a chad language
%i decided to check for changes first and only after for return
%is this better? i dont know
%TODO change return state priority to Max
%
%Tricky binay math(the prgramer way)
%00 -> flag-direction, flag-speed
%checks if flag bit is set to one if so dont do operation
simulator(PlayerState, Flag) ->
    {{Vx, Vy}, Alfa, {Accel, AngVel}} = PlayerState,
    receive
        speed_up when Flag band 1 == 0 ->
            NewPlayerState =
                {{Vx + Accel * math:cos(Alfa), Vy + Accel * math:sin(Alfa)}, Alfa, {Accel, AngVel}},
            simulator(NewPlayerState, Flag bor 1);
        change_direction when Flag band 2 == 0 ->
            NewPlayerState = {{Vx, Vy}, Alfa + AngVel, {Accel, AngVel}},
            simulator(NewPlayerState, Flag bor 2);
        {change_accel, Delta} ->
            simulator({{Vx, Vy}, Alfa, {Accel + Delta, AngVel}}, Flag);
        {change_angvel, Delta} ->
            simulator({{Vx, Vy}, Alfa, {Accel, AngVel + Delta}}, Flag);
        reset_param ->
            simulator({{Vx, Vy}, Alfa, {1, 1}}, Flag); %TODO define starting values
        {return_state, From} ->
            From ! PlayerState,
            simulator(PlayerState, 0);
        _ ->
            simulator(PlayerState, Flag)
    end.

colision(X1, Y1, X2, Y2, Radius) ->
    (X1 - X2) * (X1 - X2) + (Y1 - Y2) * (Y1 - Y2) =< Radius * Radius.

check_color({X, Y, C}, Sim) ->
    Delta = 1,%TODO check proper delta
    case C of
        blue ->
            Sim ! {change_angvel, Delta},
            {X, Y, C};
        green ->
            Sim ! {change_accel, Delta},
            {X, Y, C};
        red ->
            Sim ! reset_param,
            {X, Y, C}
    end.

update_deltas({X1, Y1}, Powerups, Sim) ->
    Radius = 1, %TODO tune
    HitList = lists:filter(fun({X, Y, _}) -> colision(X1, Y1, X, Y, Radius) end, Powerups),
    lists:map(fun(X) -> check_color(X, Sim) end, HitList).

check_player_colision({X1, Y1}, {X2, Y2}, Alfa1, Alfa2) ->
    Radius = 1,%TODO tune
    GuardCol = colision(X1, Y1, X2, Y2, Radius) and abs(Alfa1) < Alfa2 - math:pi() / 2,
    if GuardCol ->
           GuardPoint = (X2 - X1) * math:cos(Alfa2) + (Y2 - Y1) * math:sin(Alfa2) > 0,
           if GuardPoint ->
                  hit1;
              true ->
                  hit2
           end;
       true ->
           nohit
    end.

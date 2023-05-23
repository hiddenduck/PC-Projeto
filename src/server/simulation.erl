-module(simulation).

-export([start_game/1, change_speed/1, change_angle/2]).

%start_game spawns a simulator for each player
%and spawns a ticker to start a game
start_game(Game) ->
    P1 = {{0, 0}, math:pi(), {1,1}},
    P2 = {{0, 0}, math:pi(), {1,1}},
    Player1_sim = spawn(fun() -> simulator(P1, 0) end),
    Player2_sim = spawn(fun() -> simulator(P2, 0) end),
    GameSim = spawn(fun() -> Self = self(),
        game(
        Game,
        {{1, 0}, {-1, 0}},
        {Player1_sim, Player2_sim},
        [],
        {0, 0},
        spawn(fun() -> ticker(Self) end)
    ) end),
    spawn(fun() -> timer(GameSim) end),
    {Player1_sim, Player2_sim}.

change_speed(PlayerSim) ->
    PlayerSim ! speed_up.

change_angle(PlayerSim, Dir) ->
    PlayerSim ! {change_angle, Dir}.

%sleep function yoinked from stor
%may be better function in erlang
sleep(T) ->
    receive after T ->
        true
    end.

ticker(GameSim) ->
    sleep(1000),
    receive
        {reset, GameSim} -> 
            sleep(2000) %TODO tune
    after
        0 ->
            GameSim ! tick
    end.

new_pos({X, Y}, Sim) ->
    Sim ! {return_state, self()},
    receive
        {{Vx, Vy}, Alfa, _} ->
            {X + Vx, Y + Vy, Alfa}
    end.

timer(GameSim) ->
    sleep(60000),
    GameSim ! timeout.

%Game controls the general game state such as 
%player position Powerups and Points
%it updates player position by quering simulator
%and calculating position
%finally it checks if a player is out of bounds
%colison of players and updates deltas from Powerups
game(Controler, Pos, Player_sims, Powerups, {P1, P2}, Ticker) ->
    receive
        tick ->
            receive
                timeout when P1 /= P2 -> 
                    {Player1_sim, Player2_sim} = Player_sims,
                    Winner = 
                    if
                        P1 > P2 -> Player1_sim;
                        P1 < P2 -> Player2_sim
                    end,
                    space_server:abort_game(Controler, Winner),
                    [PlayerSim ! stop || PlayerSim <- Player_sims],
                    ok
            after
                0 ->

                    Base1 = {0.25,0.5,0},
                    Base2 = {0.75,0.5,0},
                    
                    {Player1_sim, Player2_sim} = Player_sims,
                    {Pos1, Pos2} = Pos,

                    {X1_, Y1_, Alfa1} = new_pos(Pos1, Player1_sim),
                    {X2_, Y2_, Alfa2} = new_pos(Pos2, Player2_sim),

                    space_server:positions({X1_, Y1_, Alfa1}, {X2_, Y2_, Alfa2}, Controler, self()),

                    Boundx = 1,%TODO tune
                    Boundy = 1,%TODO tune

                    if % check players in bounds
                       X1_ > Boundx, X1_ < Boundx; Y1_ > Boundy, Y1_ < Boundy ->
                           Player1_sim ! reset_param,
                           Player2_sim ! reset_param,
                           
                           space_server:positions(Base1, Base2, Controler, self()),
                           space_server:score(P1, P2+1, Controler, self()),
                           
                           game(Controler, {Base1, Base2}, Player_sims, Powerups, {P1, P2 + 1}, Ticker);
                       X2_ > Boundx, X2_ < Boundx; Y2_ > Boundy, Y2_ < Boundy ->
                           Player1_sim ! reset_param,
                           Player2_sim ! reset_param,
                           
                           space_server:positions(Base1, Base2, Controler, self()),
                           space_server:score(P1 + 1, P2, Controler, self()),
                           
                           game(Controler, {Base1, Base2}, Player_sims, Powerups, {P1 + 1, P2}, Ticker);
                       true -> % else check_player_colision
                           case check_player_colision(Pos1, Pos2, Alfa1, Alfa2) of
                               hit1 ->
                                   Player1_sim ! reset_param,
                                   Player2_sim ! reset_param,
                           
                                   space_server:positions(Base1, Base2, Controler, self()),
                                   space_server:score(P1 + 1, P2, Controler, self()),

                                   game(Controler, {Base1, Base2}, Player_sims, Powerups, {P1 + 1, P2}, Ticker);
                               hit2 ->
                                   Player2_sim ! reset_param,
                           
                                   space_server:positions(Base1, Base2, Controler, self()),
                                   space_server:score(P1, P2 + 1, Controler, self()),

                                   game(Controler, {Base1, Base2}, Player_sims, Powerups, {P1, P2 + 1}, Ticker);
                               nohit ->
                                   game(Controler, {{X1_, Y1_},
                                         {X2_, Y2_}}, % if no hit call ticker after update_deltas
                                        {Player1_sim, Player2_sim},
                                        Powerups
                                        -- update_deltas({X1_, Y1_}, Powerups, Player1_sim)
                                        -- update_deltas({X2_, Y2_}, Powerups, Player2_sim),
                                        {P1, P2}, Ticker)
                           end
                    end
            end
    end.

%simulator saves and updates the current values for speed and the angle
%updates are done through messages to the process
%saves values of speed in separate coordenates to save work on mv()
%NOTE Alfa is in radians because erlang is a chad language
%only alows for one change per tick
%use a binary flag least significant bit -> speed, most significant bit -> angle
%also saves acceleration and angular velocity, these are also changes trough messages
%in future may be better to make speed and dir have higher priority
simulator(PlayerState, Flag) ->
    {{Vx, Vy}, Alfa, {Accel, AngVel}} = PlayerState,
    receive
        stop -> 
            ok;
        speed_up when Flag band 1 == 0 ->
            NewPlayerState =
                {{Vx + Accel * math:cos(Alfa), Vy + Accel * math:sin(Alfa)}, Alfa, {Accel, AngVel}},
            simulator(NewPlayerState, Flag bor 1);
        {change_direction, Dir} when Flag band 2 == 0 ->
            NewPlayerState = {{Vx, Vy}, math:fmod(Alfa + Dir * AngVel, 2*math:pi()), {Accel, AngVel}},
            simulator(NewPlayerState, Flag bor 2)
    after
        0 ->
            receive
                {change_accel, Delta} ->
                    simulator({{Vx, Vy}, Alfa, {Accel + Delta, AngVel}}, Flag);
                {change_angvel, Delta} ->
                    simulator({{Vx, Vy}, Alfa, {Accel, AngVel + Delta}}, Flag);
                reset_param ->
                    simulator({{0, 0}, 0, {1, 1}}, Flag); %TODO define starting values
                {return_state, From} ->
                    From ! PlayerState,
                    simulator(PlayerState, 0);
                _ ->
                    simulator(PlayerState, Flag)
                    end
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
    GuardCol = colision(X1, Y1, X2, Y2, Radius), abs(Alfa1) < (Alfa2 - math:pi() / 2),
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

-module(simulation).

-export([start_game/1, change_speed/1, change_angle/2]).

-define(DELTA_ANGLE, 0.125).
-define(DELTA_ACC, 0.125).
-define(RADIUS, 15).
-define(GAME_DURATION, 60000).
-define(POWER_CHANCE, 150).
-define(BASE_ACCEL, 0.125).
-define(BASE_ANGVEL, 0.125).
-define(TICK_RATE, 100).
-define(RESET_TIME, 5000).
-define(BOX_LIMIT, 10).
-define(DECAY_RATE, 0.0001).

%start_game spawns a simulator for each player
%and spawns a ticker to start a game
start_game(Controller) ->
    P1 = {{0, 0}, 0, {0.125,0.125}},
    P2 = {{0, 0}, math:pi(), {0.125,0.125}},
    Player1_sim = spawn(fun() -> simulator(P1, 0) end),
    Player2_sim = spawn(fun() -> simulator(P2, 0) end),
    GameSim = spawn(fun() -> Self = self(),
        game(
        Controller,                       % Controller
        {{100, 400}, {400, 400}},         % positions
        {Player1_sim, Player2_sim},       % sims
        [],                               % powerups
        {0, 0},                           % points
        spawn(fun() -> timer(Self) end),  % time
        spawn(fun() -> ticker(Self) end), % ticker
        false                             % golden point
    ) end),
    spawn(fun() -> timer(GameSim) end),
    {Player1_sim, Player2_sim, GameSim}.

change_speed(PlayerSim) ->
    PlayerSim ! speed_up.

change_angle(PlayerSim, Dir) ->
    PlayerSim ! {change_direction, Dir}.

%sleep function yoinked from stor
%may be better function in erlang
sleep(T) ->
    receive after T ->
        true
    end.

ticker(GameSim) ->
    receive
        stop ->
            ok
    after
        ?TICK_RATE ->
            GameSim ! tick,
            ticker(GameSim)
    end.

new_pos({X, Y}, Sim) ->
    Sim ! {return_state, self()},
    receive
        {State, Sim}->
            {{Vx, Vy}, Alfa, _} = State,
            %io:format("~p\n", [State]),
            {X + Vx, Y + Vy, Alfa}
    end.

timer(GameSim) ->
    receive
        stop ->
            ok
    after
        ?GAME_DURATION ->
            io:format("times up"),
            GameSim ! timeout
    end.

kill_procs(Procs) ->
    [Proc ! stop || Proc <- Procs].

%Game controls the general game state such as 
%player position Powerups and Points
%it updates player position by quering simulator
%and calculating position
%finally it checks if a player is out of bounds
%colison of players and updates deltas from Powerups
game(Controller, Pos, Player_sims, OldPowerups, {P1, P2}, Timer, Ticker, Golden) ->
    receive
        _ when Golden, P1 /= P2 -> 
            %io:format("tick tock\n"),
            {Player1_sim, Player2_sim} = Player_sims,
            Loser = 
            if
                P1 > P2 -> p2;
                P1 < P2 -> p1
            end,
            space_server:abort_game(Controller, Loser),
            kill_procs([Player1_sim, Player2_sim, Ticker, Timer]),
            ok;
        {stop, Controller} ->
            {Player1_sim, Player2_sim} = Player_sims,
            kill_procs([Player1_sim, Player2_sim, Ticker, Timer]),
            ok;
        timeout when P1 == P2 ->
            space_server:golden_point(Controller),
            game(Controller, Pos, Player_sims, OldPowerups, {P1, P2}, Timer, Ticker, true);
        timeout when P1 /= P2 -> 
            %io:format("tick tock\n"),
            {Player1_sim, Player2_sim} = Player_sims,
            Loser = 
            if
                P1 > P2 -> p2;
                P1 < P2 -> p1
            end,
            space_server:abort_game(Controller, Loser),
            kill_procs([Player1_sim, Player2_sim, Ticker, Timer]),
            ok;

        tick ->

            %Base1 = {100,400},
            %Base2 = {400,400},

            {Player1_sim, Player2_sim} = Player_sims,
            {Pos1, Pos2} = Pos,

            {X1_, Y1_, Alfa1} = new_pos(Pos1, Player1_sim),
            {X2_, Y2_, Alfa2} = new_pos(Pos2, Player2_sim),

            Player1_sim ! decay,
            Player2_sim ! decay,

            space_server:positions({X1_, Y1_, Alfa1}, {X2_, Y2_, Alfa2}, Controller, self()),

            Boundx_min = 0,%TODO tune
            Boundx_max = 700,%TODO tune
            Boundy_min = 0,%TODO tune
            Boundy_max = 700,%TODO tune
            
            %Isto tudo pode estar dentro de uma função que devolve Powerups
            Pow = rand:uniform(?POWER_CHANCE),
            if 
                Pow == 1, length(OldPowerups) < ?BOX_LIMIT ->
                    C = case rand:uniform(3) of
                        1 -> blue;
                        2 -> green;
                        3 -> red
                    end,
                    {X, Y} = get_random_pos([{X1_, Y1_, Alfa1} , {X2_, Y2_, Alfa2} | OldPowerups], {Boundx_max, Boundy_max}),
                    AddPowerups = [{X,Y,C} | OldPowerups],
                    Add = [{X,Y,C}],
                    io:format("novo powerup\n");
                    true -> AddPowerups = OldPowerups, Add = []
            end,
            Remove = update_deltas({X1_, Y1_}, AddPowerups, Player1_sim) ++ update_deltas({X2_, Y2_}, AddPowerups, Player2_sim),
            if 
                Add =:= [], Remove =:= [] -> ok;
                true -> space_server:boxes(Add, Remove, Controller, self())
            end,
            Powerups = AddPowerups -- Remove,
            

            if % check players in bounds
                X1_ < Boundx_min + ?RADIUS; X1_ > Boundx_max - ?RADIUS; Y1_ < Boundy_min + ?RADIUS; Y1_ > Boundy_max - ?RADIUS ->
                    
                    space_server:abort_game(Controller, p1),
                    kill_procs([Player1_sim, Player2_sim, Ticker, Timer]),
                    ok;

                X2_ < Boundx_min + ?RADIUS; X2_ > Boundx_max - ?RADIUS; Y2_ < Boundy_min + ?RADIUS; Y2_ > Boundy_max - ?RADIUS ->
                    
                    space_server:abort_game(Controller, p2),
                    kill_procs([Player1_sim, Player2_sim, Ticker, Timer]),
                    ok;

                true -> % else check_player_colision
                    case check_player_colision(Pos1, Pos2, Alfa1, Alfa2) of
                        hit1 ->
                            Player1_sim ! reset_param,
                            Player2_sim ! reset_state,
                            {NewPosX, NewPosY} = get_random_pos([{X1_, Y1_, Alfa1} | Powerups], {Boundx_max, Boundy_max}),
                            space_server:positions({X1_, Y1_, Alfa1}, {NewPosX, NewPosY, 0}, Controller, self()),
                            space_server:score(P1 + 1, P2, Controller, self()),

                            Ticker ! reset,

                            game(Controller, {{X1_, Y1_}, {NewPosX, NewPosY}}, Player_sims, Powerups, {P1 + 1, P2}, Timer, Ticker, Golden);
                        hit2 ->
                            Player1_sim ! reset_state,
                            Player2_sim ! reset_param,
                            {NewPosX, NewPosY} = get_random_pos([{X2_, Y2_, Alfa2} | Powerups], {Boundx_max, Boundy_max}),
                    
                            space_server:positions({NewPosX, NewPosY, 0}, {X2_, Y2_, Alfa2}, Controller, self()),
                            space_server:score(P1, P2 + 1, Controller, self()),

                            Ticker ! reset,

                            game(Controller, {{NewPosX, NewPosY}, {X2_, Y2_}}, Player_sims, Powerups, {P1, P2 + 1}, Timer, Ticker, Golden);
                        nohit ->
                            game(Controller, {{X1_, Y1_},{X2_, Y2_}}, {Player1_sim, Player2_sim}, Powerups,{P1, P2}, Timer, Ticker, Golden) % if no hit call ticker after update_deltas 
                           end
                    end
            end.

%TODO é preciso transformar este primeiro par numa lista para cada um dos power_ups
%depois pode-se reutilizar a função para spawnar os power_ups se tivermos em conta os jogadores
get_random_pos(Positions, {Boundx_max, Boundy_max}) ->
    Radius = ?RADIUS * 3,
    {NewPosX, NewPosY} = {rand:uniform(Boundx_max-2*?RADIUS-2)+?RADIUS+1,rand:uniform(Boundy_max-2*?RADIUS-2)+?RADIUS + 1},
    Bool = lists:any(fun({X,Y,_}) -> colision(X, Y, NewPosX, NewPosY, Radius) end, Positions), 
    if
        Bool ->
            get_random_pos(Positions, {Boundx_max, Boundy_max});
        true ->
            {NewPosX, NewPosY}
    end.

% [0, 2pi]
normalize(Angle) ->
    math:fmod(Angle + 2*math:pi(), 2*math:pi()).

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
            %io:format("vroom received, over\n"),
            NewPlayerState =
                {{Vx + Accel * math:cos(Alfa), Vy + Accel * math:sin(Alfa)}, Alfa, {Accel, AngVel}},
            simulator(NewPlayerState, Flag bor 1);
        {change_direction, Dir} when Flag band 2 == 0 ->
            %io:format("turn received, over\n"),
            NewPlayerState = {{Vx, Vy}, normalize(Alfa + Dir*AngVel), {Accel, AngVel}},
            simulator(NewPlayerState, Flag bor 2);
        _ ->
            simulator(PlayerState, Flag)
    after
        0 ->
            receive
                {change_accel, Delta} ->
                    io:format("?:~p", [Delta*(?BASE_ACCEL/Accel)]),
                    simulator({{Vx, Vy}, Alfa, {Accel + Delta*(?BASE_ACCEL/Accel), AngVel}}, Flag);
                {change_angvel, Delta} ->
                    io:format("?:~p", [Delta*(?BASE_ANGVEL/AngVel)]),
                    simulator({{Vx, Vy}, Alfa, {Accel, AngVel + Delta*(?BASE_ANGVEL/AngVel)}}, Flag);
                reset_state ->
                    simulator({{0, 0}, 0, {?BASE_ACCEL, ?BASE_ANGVEL}}, Flag); %TODO define starting values!!!!!!!!!!!!!!!!!!!!!!
                reset_param ->
                    simulator({{Vx, Vy}, Alfa, {?BASE_ACCEL, ?BASE_ANGVEL}}, Flag); %TODO define starting values!!!!!!!!!!!!!!!!!!!!!!
                {return_state, From} ->
                    From ! {PlayerState, self()},
                    simulator(PlayerState, 0);
                decay ->
                    if 
                        Accel > ?BASE_ACCEL ->
                            Accel_ = max(Accel - ?DECAY_RATE, ?BASE_ACCEL),
                            io:format("decay Accel ~p", [Accel_]);
                        true ->
                            Accel_ = Accel
                    end,
                    if
                        AngVel > ?BASE_ANGVEL ->
                            AngVel_ = max(AngVel - ?DECAY_RATE, ?BASE_ANGVEL),
                            io:format("decay AngVel ~p", [AngVel_]);
                        true ->
                            AngVel_ = AngVel
                    end,
                    simulator({{Vx, Vy}, Alfa, {Accel_, AngVel_}}, Flag)
            end
    end.

colision(X1, Y1, X2, Y2, Radius) ->
    (X1 - X2) * (X1 - X2) + (Y1 - Y2) * (Y1 - Y2) =< Radius * Radius.

check_color({X, Y, C}, Sim) ->
    %TODO check proper delta
    case C of
        blue ->
            io:format("blue hit"),
            Sim ! {change_angvel, ?DELTA_ANGLE},
            {X, Y, C};
        green ->
            io:format("green hit"),
            Sim ! {change_accel, ?DELTA_ACC},
            {X, Y, C};
        red ->
            io:format("red hit"),
            Sim ! reset_param,
            {X, Y, C}
    end.

update_deltas({X1, Y1}, Powerups, Sim) ->
    Radius = ?RADIUS * 2, %TODO tune
    HitList = lists:filter(fun({X, Y, _}) -> colision(X1, Y1, X, Y, Radius) end, Powerups),
    [check_color(X, Sim) || X <- HitList].

check_player_colision({X1, Y1}, {X2, Y2}, Alfa1, Alfa2) ->
    Radius = ?RADIUS * 2,%TODO tune
    GuardCol = colision(X1, Y1, X2, Y2, Radius) and (abs(Alfa1 - Alfa2) < math:pi()/2), 
    %io:format("~w ~w ~w \n", [colision(X1, Y1, X2, Y2, Radius), Alfa1, Alfa2]),
    if GuardCol ->
           GuardPoint = (X2 - X1) * math:cos(Alfa2) + (Y2 - Y1) * math:sin(Alfa2) > 0,
           if GuardPoint ->
                %io:format("Hit1\n"),
                hit1;
            true ->
                %io:format("Hit2\n"),
                hit2
           end;
        true ->
            %io:format("NoHit\n"),
            nohit
    end.

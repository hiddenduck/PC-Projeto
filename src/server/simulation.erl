-module(simulation).

-export([start_game/1, change_speed/1, change_angle/2]).

-define(DELTA_ANGLE, 0.125).
-define(DELTA_ACC, 0.125).
-define(RADIUS, 15).
-define(GAME_DURATION, 60000).
-define(POWER_CHANCE, 100).
-define(BASE_ACCEL, 0.125).
-define(BASE_ANGVEL, 0.125).
-define(TICK_RATE, 100).
-define(BOX_LIMIT, 10).
-define(DECAY_RATE, 0.0001).

%start_game spawns a simulator for each player
%and spawns a ticker to start a game
start_game(Controller) ->
    P1State = {{100, 400}, {0, 0}, 0, {0.125,0.125}},
    P2State = {{400, 400}, {0, 0}, math:pi(), {0.125,0.125}},
    game(
        Controller,
        [],     %Powerups
        P1State,%P1
        P2State,%P2
        {0, 0}, %Points
        spawn(fun() -> ticker(self()) end),  %Timer
        spawn(fun() -> timer(self()) end),  %Timer
        false, %Golden
        0      %Flag
    ).

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
game(Controller, Powerups, P1State, P2State, Points, Timer, Ticker, Golden, Flag) ->
    {{X1,Y1}, {V1x, V1y}, Alfa1, {Accel1, AngVel1}} = P1State,
    {{X2,Y2}, {V2x, V2y}, Alfa2, {Accel2, AngVel2}} = P2State,
    {P1, P2} = Points,
    receive
        _ when Golden, P1 /= P2 -> 
            %io:format("tick tock\n"),
            Loser = 
            if
                P1 > P2 -> p2;
                P1 < P2 -> p1
            end,
            space_server:abort_game(Controller, Loser),
            kill_procs([Timer, Ticker]),
            ok;
        {stop, Controller} ->
            kill_procs([Timer, Ticker]),
            ok;
        timeout when P1 == P2 ->
            space_server:golden_point(Controller),
            game(Controller, Powerups, P1State, P2State, Points, Timer, Ticker, true, Flag);
        timeout when P1 /= P2 -> 
            %io:format("tick tock\n"),
            Loser = 
            if
                P1 > P2 -> p2;
                P1 < P2 -> p1
            end,
            space_server:abort_game(Controller, Loser),
            kill_procs([Timer, Ticker]),
            ok;

        tick ->

            {X1_, Y1_} = {X1 + V1x, Y1 + V1y},
            {X2_, Y2_} = {X2 + V2x, Y2 + V2y},
            %
            %space_server:positions({X1_, Y1_, Alfa1}, {X2_, Y2_, Alfa2}, Controller, self()),

            Boundx_min = 0,%TODO tune
            Boundx_max = 700,%TODO tune
            Boundy_min = 0,%TODO tune
            Boundy_max = 700,%TODO tune
            
            
            if % check players in bounds
                X1_ < Boundx_min + ?RADIUS; X1_ > Boundx_max - ?RADIUS; Y1_ < Boundy_min + ?RADIUS; Y1_ > Boundy_max - ?RADIUS ->
                    
                    space_server:abort_game(Controller, p1),
                    kill_procs([Timer, Ticker]),
                    ok;

                X2_ < Boundx_min + ?RADIUS; X2_ > Boundx_max - ?RADIUS; Y2_ < Boundy_min + ?RADIUS; Y2_ > Boundy_max - ?RADIUS ->
                    
                    space_server:abort_game(Controller, p2),
                    kill_procs([Timer, Ticker]),
                    ok;
                    
                true -> % else check_player_colision
                    
                    Powerups_ = gen_random_box({X1_, Y1_}, {X2_, Y2_}, Powerups, {Boundx_max, Boundy_max}),
                    
                    {Accel1_, AngVel1_, HitList1} = update_deltas({X1_, Y1_}, Powerups_, Accel1, AngVel1),
                    {Accel2_, AngVel2_, HitList2} = update_deltas({X2_, Y2_}, Powerups_, Accel2, AngVel2),
                    
                    case check_player_colision({X1, Y1}, {X2, Y2}, Alfa1, Alfa2) of
                        hit1 ->
                            {X_, Y_} = get_random_pos([{X1_, Y1_, Alfa1} | Powerups], {Boundx_max, Boundy_max}),
                            P1State_ = {{X1_, Y1_}, {V1x, V1y}, Alfa1, {?BASE_ACCEL, ?BASE_ANGVEL}},
                            P2State_ = {{X_, Y_}, {0, 0}, 0, {?BASE_ACCEL, ?BASE_ANGVEL}},
                            Points_ = {P1 + 1, P2};
                        hit2 ->
                            {X_, Y_} = get_random_pos([{X2_, Y2_, Alfa2} | Powerups], {Boundx_max, Boundy_max}),
                            P2State_ = {{X2_, Y2_}, {V2x, V2y}, Alfa2, {?BASE_ACCEL, ?BASE_ANGVEL}},
                            P1State_ = {{X_, Y_}, {0, 0}, 0, {?BASE_ACCEL, ?BASE_ANGVEL}},
                            Points_ = {P1, P2 + 1};
                        nohit ->
                            P1State_ = {{X1_, Y1_}, {V1x, V1y}, Alfa1, {Accel1_, AngVel1_}},
                            P2State_ = {{X2_, Y2_}, {V2x, V2y}, Alfa2, {Accel2_, AngVel2_}},
                            Points_ = Points
                    end,
                    game(Controller, Powerups_ -- HitList1 -- HitList2, P1State_, P2State_, Points_, Timer, Ticker, Golden, Flag)
            end
    end.

gen_random_box(Pos1, Pos2, Powerups, Bounds) ->
    GenFlag = rand:uniform(?POWER_CHANCE),
    if
        GenFlag == 1, length(Powerups) < ?BOX_LIMIT ->
            C = 
            case (rand:uniform(5) - 1) rem 3  of 
                    0 -> blue;
                    1 -> green;
                    2 -> red
                end,
            {X, Y} = get_random_pos([Pos1 , Pos2 | Powerups], Bounds),
            [{X, Y, C} | Powerups];
        true ->
            Powerups
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
            simulator(NewPlayerState, Flag bor 2)
    after
        0 ->
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
                {change_accel, Delta} ->
                    io:format("?:~p", [Delta*(?BASE_ACCEL/Accel)]),
                    simulator({{Vx, Vy}, Alfa, {Accel + Delta*(?BASE_ACCEL/Accel), AngVel}}, Flag);
                {change_angvel, Delta} ->
                    io:format("?:~p", [Delta*(?BASE_ANGVEL/AngVel)]),
                    simulator({{Vx, Vy}, Alfa, {Accel, AngVel + Delta*(?BASE_ANGVEL/AngVel)}}, Flag);
                reset_state ->
                    io:format("reset state\n"),
                    simulator({{0, 0}, 0, {?BASE_ACCEL, ?BASE_ANGVEL}}, Flag); %TODO define starting values!!!!!!!!!!!!!!!!!!!!!!
                reset_param ->
                    io:format("reset param\n"),
                    simulator({{Vx, Vy}, Alfa, {?BASE_ACCEL, ?BASE_ANGVEL}}, Flag); %TODO define starting values!!!!!!!!!!!!!!!!!!!!!!
                {return_state, From} ->
                    From ! {PlayerState, self()},
                    simulator(PlayerState, 0);
                decay ->
                    if 
                        Accel > ?BASE_ACCEL ->
                            %io:format("decay Accel ~p", [Accel_]),
                            Accel_ = max(Accel - ?DECAY_RATE, ?BASE_ACCEL);
                        true ->
                            Accel_ = Accel
                    end,
                    if
                        AngVel > ?BASE_ANGVEL ->
                            %io:format("decay AngVel ~p", [AngVel_]),
                            AngVel_ = max(AngVel - ?DECAY_RATE, ?BASE_ANGVEL);
                        true ->
                            AngVel_ = AngVel
                    end,
                    simulator({{Vx, Vy}, Alfa, {Accel_, AngVel_}}, Flag);
                _ ->
                    simulator(PlayerState, Flag)
            end
    end.

decay(Accel, AngVel) ->
    if 
        Accel > ?BASE_ACCEL ->
            %io:format("decay Accel ~p", [Accel_]),
            Accel_ = max(Accel - ?DECAY_RATE, ?BASE_ACCEL);
        true ->
            Accel_ = Accel
    end,
    if
        AngVel > ?BASE_ANGVEL ->
            %io:format("decay AngVel ~p", [AngVel_]),
            AngVel_ = max(AngVel - ?DECAY_RATE, ?BASE_ANGVEL);
        true ->
            AngVel_ = AngVel
    end,
    {Accel_, AngVel_}.
    

colision(X1, Y1, X2, Y2, Radius) ->
    (X1 - X2) * (X1 - X2) + (Y1 - Y2) * (Y1 - Y2) =< Radius * Radius.

check_color({_, _, C}, {Accel, AngVel}) ->
    %TODO check proper delta
    case C of
        blue ->
            io:format("blue hit"),
            {Accel + ?DELTA_ACC*(?BASE_ACCEL/Accel), AngVel};
        green ->
            io:format("green hit"),
            {Accel, AngVel + ?DELTA_ANGLE*(?BASE_ANGVEL/AngVel)};
        red ->
            io:format("red hit"),
            {?BASE_ACCEL, ?BASE_ANGVEL}
    end.

update_deltas({X, Y}, Powerups, Accel, AngVel) ->
    Radius = ?RADIUS * 2, %TODO tune
    case lists:filter(fun({X_, Y_, _}) -> colision(X, Y, X_, Y_, Radius) end, Powerups) of
        [] ->
            {decay(Accel, AngVel), []};
        HitList -> {lists:foldl(fun check_color/2, {Accel, AngVel}, HitList), HitList}
    end.

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

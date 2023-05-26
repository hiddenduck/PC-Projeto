-module(simulation).

-export([start_game/1, change_speed/1, change_angle/2, leave/2, buttons/3]).

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
start_game({P1Proc, P2Proc} = Info) ->
    P1State = {{100, 400}, {0, 0}, 0, {0.125,0.125}},
    P2State = {{400, 400}, {0, 0}, math:pi(), {0.125,0.125}},
    Self = self(),
    space_server:start_game(P1Proc, {{100, 400, 0}, {400, 400, math:pi()}}),
    space_server:start_game(P2Proc, {{400, 400, math:pi()}, {100, 400, 0}}),
    game(
        Info,
        [],     %Powerups
        P1State,%P1
        P2State,%P2
        {false, false, false},
        {false, false, false},
        {0, 0}, %Points
        spawn(fun() -> ticker(Self) end),  %Timer
        spawn(fun() -> timer(Self) end),  %Timer
        false, %Golden
        0      %Flag
    ).

buttons(Buttons, Game, Player) ->
    Game ! {Buttons, Player}.

leave(Game, Player) ->
    Game ! {forfeit, Player}.

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
game(GameInfo, Powerups, P1State, P2State, P1Keys, P2Keys, Points, Timer, Ticker, Golden, Flag) ->
    {{X1,Y1}, {V1x, V1y}, Alfa1, {Accel1, AngVel1}} = P1State,
    {{X2,Y2}, {V2x, V2y}, Alfa2, {Accel2, AngVel2}} = P2State,
    {P1Proc, P2Proc} = GameInfo,
    {P1, P2} = Points,
    receive
        _ when Golden, P1 /= P2 -> 
            %io:format("tick tock\n"),
            space_server:end_game(
              if
                  P1 > P2 -> {P1Proc, P2Proc};
                  P1 < P2 -> {P2Proc, P1Proc}
              end),
                  
            kill_procs([Timer, Ticker]),
            ok;
        stop ->
            kill_procs([Timer, Ticker]),
            ok;
        timeout when P1 == P2 ->
            space_server:golden_point(P1Proc, P2Proc),
            game(GameInfo, Powerups, P1State, P2State, P1Keys, P2Keys, Points, Timer, Ticker, true, Flag);
        timeout when P1 /= P2 -> 
            %io:format("tick tock\n"),
            space_server:end_game(
              if
                  P1 > P2 -> {P1Proc, P2Proc};
                  P1 < P2 -> {P2Proc, P1Proc}
              end),

            kill_procs([Timer, Ticker]),
            ok;
        {forfeit, Player} ->
            space_server:end_game(
              case Player of
                  P1Proc -> {P2Proc, P1Proc};
                  P2Proc -> {P1Proc, P2Proc}
              end),

            kill_procs([Timer, Ticker]),
            ok;
        {{A,W,D},P1Proc} ->
            game(GameInfo, Powerups, P1State, P2State, {A, W, D}, P2Keys, Points, Timer, Ticker, Golden, Flag);
        {{A,W,D},P2Proc} ->
            game(GameInfo, Powerups, P1State, P2State, P1Keys, {A, W, D}, Points, Timer, Ticker, Golden, Flag);
        tick ->

            {V1x_, V1y_, Alfa1_} = process_keys(P1Keys, V1x, V1y, Alfa1, Accel1, AngVel1),
            {V2x_, V2y_, Alfa2_} = process_keys(P2Keys, V2x, V2y, Alfa2, Accel2, AngVel2),

            {X1_, Y1_} = {X1 + V1x, Y1 + V1y},
            {X2_, Y2_} = {X2 + V2x, Y2 + V2y},

            space_server:positions({X1_, Y1_}, {X2_, Y2_}, P1Proc, P2Proc, self()),
            
            Boundx_min = 0,%TODO tune
            Boundx_max = 700,%TODO tune
            Boundy_min = 0,%TODO tune
            Boundy_max = 700,%TODO tune
            
            
            if % check players in bounds
                X1_ < Boundx_min + ?RADIUS; X1_ > Boundx_max - ?RADIUS; Y1_ < Boundy_min + ?RADIUS; Y1_ > Boundy_max - ?RADIUS ->
                    
                    space_server:end_game(P2Proc, P1Proc),
                    kill_procs([Timer, Ticker]),
                    ok;

                X2_ < Boundx_min + ?RADIUS; X2_ > Boundx_max - ?RADIUS; Y2_ < Boundy_min + ?RADIUS; Y2_ > Boundy_max - ?RADIUS ->
                    
                    space_server:end_game(P1Proc, P2Proc),
                    kill_procs([Timer, Ticker]),
                    ok;
                    
                true -> % else check_player_colision
                    
                    {{Accel1_, AngVel1_}, HitList1} = update_deltas({X1_, Y1_}, Powerups, Accel1, AngVel1),
                    {{Accel2_, AngVel2_}, HitList2} = update_deltas({X2_, Y2_}, Powerups, Accel2, AngVel2),

                    case {gen_random_box({X1_, Y1_}, {X2_, Y2_}, Powerups, {Boundx_max, Boundy_max}), HitList1 ++ HitList2} of
                        {null, []} ->
                            Powerups_ = Powerups;
                        {Powerup, HitList} ->
                            Powerups_ = [Powerup | Powerups],
                            space_server:boxes([Powerup], HitList, P1Proc, P2Proc, self())
                    end,
                    
                    case check_player_colision({X1, Y1}, {X2, Y2}, Alfa1, Alfa2) of
                        hit1 ->
                            {X_, Y_} = get_random_pos([{X1_, Y1_, Alfa1_} | Powerups], {Boundx_max, Boundy_max}),
                            P1State_ = {{X1_, Y1_}, {V1x_, V1y_}, Alfa1_, {?BASE_ACCEL, ?BASE_ANGVEL}},
                            P2State_ = {{X_, Y_}, {0, 0}, 0, {?BASE_ACCEL, ?BASE_ANGVEL}},
                            Points_ = {P1 + 1, P2},

                            space_server:positions({X1_, Y1_}, {X_, Y_}, P1Proc, P2Proc, self()),
                            space_server:score(P1 + 1, P2, P1Proc, P2Proc, self());

                        hit2 ->
                            {X_, Y_} = get_random_pos([{X2_, Y2_, Alfa2_} | Powerups], {Boundx_max, Boundy_max}),
                            P2State_ = {{X2_, Y2_}, {V2x_, V2y_}, Alfa2_, {?BASE_ACCEL, ?BASE_ANGVEL}},
                            P1State_ = {{X_, Y_}, {0, 0}, 0, {?BASE_ACCEL, ?BASE_ANGVEL}},
                            Points_ = {P1, P2 + 1},

                            space_server:positions({X_, Y_}, {X2_, Y2_}, P1Proc, P2Proc, self()),
                            space_server:score(P1, P2 + 1, P1Proc, P2Proc, self());

                        nohit ->
                            P1State_ = {{X1_, Y1_}, {V1x_, V1y_}, Alfa1_, {Accel1_, AngVel1_}},
                            P2State_ = {{X2_, Y2_}, {V2x_, V2y_}, Alfa2_, {Accel2_, AngVel2_}},
                            Points_ = Points
                    end,
                    game(GameInfo, Powerups_ -- HitList1 -- HitList2, P1State_, P2State_, P1Keys, P2Keys, Points_, Timer, Ticker, Golden, Flag)
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
            {X, Y, C};
        true ->
            null
    end.

process_keys({A, W, D}, Vx, Vy, Alfa, Accel, AngVel) ->
    case W of
        true ->
            {Vx_, Vy_} = {Vx + Accel * math:cos(Alfa), Vy + Accel * math:sin(Alfa)};
        false ->
            {Vx_, Vy_} = {Vx, Vy}
    end,
    case {A,D} of
        {true, false} ->
            Alfa_ = normalize(Alfa - AngVel);
        {false, true} ->
            Alfa_ = normalize(Alfa + AngVel);
        {false, false} ->
            Alfa_ = Alfa
    end,
    {Vx_, Vy_, Alfa_}.

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

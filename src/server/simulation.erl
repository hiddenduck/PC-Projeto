-module(simulation).

-export([start_game/1, change_speed/1, change_angle/2]).

-define(RADIUS, 10).

%start_game spawns a simulator for each player
%and spawns a ticker to start a game
start_game(Game) ->
    P1 = {{0, 0}, 0, {0.25,0.125}},
    P2 = {{0, 0}, 0, {0.25,0.125}},
    Player1_sim = spawn(fun() -> simulator(P1, 0) end),
    Player2_sim = spawn(fun() -> simulator(P2, 0) end),
    GameSim = spawn(fun() -> Self = self(),
        game(
        Game,
        {{100, 400}, {400, 400}},
        {Player1_sim, Player2_sim},
        [],
        {0, 0},
        spawn(fun() -> ticker(Self) end)
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
            ok;
        {reset, GameSim} -> 
            sleep(20000), %TODO tune
            ticker(GameSim)
    after
        100 ->
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
    sleep(60000),
    io:format("times up"),
    GameSim ! timeout.

%Game controls the general game state such as 
%player position Powerups and Points
%it updates player position by quering simulator
%and calculating position
%finally it checks if a player is out of bounds
%colison of players and updates deltas from Powerups
game(Controller, Pos, Player_sims, Powerups, {P1, P2}, Ticker) ->
    receive
        {stop, Controller} ->
            {Player1_sim, Player2_sim} = Player_sims,
            Player1_sim ! stop,
            Player2_sim ! stop,
            ok;
        timeout when P1 == P2 ->
            space_server:golden_point(Controller),
            game(Controller, Pos, Player_sims, Powerups, {P1, P2}, Ticker);
        timeout when P1 /= P2 -> 
            %io:format("tick tock\n"),
            {Player1_sim, Player2_sim} = Player_sims,
            Loser = 
            if
                P1 > P2 -> p2;
                P1 < P2 -> p1
            end,
            space_server:abort_game(Controller, Loser),
            Player1_sim ! stop,
            Player2_sim ! stop,
            ok;

        tick ->

            %Base1 = {100,400},
            %Base2 = {400,400},

            {Player1_sim, Player2_sim} = Player_sims,
            {Pos1, Pos2} = Pos,

            {X1_, Y1_, Alfa1} = new_pos(Pos1, Player1_sim),
            {X2_, Y2_, Alfa2} = new_pos(Pos2, Player2_sim),

            space_server:positions({X1_, Y1_, Alfa1}, {X2_, Y2_, Alfa2}, Controller, self()),

            Boundx_min = 0,%TODO tune
            Boundx_max = 700,%TODO tune
            Boundy_min = 0,%TODO tune
            Boundy_max = 700,%TODO tune

            if % check players in bounds
                X1_ < Boundx_min; X1_ > Boundx_max; Y1_ < Boundy_min; Y1_ > Boundy_max ->
                    Player1_sim ! reset_param,
                    %Player2_sim ! reset_param,
                    {NewPosX, NewPosY} = get_random_pos({X2_, Y2_}, {Boundx_max, Boundy_max}),
                    space_server:positions({NewPosX, NewPosY, 0}, {X2_, Y2_, Alfa2}, Controller, self()),
                    space_server:score(P1, P2+1, Controller, self()),

                    Ticker ! reset,

                    game(Controller, {{NewPosX, NewPosY}, {X2_, Y2_}}, Player_sims, Powerups, {P1, P2 + 1}, Ticker);
                X2_ < Boundx_min; X2_ > Boundx_max; Y2_ < Boundy_min; Y2_ > Boundy_max ->
                    %Player1_sim ! reset_param,
                    Player2_sim ! reset_param,
                    {NewPosX, NewPosY} = get_random_pos({X1_, Y1_}, {Boundx_max, Boundy_max}),
                    
                    space_server:positions({X1_, Y1_, Alfa1}, {NewPosX, NewPosY, 0}, Controller, self()),
                    space_server:score(P1 + 1, P2, Controller, self()),

                    Ticker ! reset,

                    game(Controller, {{X1_, Y1_}, {NewPosX, NewPosY}}, Player_sims, Powerups, {P1 + 1, P2}, Ticker);
                true -> % else check_player_colision
                    case check_player_colision(Pos1, Pos2, Alfa1, Alfa2) of
                        hit1 ->
                            Player1_sim ! reset_param,
                            %Player2_sim ! reset_param,
                            {NewPosX, NewPosY} = get_random_pos({X2_, Y2_}, {Boundx_max, Boundy_max}),
                            space_server:positions({NewPosX, NewPosY, 0}, {X2_, Y2_, Alfa2}, Controller, self()),
                            space_server:score(P1 + 1, P2, Controller, self()),

                            Ticker ! reset,

                            game(Controller, {{NewPosX, NewPosY}, {X2_, Y2_}}, Player_sims, Powerups, {P1, P2 + 1}, Ticker);
                        hit2 ->
                            Player2_sim ! reset_param,
                            {NewPosX, NewPosY} = get_random_pos({X1_, Y1_}, {Boundx_max, Boundy_max}),
                    
                            space_server:positions({X1_, Y1_, Alfa1}, {NewPosX, NewPosY, 0}, Controller, self()),
                            space_server:score(P1, P2 + 1, Controller, self()),

                            Ticker ! reset,

                            game(Controller, {{X1_, Y1_}, {NewPosX, NewPosY}}, Player_sims, Powerups, {P1 + 1, P2}, Ticker);
                        nohit ->
                            game(Controller, {{X1_, Y1_},
                                             {X2_, Y2_}}, % if no hit call ticker after update_deltas
                                 {Player1_sim, Player2_sim},
                                 Powerups
                                 -- update_deltas({X1_, Y1_}, Powerups, Player1_sim)
                                 -- update_deltas({X2_, Y2_}, Powerups, Player2_sim),
                                 {P1, P2}, Ticker)
                           end
                    end
            end.

%TODO é preciso transformar este primeiro par numa lista para cada um dos power_ups
%depois pode-se reutilizar a função para spawnar os power_ups se tivermos em conta os jogadores
get_random_pos({OtherPlayerX, OtherPlayerY}, {Boundx_max, Boundy_max}) ->
    Radius = ?RADIUS * 4,
    {NewPosX, NewPosY} = {rand:uniform(Boundx_max+1)-1,rand:uniform(Boundy_max+1)-1},
    Bool = colision(OtherPlayerX, OtherPlayerY, NewPosX, NewPosY, Radius),
    if
        Bool ->
            get_random_pos({OtherPlayerX, OtherPlayerY}, {Boundx_max, Boundy_max});
        true ->
            {NewPosX, NewPosY}
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
            %io:format("vroom received, over\n"),
            NewPlayerState =
                {{Vx + Accel * math:cos(Alfa), Vy + Accel * math:sin(Alfa)}, Alfa, {Accel, AngVel}},
            simulator(NewPlayerState, Flag bor 1);
        {change_direction, Dir} when Flag band 2 == 0 ->
            %io:format("turn received, over\n"),
            NewPlayerState = {{Vx, Vy}, math:fmod(Alfa + Dir * AngVel, 2*math:pi()), {Accel, AngVel}},
            simulator(NewPlayerState, Flag bor 2);
        _ ->
            simulator(PlayerState, Flag)
    after
        0 ->
            receive
                {change_accel, Delta} ->
                    simulator({{Vx, Vy}, Alfa, {Accel + Delta, AngVel}}, Flag);
                {change_angvel, Delta} ->
                    simulator({{Vx, Vy}, Alfa, {Accel, AngVel + Delta}}, Flag);
                reset_param ->
                    
                    simulator({{0, 0}, 0, {0.25,0.125}}, Flag); %TODO define starting values!!!!!!!!!!!!!!!!!!!!!!
                {return_state, From} ->
                    From ! {PlayerState, self()},
                    simulator(PlayerState, 0)
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
    Radius = ?RADIUS * 2,%TODO tune
    GuardCol = colision(X1, Y1, X2, Y2, Radius) and (abs(Alfa1 - Alfa2) < math:pi()/2),
    if GuardCol ->
           GuardPoint = (X2 - X1) * math:cos(Alfa2) + (Y2 - Y1) * math:sin(Alfa2) > 0,
           if GuardPoint ->
                io:format("Hit1\n"),
                hit1;
            true ->
                io:format("Hit2\n"),
                hit2
           end;
        true ->
            io:format("NoHit\n"),
            nohit
    end.

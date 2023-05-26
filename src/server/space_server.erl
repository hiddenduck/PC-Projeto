-module(space_server).
-export([start/1, stop/0, abort_game/2, positions/4, boxes/4, score/4, golden_point/1, online/0]). % server:start(1234)
                            % nc localhost 1234
                            % netstat -p tcp -an | grep 1234
-define(GAMETIME, 120000).

start(Port) -> register(?MODULE, spawn(fun() -> server(Port) end)), ok.

stop() -> ?MODULE ! stop.

%Início do registo do server, começa com um ListeningSocket para ir gerando um para cada jogador
%Regista dois processos, um como o lobby e outro como o game_manager, depois torna-se no acceptor
server(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [{packet, line}, {reuseaddr, true}]),
    spawn(fun() -> file_manager:start() end),
    spawn(fun() -> acceptor(LSock) end),
    register(lobby, spawn(fun()-> lobby(#{}, #{}) end)),
    register(game_manager, spawn(fun() -> game_manager(#{}, []) end)),
    receive stop -> 
        file_manager:stop(),
        lobby ! stop,
        game_manager ! stop
    end.

online() ->
    lobby ! {online, self()},
    receive {Users, lobby} -> Users end.

%Lobby como sala tirada diretamente das salas definidas nas aulas prática
%A chave é o Username porque é necessário para testar quando um utilizador entra
lobby(Users, WinMap) ->
    %io:format("~p\n", [WinMap]),
    receive
        {top, 0, From} ->
            From ! {lists:sort(fun({U1, V1}, {U2, V2}) -> 
                if V1 =:= V2 -> U1<U2; true -> V1>V2 end end, maps:to_list(WinMap)), lobby},
            lobby(Users,WinMap);
        {top, Number, From} ->
            From ! {lists:sublist(lists:sort(fun({U1, V1}, {U2, V2}) -> 
                if V1 =:= V2 -> U1<U2; true -> V1>V2 end end, maps:to_list(WinMap)), Number), lobby},
            lobby(Users,WinMap);
        {online, From} ->
            From ! {maps:keys(Users), lobby},
            lobby(Users,WinMap);
        {loss, Username, User} ->
            io:format("user loss ~p ~n", [Username]),
            lobby(Users#{Username => {unready, User}}, WinMap);
        {win, Username, User} ->
            io:format("user won ~p ~n", [Username]),
            case maps:find(Username, WinMap) of
                {ok, OldWins} -> Wins = OldWins + 1;
                _ -> Wins = 1
            end,
            lobby(Users#{Username => {unready, User}}, WinMap#{Username => Wins});
        {enter, Username, User} ->
            io:format("user entered ~p ~n", [Username]),
            lobby(Users#{Username => {unready, User}}, WinMap);
        {leave, Username, User} ->
            io:format("user left ~p ~n", [Username]),
            lobby(maps:remove(Username, Users), WinMap);
        {Status, Username, User} ->
            io:format("user ~p ~p ~n", [Status, Username]),
            lobby(Users#{Username => {unready, User}}, WinMap);
        stop -> 
            lists:map(fun({_, Pid})-> Pid ! stop end, maps:values(Users)),
            io:format("lobby terminado\n")
    end.  

%Gestor dos jogos
%RoomMap é um mapa que associa níveis ao jogador que o começou e ao jogo, que é limpo sempre que um jogo de um dado nível começa
%GameControllers contém todas as salas de jogos em andamento
%A junção de GameControllers com o Lobby dá todos os jogadores atualmente online
game_manager(RoomMap, GameControllers) ->
    receive
        {unready, Level, User} -> 
            case maps:find(Level, RoomMap) of
                {ok, {User, Controller}} ->
                    %Avisar o controlador para não esperar, uma maneira de terminar o ready
                    Controller ! {abort, game_manager},
                    New_Map = maps:remove(Level, RoomMap);
                _ ->
                    %Se alguém estiver ready e não for este não se faz nada?
                    New_Map = RoomMap,
                    User ! {error_not_ready, game_manager}    
            end,
            game_manager(New_Map, GameControllers);
        {ready, Level, Username, User} ->
            case maps:find(Level, RoomMap) of
                {ok, {User, _}} ->
                    User ! {error_already_ready, game_manager},
                    game_manager(RoomMap, GameControllers);
                {ok, {_, Controller}} ->
                    %juntar-se ao jogo
                    Controller ! {start, Username, User, game_manager},
                    User ! {ok, Controller, game_manager},
                    New_Map = maps:remove(Level, RoomMap),
                    game_manager(New_Map, [Controller | GameControllers]);
                _ ->
                    %criar uma espera
                    Controller = spawn(fun()-> ready([{Username,User}]) end),
                    User ! {ok, Controller, game_manager},
                    game_manager(RoomMap#{Level => {User, Controller}}, GameControllers)
            end;
        {end_game, Controller} ->
            game_manager(RoomMap, GameControllers -- [Controller]);
        stop ->
            [Controller ! stop || Controller <- GameControllers]
    end.

%Função de espera que correrá depois da chamada ready de um utilizador
%Um jogador dá ready. 
%O segundo jogador dá ready e depois o primeiro cancela o jogo, enquanto o ecrã dele ainda não recebeu o ok
ready([{FstUsername, FstPlayer}]) ->
    receive 
        %Este abort tem de vir de parte do game_manager aquando de um unready
        {abort, game_manager} -> ok;
        %Não pode receber o abort do primeiro jogador porque isso tem de ser testado no sync_up
        %Problemas de concorrência podem fazer com que o jogo comece mas um dos jogadores se desconecte antes de o saber
        {start, SndUsername, SndPlayer, game_manager} ->
            sync_up({FstUsername, FstPlayer}, {SndUsername, SndPlayer})
    end.

abort_sync(FstPlayer, SndPlayer) ->
    FstPlayer ! {abort, self()},
    SndPlayer ! {abort, self()},
    game_manager ! {end_game, self()}.

%Função chamada depois da ligação de ambos os jogadores, para começar a Simulação e sintonizar ambos os jogadores
%Se algum for cancelado dentro de um minuto o jogo não ocorre e pontos não são dados
%As simulações são passadas para cada um dos jogadores
sync_up({FstUsername, FstPlayer}, {SndUsername, SndPlayer}) ->
    %Avisar os utilizadores para entrarem no jogo
    {Player1Sim, Player2Sim, GameSim} = simulation:start_game(self()),
    %io:format("~p ~p ~n", [Player1Sim, Player2Sim]),
    FstPlayer ! {start_game, Player1Sim, self()},
    SndPlayer ! {start_game, Player2Sim, self()},
    receive
        {ok, FromFst, FstPlayer} -> 
            receive
                {ok, FromSnd, SndPlayer} -> game({FstUsername, FromFst, FstPlayer}, {SndUsername, FromSnd, SndPlayer}, GameSim)
                after 15000 -> abort_sync(FstPlayer, SndPlayer)
            end;
        {ok, FromSnd, SndPlayer} -> 
            receive
                {ok, FromFst, FstPlayer} -> game({FstUsername, FromFst, FstPlayer}, {SndUsername, FromSnd, SndPlayer}, GameSim)
                after 15000 -> abort_sync(FstPlayer, SndPlayer)
            end;
        %1 minuto de espera para conexão parece justo, se não der é preciso avisar do fim do jogo
        {abort, FstPlayer} -> abort_sync(FstPlayer, SndPlayer);
        {abort, SndPlayer} -> abort_sync(FstPlayer, SndPlayer)
        after 15000 -> abort_sync(FstPlayer, SndPlayer)
    end.


%O Controller é a quem é enviada a mensagem (Controlador do jogo do lado do servidor)

golden_point(Controller) ->
    Controller ! golden.

%Termina o jogo graciosamente, dado um perdedor
abort_game(Controller, Loser) ->
    Controller ! {abort, Loser}.

%Recebe as posições da simulação em dois tuplos
%{xp,yp,ap}, {xe,ye,ap}
positions(FstPositions, SndPositions, Controller, Game) ->
    Controller ! {positions, FstPositions, SndPositions, Game}.

%Recebe as posições a adicionar e remover das caixas em listas de tuplos
%[{x1,y1,color1},{x2,y2,color2}]
boxes(Add, Remove, Controller, Game) ->
    Controller ! {boxes, Add, Remove, Game}.

%Recebe a pontuação de ambos os jogadores
score(FstPoints, SndPoints, Controller, Game) ->
    Controller ! {score, FstPoints, SndPoints, Game}.

game_cleanup({FstUsername, FromFst, ToFst}, {SndUsername, FromSnd, ToSnd}, Loser, GameSim) ->
    if 
        Loser == p1; Loser == ToFst ->
            WinnerUsername = SndUsername, LoserUsername = FstUsername,
            WinnerFrom = FromSnd, LoserFrom = FromFst;
        Loser == p2; Loser == ToSnd ->
            WinnerUsername = FstUsername, LoserUsername = SndUsername,
            WinnerFrom = FromFst, LoserFrom = FromSnd
    end,
    {ok, WinnerLevel, LoserLevel} = file_manager:end_game(WinnerUsername, LoserUsername),
    end_game(WinnerFrom,WinnerLevel,LoserFrom,LoserLevel),
    GameSim ! {stop, self()}.

handle(Type, FstArg, SndArg, FstTriple, SndTriple) ->
    {_, FromFst, _} = FstTriple,
    {_, FromSnd, _} = SndTriple,
    case Type of
        positions -> 
            positions(FstArg, SndArg, FromFst, self()),
            positions(SndArg, FstArg, FromSnd, self());
        score -> 
            score(FstArg, SndArg, FromFst, self()),
            score(SndArg, FstArg, FromSnd, self());
        boxes ->
            boxes(FstArg, SndArg, FromFst, self()),
            boxes(FstArg, SndArg, FromSnd, self())
    end.

%Espera do jogo que recebe o final do tempo e também o cancelar dos jogadores.
%Dita os vencedores, chamando a função para marcar pontos, o que pode fazer com que os restantes esperem por correr depois
game(FstTriple, SndTriple, GameSim) -> 
    receive
        %Recebe o jogador perdedor
        {abort, Player} ->
            game_cleanup(FstTriple, SndTriple, Player, GameSim);
        stop ->
            %ver a melhor forma de fazer o stop
            {_, _, ToFst} = FstTriple,
            {_, _, ToSnd} = SndTriple,
            abort_sync(ToFst, ToSnd)
            %end_game(FromFst, -1, FromSnd, -1);
    after 0 ->
        receive
            {abort, Player} ->
                game_cleanup(FstTriple, SndTriple, Player, GameSim);
            stop ->
                %ver a melhor forma de fazer o stop
                {_, _, ToFst} = FstTriple,
                {_, _, ToSnd} = SndTriple,
                abort_sync(ToFst, ToSnd);
                %end_game(FromFst, -1, FromSnd, -1);
            golden ->
                {_, FromFst, _} = FstTriple,
                {_, FromSnd, _} = SndTriple,
                FromFst ! {golden, self()},
                FromSnd ! {golden, self()},
                game(FstTriple, SndTriple, GameSim);
            {Type, FstArg, SndArg, _} ->
                handle(Type, FstArg, SndArg, FstTriple, SndTriple),
                game(FstTriple, SndTriple, GameSim)
        end
    end.

%Termina o jogo avisando todos os intervenientes
end_game(Winner,WinnerLevel, Loser, LoserLevel) ->
    Winner ! {victory, WinnerLevel, self()},
    Loser ! {defeat, LoserLevel, self()},
    game_manager ! {end_game, self()}.

% As funções seguintes dizem respeito às funções que ditam o estado dos jogadores
% A função acceptor abre a Socket de ligação com o utilizador
% Os diferentes estados são: Main_Menu; User; Ready_User; e Player

acceptor(LSock) ->
    case gen_tcp:accept(LSock) of
        {ok, Sock}  ->
            spawn(fun() -> acceptor(LSock) end),
            main_menu(Sock);
        _ -> ok
    end.

main_menu(Sock) ->
    receive
        {tcp, _, "register:" ++ DataN} ->
            Data = lists:droplast(DataN),
            [Username, Password] = string:split(Data, ":", all),
            case file_manager:create_account(Username, Password) of
                ok -> gen_tcp:send(Sock, "register:ok\n");
                user_exists -> gen_tcp:send(Sock, "register:user_exists\n")
            end,
            main_menu(Sock);
        {tcp, _, "login:" ++ DataN} -> 
            Data = lists:droplast(DataN),
            [Username, Password] = string:split(Data, ":", all),
            case file_manager:login(Username, Password) of
                ok ->
                    Bool = lists:member(Username, online()),
                    if Bool -> 
                        gen_tcp:send(Sock, "login:user_online\n");
                    true ->
                        lobby ! {unready, Username, self()},
                        case file_manager:check_level(Username) of
                            {ok, Level} ->  gen_tcp:send(Sock, "login:ok:" ++ integer_to_list(Level) ++ "\n"),
                                            user(Sock, Username);
                            {invalid_user, _} -> gen_tcp:send(Sock, "login:invalid_user\n");
                            _ -> gen_tcp:send(Sock, "login:error\n")
                        end
                    end;
                invalid_password ->
                    gen_tcp:send(Sock, "login:invalid_password\n");
                _ ->
                    gen_tcp:send(Sock, "login:unknown_username\n")
            end,
            main_menu(Sock);
        {tcp_error, _, _} -> ok;
        {tcp_closed, _, _} -> ok;
        _ -> gen_tcp:send(Sock, "login:unknown_command\n")
    end.

logout(Username, Sock) ->
    lobby ! {leave, Username, self()},
    gen_tcp:send(Sock, "logout:ok\n"),
    main_menu(Sock).

leaderboard(NumberN, Sock) ->
    Number = lists:droplast(NumberN),
    {Int, []} = string:to_integer(["0" | Number]),
    lobby ! {top, Int, self()},
    receive 
        {List, lobby} -> 
            %io:format("~p\n", [lists:foldl(fun({U, W}, Acc) -> lists:concat([Acc, U, "_", W, ":"]) end, "top:", List) ++ "\n"]),
            gen_tcp:send(Sock, 
                lists:foldl(fun({U, W}, Acc) -> lists:concat([Acc, U, "_", W, ":"]) end, 
                    "top:", List) 
                ++ "\n") 
    end.

user(Sock, Username) ->
    receive
        {tcp, _, "top:" ++ NumberN} ->
            leaderboard(NumberN, Sock),
            user(Sock, Username);
        {tcp, _, "logout:\n"} ->
            logout(Username, Sock);
        {tcp, _, "close:" ++ PasswdN} ->
            Passwd = lists:droplast(PasswdN),
            %Data = lists:droplast(DataN),
            case file_manager:close_account(Username, Passwd) of
                ok ->
                    lobby ! {leave, Username, self()},
                    gen_tcp:send(Sock, "close:ok\n");

                wrong_password -> 
                    gen_tcp:send(Sock, "close:error_wrong_password\n"), user(Sock, Username);
                invalid -> gen_tcp:send(Sock, "close:error_invalid\n"), user(Sock, Username)
            end;
        {tcp, _, "ready:true\n"} ->
            {ok, Level} = file_manager:check_level(Username),
            game_manager ! {ready, Level, Username, self()},
            lobby ! {ready, Username, self()},
            receive 
                {ok, Game, game_manager} -> gen_tcp:send(Sock, "ready:ok\n"), user_ready(Sock, Game, Username)
                %{error_already_ready, game_manager} -> gen_tcp:send(Sock, "game:error_already_ready"), user(Sock, Room, Username)
            end;
        {tcp_closed, _} ->
            lobby ! {leave, Username, self()};
        {tcp_error, _, _} ->
            lobby ! {leave, Username, self()};
        stop ->
            gen_tcp:close(Sock)
    end.

unready(Username, Game) ->
    Game ! {abort, self()},
    {ok, Level} = file_manager:check_level(Username),
    game_manager ! {unready, Level, self()}.

user_ready(Sock, Game, Username) -> 
    receive
        {start_game, Simulation, Game} ->
            lobby ! {game, Username, self()},
            %game:start
            gen_tcp:send(Sock, "game:s\n"),
            ToSim = self(),
            FromSim = spawn(fun() -> player_fromsim(Sock, Game, ToSim) end),
            Game ! {ok, FromSim, self()},
            player_tosim(Sock, Game, Simulation, Username, FromSim);
        {tcp, _, Data} ->
            case Data of
                "top:" ++ NumberN ->
                    leaderboard(NumberN, Sock),
                    user_ready(Sock, Game, Username);
                "logout:\n" -> 
                    unready(Username, Game),
                    logout(Username, Sock);
                "close:" ++ PasswdN -> 
                    Passwd = lists:droplast(PasswdN),
                    case file_manager:close_account(Username, Passwd) of
                        ok -> 
                            unready(Username, Game),
                            lobby ! {leave, Username, self()},
                            gen_tcp:send(Sock, "close:ok\n");

                        wrong_password -> gen_tcp:send(Sock, "close:error_wrong_password\n"), user_ready(Sock, Game, Username);
                        invalid -> gen_tcp:send(Sock, "close:error_invalid\n"), user_ready(Sock, Game, Username)
                    end;
                "ready:false\n" -> 
                    unready(Username, Game),
                    lobby ! {unready, Username, self()},
                    gen_tcp:send(Sock, "ready:ok\n"), user(Sock,  Username)
            end;
        {tcp_closed, _} ->
            unready(Username, Game),
            lobby ! {leave, Username, self()};
        {tcp_error, _, _} ->
            unready(Username, Game),
            lobby ! {leave, Username, self()};
        stop ->
            gen_tcp:close(Sock)
    end.

player_fromsim(Sock, Game, ToSim) ->
    %io:format("player_from\n"),
    receive
        stop -> ok;
        {victory, Level, Game} -> 
            ToSim ! {abort, self()},
            gen_tcp:send(Sock, "game:w:" ++ integer_to_list(Level) ++ "\n");
        {defeat, Level, Game} -> 
            ToSim ! {abort, self()},
            gen_tcp:send(Sock, "game:l\n")
        after 0 ->
            %io:format("player_from_after\n"),
            receive
                stop -> gen_tcp:close(Sock), ToSim ! {stop, self()};
                {victory, Level, Game} -> 
                    ToSim ! {abort, win, self()},
                    gen_tcp:send(Sock, "game:w:" ++ integer_to_list(Level) ++ "\n");
                {defeat, Level, Game} -> 
                    ToSim ! {abort, loss, self()},
                    gen_tcp:send(Sock, "game:l\n");
                {abort, ToSim} ->
                    %TODO ver isto com o Carlos
                    gen_tcp:send(Sock, "game:a\n");
                {golden, Game} -> 
                    gen_tcp:send(Sock, "game:g\n"),
                    player_fromsim(Sock, Game, ToSim);
                {positions, {XP, YP, AP}, {XE, YE, AE}, Game} -> 
                    %pos:x:y:alpha
                    %posE:x:y:alpha
                    %io:format("~p ~p ~p, ~p ~p ~p ~n", [XP, YP, AP, XE, YE, AE]),
                    gen_tcp:send(Sock, lists:concat(["pos:", XP, ":", YP , ":", AP,
                                "\nposE:", XE, ":", YE, ":", AE, "\n"])),
                    player_fromsim(Sock, Game, ToSim);
                {boxes, Add, Remove, Game} ->
                    %box:+_x_y_color
                    %box:+:x:y:color:-:x:y:color
                    %box:-:x:y:color
                    %Add e Remove são listas com listas dos elementos 
                    %[{x1,y1,color1}, {x2,y2,color2}]
                    %io:format("~p ~p ~n", [Add, Remove]),
                    StrList = string:join(  [lists:concat(["+:", X, ":", Y, ":", C]) || {X,Y,C} <- Add] ++
                                            [lists:concat(["-:", X, ":", Y, ":", C]) || {X,Y,C} <- Remove], ":"),
                    %io:format("~p\n", StrList),
                    gen_tcp:send(Sock, lists:concat(["box:", StrList, "\n"])),
                    player_fromsim(Sock, Game, ToSim);
                {score, Player, Enemy, Game} ->
                    gen_tcp:send(Sock, lists:concat(["points:", Player, ":", Enemy, "\n"])),
                    player_fromsim(Sock, Game, ToSim)
            end
    end.

%Filho direto do processo utilizador, o FromSim é o outro processo que deve ser terminado no fim 
player_tosim(Sock, Game, Simulation, Username, FromSim) -> 
    %io:format("player_to\n"),
    receive
        stop -> gen_tcp:close(Sock), FromSim ! stop;
        {abort, Game} ->
            FromSim ! {abort, self()},
            lobby ! {unready, Username, self()},
            user(Sock, Username);
        {abort, Res, FromSim} ->
            lobby ! {Res, Username, self()},
            user(Sock, Username);
        {tcp, _, DataN} -> 
            Data = lists:droplast(DataN),
            ["move", Left, Front, Right] = string:split(Data, ":", all),
            %io:format("~p ~p ~p ~n", [Left, Front, Right]),
            if 
                Left =:= "t" -> 
                    simulation:change_angle(Simulation,-1);
                Right =:= "t" -> 
                    simulation:change_angle(Simulation,1);
                true -> ok
            end,
            if 
                Front =:= "t" -> 
                    simulation:change_speed(Simulation);
                true -> ok
            end,
            player_tosim(Sock, Game, Simulation, Username, FromSim);
        {tcp_closed, _} -> 
            abort_game(Game, self()),
            lobby ! {leave, Username, self()};
            
        {tcp_error, _, _} -> 
            abort_game(Game, self()),
            lobby ! {leave, Username, self()}

    end.

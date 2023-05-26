-module(space_server).
-export([start/1, stop/0, end_game/1, start_game/2, positions/5, boxes/5, score/5, golden_point/2, online/0]). % server:start(1234)
                            % nc localhost 1234
                            % netstat -p tcp -an | grep 1234

start(Port) -> register(?MODULE, spawn(fun() -> server(Port) end)), ok.

stop() -> ?MODULE ! stop.

%Avisa os jogadores do Golden Point
golden_point(FstPlayer, SndPlayer) ->
    FstPlayer ! golden,
    SndPlayer ! golden.

%Recebe as posições da simulação em dois tuplos
%{xp,yp,ap}, {xe,ye,ap}
positions(FstPositions, SndPositions, FstPlayer, SndPlayer, Game) ->
    FstPlayer ! {positions, FstPositions, SndPositions, Game},
    SndPlayer ! {positions, SndPositions, FstPositions , Game}.

%Recebe as posições a adicionar e remover das caixas em listas de tuplos
%[{x1,y1,color1},{x2,y2,color2}]
boxes(Add, Remove, FstPlayer, SndPlayer, Game) ->
    FstPlayer ! {boxes, Add, Remove, Game},
    SndPlayer ! {boxes, Add, Remove, Game}.

%Recebe a pontuação de ambos os jogadores
score(FstPoints, SndPoints, FstPlayer, SndPlayer, Game) ->
    FstPlayer ! {score, FstPoints, SndPoints, Game},
    SndPlayer ! {score, SndPoints, FstPoints, Game}.

%Termina o jogo, dado um vencedor, avisando todos os intervenientes
end_game({Winner, Loser}) ->
    Winner ! {victory, self()},
    Loser ! {defeat, self()},
    game_manager ! {end_game, self()}.

%Responsabilidade do simulation
%Começa o jogo para um dado jogador e uma posição inicial
start_game(Player, Pos) ->
    Player ! {start_game, Pos, self()}.

%Início do registo do server, começa com um ListeningSocket para ir gerando um para cada jogador
%Regista dois processos, um como o lobby e outro como o game_manager, depois torna-se no acceptor
server(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [{packet, line}, {reuseaddr, true}]),
    file_manager:start(),
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
%TODO Não se pode fazer pelos values? o online já está a ser dumb e já
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
        {ready, Level, User} ->
            case maps:find(Level, RoomMap) of
                {ok, {User, _}} ->
                    User ! {error_already_ready, game_manager},
                    game_manager(RoomMap, GameControllers);
                {ok, {_, Controller}} ->
                    %juntar-se ao jogo
                    Controller ! {start, User, game_manager},
                    User ! {ok, Controller, game_manager},
                    New_Map = maps:remove(Level, RoomMap),
                    game_manager(New_Map, [Controller | GameControllers]);
                _ ->
                    %criar uma espera
                    Controller = spawn(fun()-> ready([User]) end),
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
ready([FstPlayer]) ->
    receive 
        %Este abort tem de vir de parte do game_manager aquando de um unready
        {abort, game_manager} -> ok;
        %Não pode receber o abort do primeiro jogador porque isso tem de ser testado no sync_up
        %Problemas de concorrência podem fazer com que o jogo comece mas um dos jogadores se desconecte antes de o saber
        {start, SndPlayer, game_manager} ->
            sync_up({FstPlayer, SndPlayer})
    end.

abort_sync(FstPlayer, SndPlayer) ->
    FstPlayer ! {abort, self()},
    SndPlayer ! {abort, self()},
    game_manager ! {end_game, self()}.

sync(FstPlayer, SndPlayer) ->
    FstPlayer ! {sync, self()},
    SndPlayer ! {sync, self()}.

%Função chamada depois da ligação de ambos os jogadores, para começar a Simulação e sintonizar ambos os jogadores
%Se algum for cancelado dentro de um minuto o jogo não ocorre e pontos não são dados
%As simulações são passadas para cada um dos jogadores
sync_up({FstPlayer, SndPlayer}) ->
    %Avisar os utilizadores para entrarem no jogo
    sync(FstPlayer, SndPlayer),
    receive
        {ok, FstPlayer} -> 
            receive
                {ok, SndPlayer} -> 
                    simulation:start_game({FstPlayer, SndPlayer})
                after 15000 -> abort_sync(FstPlayer, SndPlayer)
            end;
        {ok, SndPlayer} -> 
            receive
                {ok, FstPlayer} -> 
                    simulation:start_game({FstPlayer, SndPlayer})
                after 15000 -> abort_sync(FstPlayer, SndPlayer)
            end;
        %1 minuto de espera para conexão parece justo, se não der é preciso avisar do fim do jogo
        {abort, FstPlayer} -> abort_sync(FstPlayer, SndPlayer);
        {abort, SndPlayer} -> abort_sync(FstPlayer, SndPlayer)
        after 15000 -> abort_sync(FstPlayer, SndPlayer)
    end.

% As funções seguintes dizem respeito às funções que ditam o estado dos jogadores
% A função acceptor abre a Socket de ligação com o utilizador
% Os diferentes estados são: Main_Menu; User; Ready_User; Loading; e Player

%TODO como fechar esta LSock no accept?
acceptor(LSock) ->
    case gen_tcp:accept(LSock) of
        {ok, Sock}  ->
            spawn(fun() -> acceptor(LSock) end),
            main_menu(Sock);
        _ -> ok
    end.

%TODO como colocar os menus no lobby, será que pode ser mapa Pid =>?
%Poder fechar estes processos também
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
                        case file_manager:check_level(Username) of
                            {ok, Level} ->  
                                lobby ! {unready, Username, self()},
                                gen_tcp:send(Sock, "login:ok:" ++ integer_to_list(Level) ++ "\n"),
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
            game_manager ! {ready, Level, self()},
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
            gen_tcp:close(Sock);
        _ ->
            user(Sock, Username)
    end.

unready(Username, Game) ->
    Game ! {abort, self()},
    {ok, Level} = file_manager:check_level(Username),
    game_manager ! {unready, Level, self()}.

user_ready(Sock, Game, Username) -> 
    receive
        {sync, Game} -> 
            lobby ! {game, Username, self()},
            %TODO arranjar uma cena melhor
            %Hol' Up
            gen_tcp:send(Sock, "game:h\n"),
            loading(Sock, Game, Username);
        {tcp, _, Data} ->
            case Data of
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
            gen_tcp:close(Sock);
        _ ->
            user_ready(Sock, Game, Username)
    end.

leave_game(Username, Game) ->
    simulation:leave(Game, self()),
    lobby ! {leave, Username, self()}.

loading(Sock, Game, Username) ->
    receive
        {start_game, {{XP, YP, AP}, {XE, YE, AE}}, Game} ->
            %game:start
            gen_tcp:send(Sock, lists:concat(["pos:", XP, ":", YP , ":", AP,
                                "\nposE:", XE, ":", YE, ":", AE, "\ngame:s\n"])),
            player(Sock, Game, Username);
        {abort, Game} ->
            %TODO ver isto com o Carlos
            lobby ! {unready, Username, self()},
            gen_tcp:send(Sock, "game:a\n"),
            user(Sock, Username);
        {tcp_closed, _} -> 
            leave_game(Username, Game);
        {tcp_error, _, _} -> 
            leave_game(Username, Game);
        stop ->
            gen_tcp:close(Sock);
        _ -> 
            loading(Sock, Game, Username)
    end.

player(Sock, Game, Username) ->
    %io:format("player_from\n"),
    receive
        stop -> gen_tcp:close(Sock);
        {victory, Game} -> 
            lobby ! {win, Username, self()},
            {ok, Level} = file_manager:win(Username),
            gen_tcp:send(Sock, "game:w:" ++ integer_to_list(Level) ++ "\n"),
            user(Sock, Username);
        {defeat, Game} -> 
            lobby ! {loss, Username, self()},
            gen_tcp:send(Sock, "game:l\n"),
            user(Sock, Username)
        after 0 ->
            %io:format("player_from_after\n"),
            receive
                stop -> gen_tcp:close(Sock);
                {victory, Game} -> 
                    lobby ! {win, Username, self()},
                    {ok, Level} = file_manager:win(Username),
                    gen_tcp:send(Sock, "game:w:" ++ integer_to_list(Level) ++ "\n"),
                    user(Sock, Username);
                {defeat, Game} -> 
                    lobby ! {loss, Username, self()},
                    gen_tcp:send(Sock, "game:l\n"),
                    user(Sock, Username);
                {golden, Game} -> 
                    gen_tcp:send(Sock, "game:g\n"),
                    player(Sock, Game, Username);
                {positions, {XP, YP, AP}, {XE, YE, AE}, Game} -> 
                    %pos:x:y:alpha
                    %posE:x:y:alpha
                    %io:format("~p ~p ~p, ~p ~p ~p ~n", [XP, YP, AP, XE, YE, AE]),
                    gen_tcp:send(Sock, lists:concat(["pos:", XP, ":", YP , ":", AP,
                                "\nposE:", XE, ":", YE, ":", AE, "\n"])),
                    player(Sock, Game, Username);
                {boxes, Add, Remove, Game} ->
                    %box:+:x:y:color:-:x:y:color
                    %[{x1,y1,color1}, {x2,y2,color2}]
                    %io:format("~p ~p ~n", [Add, Remove]),
                    StrList = string:join(  [lists:concat(["+:", X, ":", Y, ":", C]) || {X,Y,C} <- Add] ++
                                            [lists:concat(["-:", X, ":", Y, ":", C]) || {X,Y,C} <- Remove], ":"),
                    %io:format("~p\n", StrList),
                    gen_tcp:send(Sock, lists:concat(["box:", StrList, "\n"])),
                    player(Sock, Game, Username);
                {score, Player, Enemy, Game} ->
                    gen_tcp:send(Sock, lists:concat(["points:", Player, ":", Enemy, "\n"])),
                    player(Sock, Game, Username);

                {tcp, _, DataN} -> 
                    Data = lists:droplast(DataN),
                    ["move", Left, Front, Right] = string:split(Data, ":", all),
                    %io:format("~p ~p ~p ~n", [Left, Front, Right]),
                    simulation:buttons({Left == "t", Front == "t", Right == "t"}),
                    player(Sock, Game, Username);
                {tcp_closed, _} -> 
                    leave_game(Username, Game);
                {tcp_error, _, _} -> 
                    leave_game(Username, Game);
                _ -> player(Sock, Game, Username)
            end
    end.
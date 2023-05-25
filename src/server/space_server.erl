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
    io:format("~p\n", [WinMap]),
    receive
        {top, 0, From} ->
            From ! {lists:sort(fun({_, V1}, {_, V2}) -> V1 > V2 end, maps:to_list(WinMap)), lobby},
            lobby(Users,WinMap);
        {top, Number, From} ->
            From ! {lists:sublist(lists:sort(fun({_, V1}, {_, V2}) -> V1 > V2 end, maps:to_list(WinMap)), Number), lobby},
            lobby(Users,WinMap);
        {online, From} ->
            From ! {maps:keys(Users), lobby},
            lobby(Users,WinMap);
        {loss, Username, User} ->
            lobby(Users#{Username => {unready, User}}, WinMap);
        {win, Username, User} ->
            io:format("user won ~p ~n", [Username]),
            case maps:find(Username, WinMap) of
                {ok, {_, _, OldWins}} -> Wins = OldWins + 1;
                _ -> Wins = 1
            end,
            %ver se o gajo que ganhou fica no top
            %não mandar para as pessoas a jogar, só unready/ready 
            %lists:map(fun({_, Pid})-> Pid ! {new_win, Username, Wins} end, maps:values(Users)),
            lobby(Users#{Username => {unready, User}}, WinMap#{Username => Wins});
        {enter, Username, User} ->
            io:format("user entered ~p ~n", [Username]),
            %TODO pôr o leaderboard inicial para todos
            lobby(Users#{Username => {unready, User}}, WinMap);
        {unready, Username, User} ->
            io:format("user unready ~p ~n", [Username]),
            lobby(Users#{Username => {unready, User}}, WinMap);
        {ready, Username, User} ->
            io:format("user ready ~p ~n", [Username]),
            lobby(Users#{Username => {ready, User}}, WinMap);
        {game, Username, User} ->
            io:format("user game ~p ~n", [Username]),
            lobby(Users#{Username => {game, User}}, WinMap);
        {leave, Username, User} ->
            io:format("user left ~p ~n", [Username]),
            lobby(maps:remove(Username, Users), WinMap);
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
                {ok, {User, _}} ->
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
                    %TODO simplificar o game_manager para não perder tempo em burocracias
                    %juntar-se ao jogo
                    %Não queremos que o game manager lide com burocracia lenta
                    %Game = spawn(fun() -> sync_up({Other, Othername}, {User, Username}) end),
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
%Ainda não tenho a certeza da necessidade da sua existência, o game_manager pode se calhar dar spawn a um sync_up
%Se calhar é mesmo preciso para receber os aborts a tempo
%Um jogador dá ready. 
%O segundo jogador dá ready e depois o primeiro cancela o jogo, enquanto o ecrã dele ainda não recebeu o ok
%O primeiro tem de poder abortar o jogo, mas esse abort ainda não pode chegar ao sync_up se o game não existir
ready([{FstUsername, FstPlayer}]) ->
    receive 
        {abort, FstPlayer} -> todo;
        %Antes de começar o jogo é preciso verificar se ainda estão vivos os jogadores
        %Problemas de concorrência podem fazer com que o jogo comece mas um dos jogadores se desconecte antes de o saber
        {start, SndUsername, SndPlayer, game_manager} ->
            sync_up({FstUsername, FstPlayer}, {SndUsername, SndPlayer})
    end.

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
                after 60000 -> end_game(FstPlayer, -1, SndPlayer, -1)
            end;
        {ok, FromSnd, SndPlayer} -> 
            receive
                {ok, FromFst, FstPlayer} -> game({FstUsername, FromFst, FstPlayer}, {SndUsername, FromSnd, SndPlayer}, GameSim)
                after 60000 -> end_game(SndPlayer, -1, FstPlayer, -1)
            end;
        %1 minuto de espera para conexão parece justo, se não der é preciso avisar do fim do jogo
        {abort, FstPlayer} -> end_game(SndPlayer, -1, FstPlayer, -1);
        {abort, SndPlayer} -> end_game(FstPlayer, -1, SndPlayer, -1)
        after 60000 -> end_game(SndPlayer, -1, FstPlayer, -1)
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

%Espera do jogo que recebe o final do tempo e também o cancelar dos jogadores.
%Dita os vencedores, chamando a função para marcar pontos, o que pode fazer com que os restantes esperem por correr depois
game({FstUsername, FromFst, ToFst}, {SndUsername, FromSnd, ToSnd}, GameSim) -> 
    receive
        golden ->
            FromFst ! {golden, self()},
            FromSnd ! {golden, self()},
            game({FstUsername, FromFst, ToFst}, {SndUsername, FromSnd, ToSnd}, GameSim);
        stop ->
            end_game(FromFst, -1, FromSnd, -1);
        {positions, FstPositions, SndPositions, _} -> 
            %io:format("Positions\n"),
            positions(FstPositions, SndPositions, FromFst, self()),
            positions(SndPositions, FstPositions, FromSnd, self()),
            game({FstUsername, FromFst, ToFst}, {SndUsername, FromSnd, ToSnd}, GameSim);
        {score, FstScore, SndScore, _} -> 
            %io:format("Scores\n"),
            score(FstScore, SndScore, FromFst, self()),
            score(SndScore, FstScore, FromSnd, self()),
            game({FstUsername, FromFst, ToFst}, {SndUsername, FromSnd, ToSnd}, GameSim);
        {boxes, Add, Remove, _} ->
            %io:format("Boxes\n"), 
            boxes(Add, Remove, FromFst, self()),
            boxes(Add, Remove, FromSnd, self()),
            game({FstUsername, FromFst, ToFst}, {SndUsername, FromSnd, ToSnd}, GameSim);
        %Recebe o jogador perdedor
        {abort, Player} ->
            case Player of
                p1 -> 
                    WinnerUsername = SndUsername, LoserUsername = FstUsername,
                    WinnerFrom = FromSnd, LoserFrom = FromFst;
                ToFst -> 
                    WinnerUsername = SndUsername, LoserUsername = FstUsername,
                    WinnerFrom = FromSnd, LoserFrom = FromFst;
                ToSnd ->
                    WinnerUsername = FstUsername, LoserUsername = SndUsername,
                    WinnerFrom = FromFst, LoserFrom = FromSnd;
                p2 -> 
                    WinnerUsername = FstUsername, LoserUsername = SndUsername,
                    WinnerFrom = FromFst, LoserFrom = FromSnd
            end,
            {ok, WinnerLevel, LoserLevel} = file_manager:end_game(WinnerUsername, LoserUsername),
            end_game(WinnerFrom,WinnerLevel,LoserFrom,LoserLevel),
            GameSim ! {stop, self()}
            %pontuar o outro jogador
            %Só precisava de enviar o vencedor porque é o único que importa mas pode ser que possa estar inválido
        %TODO Testar se o fim do jogo ocorreu mesmo, continuar até ao próximo ponto, terminar o jogo e marcar pontos
        %TODO Provavelmente vai ser preciso que a simulação conheça o jogo que a está a correr, para comunicar este fim especial
        %Talvez esta indireção (este game aqui) seja inútil e os aborts possam existir diretamente no game da simulação
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
            io:format("~p\n", [lists:foldl(fun({U, W}, Acc) -> lists:concat([Acc, U, "_", W, ":"]) end, "top:", List) ++ "\n"]),
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
                    gen_tcp:send(Sock, "close:ok");

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
            gen_tcp:send(Sock, "server:closed\n")
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
            gen_tcp:send(Sock, "server:closed\n")
    end.

player_fromsim(Sock, Game, ToSim) ->
    %io:format("player_from\n"),
    receive
        {victory, Level, Game} -> 
            ToSim ! {abort, self()},
            gen_tcp:send(Sock, "game:w:" ++ integer_to_list(Level) ++ "\n");
        {defeat, Level, Game} -> 
            ToSim ! {abort, self()},
            gen_tcp:send(Sock, "game:l\n")
        after 0 ->
            %io:format("player_from_after\n"),
            receive
                {victory, Level, Game} -> 
                    ToSim ! {abort, win, self()},
                    gen_tcp:send(Sock, "game:w:" ++ integer_to_list(Level) ++ "\n");
                {defeat, Level, Game} -> 
                    ToSim ! {abort, loss, self()},
                    gen_tcp:send(Sock, "game:l\n");
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

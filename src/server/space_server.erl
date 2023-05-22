-module(space_server).
-export([start/1, stop/0, abort_game/2, positions/4, boxes/4, score/4]). % server:start(1234)
                            % nc localhost 1234
                            % netstat -p tcp -an | grep 1234
-define(GAMETIME, 120000).

start(Port) -> register(?MODULE, spawn(fun() -> server(Port) end)), ok.

stop() -> ?MODULE ! stop.

%Início do registo do server, começa com um ListeningSocket para ir gerando um para cada jogador
%Regista dois processos, um como o lobby e outro como o game_manager, depois torna-se no acceptor
server(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [{packet, line}, {reuseaddr, true}]),
    register(lobby, spawn(fun()-> lobby([]) end)),
    register(game_manager, spawn(fun() -> game_manager(#{}, []) end)),
    spawn(fun() -> file_manager:start() end),
    spawn(fun() -> acceptor(LSock) end),
    receive stop -> 
        file_manager:stop(),
        lobby ! stop,
        game_manager ! stop
    end.

%Lobby como sala tirada diretamente das salas definidas nas aulas práticas
lobby(Users) ->
    receive
        {enter, User} ->
            io:format("user entered ~p ~n", [User]),
            lobby([User | Users]);
        {line, Data} = Msg ->
            io:format("received  ~p ~n", [Data]),
            [User ! Msg || User <- Users],
            lobby(Users);
        {leave, User} ->
            io:format("user left ~n", []),
            lobby(Users -- [User]);
        stop -> 
            [User ! stop || User <- Users]
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
    {Player1Sim, Player2Sim} = simulation:start_game(self()),
    FstPlayer ! {start_game, Player1Sim, self()},
    SndPlayer ! {start_game, Player2Sim, self()},
    receive
        {ok, FstPlayer} -> 
            receive
                {ok, SndPlayer} -> game([{FstUsername, FstPlayer}, {SndUsername, SndPlayer}])
                after 60000 -> end_game(FstPlayer, -1, SndPlayer, -1)
            end;
        {ok, SndPlayer} -> 
            receive
                {ok, FstPlayer} -> game([{FstUsername, FstPlayer}, {SndUsername, SndPlayer}])
                after 60000 -> end_game(SndPlayer, -1, FstPlayer, -1)
            end;
        %1 minuto de espera para conexão parece justo, se não der é preciso avisar do fim do jogo
        {abort, FstPlayer} -> end_game(SndPlayer, -1, FstPlayer, -1);
        {abort, SndPlayer} -> end_game(FstPlayer, -1, SndPlayer, -1)
        after 60000 -> end_game(SndPlayer, -1, FstPlayer, -1)
    end.


%O Controller é a quem é enviada a mensagem (Controlador do jogo do lado do servidor)

%Termina o jogo graciosamente, dado um vencedor
abort_game(Controller, Winner) ->
    Controller ! {abort, Winner}.

%Recebe as posições da simulação em dois tuplos
%{xp,yp,ap}, {xe,ye,ap}
positions(FstPositions, SndPositions, Controller, Game) ->
    Controller ! {positions, FstPositions, SndPositions, Game}.

%Recebe as posições a adicionar e remover das caixas em listas de listas
%[[x1,y1,color1],[x2,y2,color2]]
boxes(Add, Remove, Controller, Game) ->
    Controller ! {boxes, Add, Remove, Game}.

%Recebe a pontuação de ambos os jogadores
score(FstPoints, SndPoints, Controller, Game) ->
    Controller ! {score, FstPoints, SndPoints, Game}.

%Espera do jogo que recebe o final do tempo e também o cancelar dos jogadores.
%Dita os vencedores, chamando a função para marcar pontos, o que pode fazer com que os restantes esperem por correr depois
game([{FstUsername, FstPlayer}, {SndUsername, SndPlayer}]) -> 
    receive
        stop ->
            end_game(FstPlayer, -1, SndPlayer, -1);
        {positions, FstPositions, SndPositions, _} -> 
            io:format("Positions\n"),
            positions(FstPositions, SndPositions, FstPlayer, self()),
            positions(SndPositions, FstPositions, SndPlayer, self()),
            game([{FstUsername, FstPlayer}, {SndUsername, SndPlayer}]);
        {score, FstScore, SndScore, _} -> 
            io:format("Scores\n"),
            score(FstScore, SndScore, FstPlayer, self()),
            score(SndScore, FstScore, SndPlayer, self()),
            game([{FstUsername, FstPlayer}, {SndUsername, SndPlayer}]);
        {boxes, Add, Remove, _} ->
            io:format("Boxes\n"), 
            boxes(Add, Remove, FstPlayer, self()),
            boxes(Add, Remove, SndPlayer, self()),
            game([{FstUsername, FstPlayer}, {SndUsername, SndPlayer}]);
        {abort, Player} ->
            case Player of
                FstPlayer -> 
                    {ok, WinnerLevel, LoserLevel} = file_manager:end_game(SndUsername, FstUsername),
                    end_game(SndPlayer,WinnerLevel,FstPlayer,LoserLevel);
                SndPlayer ->
                    {ok, WinnerLevel, LoserLevel} = file_manager:end_game(FstUsername, SndUsername),
                    end_game(FstPlayer, WinnerLevel, SndPlayer, LoserLevel)
            end
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
            [Username, Password] = re:split(Data, "[:]"),
            case file_manager:create_account(Username, Password) of
                ok -> gen_tcp:send(Sock, "register:ok\n");
                user_exists -> gen_tcp:send(Sock, "register:user_exists\n")
            end,
            main_menu(Sock);
        {tcp, _, "login:" ++ DataN} -> 
            Data = lists:droplast(DataN),
            [Username, Password] = re:split(Data, "[:]"),
            io:format("~p", [Password]),
            case file_manager:login(Username, Password) of
                ok ->
                    lobby ! {enter, "lobby", self()},
                    case file_manager:check_level(Username) of
                        {ok, Level} ->  gen_tcp:send(Sock, "login:ok:" ++ integer_to_list(Level) ++ "\n"),
                                        user(Sock, Username);
                        {invalid_user, _} -> gen_tcp:send(Sock, "login:invalid_user\n");
                        _ -> gen_tcp:send(Sock, "login:error\n")
                    end;
                invalid_password ->
                    gen_tcp:send(Sock, "login:invalid_password\n"),
                    main_menu(Sock);
                _ ->
                    gen_tcp:send(Sock, "login:unknown_username\n"),
                    main_menu(Sock)
            end;
        {tcp_error, _, _} -> ok;
        {tcp_closed, _, _} -> ok;
        _ -> gen_tcp:send(Sock, "login:unknown_command\n")
    end.

user(Sock, Username) ->
    receive
        {line, Data} ->
            gen_tcp:send(Sock, "text:" ++ Data),
            user(Sock, Username);
        {tcp, _, Data} ->
            case Data of
                "logout:\n" -> 
                    lobby ! {leave, self()},
                    gen_tcp:send(Sock, "logout:ok\n"),
                    main_menu(Sock);
                "close:" ++ DataN -> 
                    Data = lists:droplast(DataN),
                    %Data = lists:droplast(DataN),
                    case file_manager:close_account(Username, Data) of
                        ok ->
                            lobby ! {leave, self()},
                            gen_tcp:send(Sock, "close:ok");

                        wrong_password -> gen_tcp:send(Sock, "close:error_wrong_password\n"), user(Sock, Username);
                        invalid -> gen_tcp:send(Sock, "close:error_invalid\n"), user(Sock, Username)
                    end;
                "ready:true\n" ->
                    {ok, Level} = file_manager:check_level(Username),
                    game_manager ! {ready, Level, Username, self()},
                    receive 
                        {ok, Game, game_manager} -> gen_tcp:send(Sock, "ready:ok\n"), user_ready(Sock, Game, Username)
                        %{error_already_ready, game_manager} -> gen_tcp:send(Sock, "game:error_already_ready"), user(Sock, Room, Username)
                    end;
                _ -> 
                    lobby ! {line, Data},
                    user(Sock, Username)
            end;
        {tcp_closed, _} ->
            lobby ! {leave, self()};
        {tcp_error, _, _} ->
            lobby ! {leave, self()};
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
            lobby ! {leave, self()},
            Game ! {ok, self()},
            %game:start
            gen_tcp:send(Sock, "game:s\n"),
            ToSim = spawn(fun() -> player_tosim(Sock, Game, Simulation, Username, self()) end),
            player_fromsim(Sock, Game, Simulation, Username, ToSim);
        {tcp, _, Data} ->
            case Data of
                "logout:\n" -> 
                    unready(Username, Game),
                    lobby ! {leave, self()},
                    gen_tcp:send(Sock, "logout:ok\n"),
                    main_menu(Sock);
                "close:" ++ PasswdN -> 
                    Passwd = lists:droplast(PasswdN),
                    case file_manager:close_account(Username, Passwd) of
                        ok -> 
                            unready(Username, Game),
                            lobby ! {leave, self()},
                            gen_tcp:send(Sock, "close:ok\n");

                        wrong_password -> gen_tcp:send(Sock, "close:error_wrong_password\n"), user_ready(Sock, Game, Username);
                        invalid -> gen_tcp:send(Sock, "close:error_invalid\n"), user_ready(Sock, Game, Username)
                    end;
                "ready:false\n" -> 
                    unready(Username, Game),
                    gen_tcp:send(Sock, "ready:ok\n"), user(Sock,  Username);
                "msg:" ++ Data -> 
                    lobby ! {line, Data},
                    user_ready(Sock, Game, Username)
            end;
        {tcp_closed, _} ->
            unready(Username, Game),
            lobby ! {leave, self()};
        {tcp_error, _, _} ->
            unready(Username, Game),
            lobby ! {leave, self()};
        stop ->
            gen_tcp:send(Sock, "server:closed\n")
    end.

%Filho direto do processo utilizador, o ToSim é o outro processo que deve ser terminado no fim 
player_fromsim(Sock, Game, Simulation, Username, ToSim) ->
    io:format("player_from\n"),
    receive
        {victory, Level, Game} -> 
            ToSim ! {abort, self()},
            gen_tcp:send(Sock, "game:w:" ++ integer_to_list(Level) ++ "\n"),
            user(Sock, Username);
        {defeat, Level, Game} -> 
            ToSim ! {abort, self()},
            gen_tcp:send(Sock, "game:l\n"),
            user(Sock, Username)
        after 0 ->
            io:format("player_from_after\n"),
            receive
                {victory, Level, Game} -> 
                    ToSim ! {abort, self()},
                    gen_tcp:send(Sock, "game:w:" ++ integer_to_list(Level) ++ "\n"),
                    user(Sock, Username);
                {defeat, Level, Game} -> 
                    ToSim ! {abort, self()},
                    gen_tcp:send(Sock, "game:l\n"),
                    user(Sock, Username);
                {positions, {XP, YP, AP}, {XE, YE, AE}, Game} -> 
                    %pos:x:y:alpha
                    %posE:x:y:alpha
                    io:format("~p ~p ~p, ~p ~p ~p ~n", [XP, YP, AP, XE, YE, AE]),
                    gen_tcp:send(Sock, "game:" ++ integer_to_list(XP) ++ ":" ++ integer_to_list(YP) ++ ":" ++ integer_to_list(AP) ++
                                ":posE:" ++ integer_to_list(XE) ++ ":" ++ integer_to_list(YE) ++ ":" ++ integer_to_list(AE) ++ "\n"),
                    player_fromsim(Sock, Game, Simulation, Username, ToSim);
                {boxes, Add, Remove, Game} ->
                    %box:+:x:y:color
                    %box:-:x:y:color
                    %Add e Remove são listas com listas dos elementos 
                    %[[x1,y1,color1], [x2,y2,color2]]
                    io:format("~p ~p ~n", [Add, Remove]),
                    StrAddList = [string:join(["+" | A], ":") || A <- Add],
                    StrRemoveList = [string:join(["-" | R], ":") || R <- Remove],
                    gen_tcp:send(Sock, "box:" ++ string:join(StrAddList, ":") ++ ":" ++ string:join(StrRemoveList, ":") ++ "\n"),
                    player_fromsim(Sock, Game, Simulation, Username, ToSim);
                {score, Player, Enemy, Game} ->
                    gen_tcp:send(Sock, "points:" ++ integer_to_list(Player) ++ ":" ++ integer_to_list(Enemy) ++ "\n"),
                    player_fromsim(Sock, Game, Simulation, Username, ToSim)
            end
    end.

%rotate
%speed_up
%telogo

player_tosim(Sock, Game, Simulation, Username, FromSim) -> 
    io:format("player_to\n"),
    receive
        {abort, FromSim} ->
            ok;
        {tcp, _, DataN} -> 
            Data = lists:droplast(DataN),
            ["move", Left, Front, Right] = re:split(Data, "[:]"),
            io:format("~p ~n", [Data]),
            if Left =:= "t", Right =:= "f" -> 
                simulation:change_angle(Simulation,1)
            end,
            if Front =:= "t" -> 
                simulation:change_speed(Simulation)
            end,
            if Left =:= "f", Right =:= "t" -> 
                simulation:change_angle(Simulation,-1)
            end,
            player_tosim(Sock, Game, Simulation, Username, FromSim);
        {tcp_closed, _} -> ok;
        {tcp_error, _, _} -> ok;
        Wat -> 
            io:format("~p", Wat),
            player_tosim(Sock, Game, Simulation, Username, FromSim)
    end.

-module(space_server).
-export([start/1, stop/0]). % server:start(1234)
                            % nc localhost 1234
                            % netstat -p tcp -an | grep 1234
-define(GAMETIME, 120000).

start(Port) -> register(?MODULE, spawn(fun() -> server(Port) end)).

stop() -> ?MODULE ! stop.

%Início do registo do server, começa com um ListeningSocket para ir gerando um para cada jogador
%Regista dois processos, um como o lobby e outro como o game_manager, depois torna-se no acceptor
server(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [{packet, line}, {reuseaddr, true}]),
    register(lobby, spawn(fun()-> lobby([]) end)),
    register(game_manager, spawn(fun() -> game_manager(#{}, []) end)),
    spawn(fun() -> acceptor(LSock) end),
    receive stop -> ok end.

%Lobby como sala tirada diretamente das salas definidas nas aulas práticas
lobby(Users) ->
    receive
        {enter, User} ->
            io:format("user entered ~p ~n", []),
            lobby([User | Users]);
        {line, Data} = Msg ->
            io:format("received  ~p ~n", [Data]),
            [User ! Msg || User <- Users],
            lobby(Users);
        {leave, User} ->
            io:format("user left ~n", []),
            lobby(Users -- [User])
    end.  

%Gestor dos jogos
%RoomMap é um mapa que associa níveis a jogos, que é limpo sempre que um jogo de um dado nível começa
%GameRooms contém todas as salas de jogos em andamento
%A junção de GameRooms com o Lobby dá todos os jogadores atualmente online
game_manager(RoomMap, GameRooms) ->
    receive
        {unready, Level, User} -> 
            case maps:find(Level, RoomMap) of
                {ok, {_, User}} ->
                    New_Map = maps:remove(Level, RoomMap);
                _ ->
                    %Se alguém estiver ready e não for este não se faz nada?
                    New_Map = RoomMap,
                    User ! {error_not_ready, game_manager}    
            end,
            game_manager(New_Map, GameRooms);
        {ready, Level, Username, User} ->
            case maps:find(Level, RoomMap) of
                {ok, {User, _}} ->
                    User ! {error_already_ready, game_manager},
                    game_manager(RoomMap, GameRooms);
                {ok, {_, Game}} ->
                    %TODO simplificar o game_manager para não perder tempo em burocracias
                    %juntar-se ao jogo
                    %Não queremos que o game manager lide com burocracia lenta
                    User ! {ok, Game, game_manager},
                    Game ! {start, Username, User, game_manager},
                    New_Map = maps:remove(Level, RoomMap),
                    game_manager(New_Map, [Game | GameRooms]);
                _ ->
                    %criar uma espera
                    Game = spawn(fun()-> ready([{User, Username}]) end),
                    User ! {ok, Game, game_manager},
                    game_manager(RoomMap#{Level => Game}, GameRooms)
            end;
        {end_game, Game} ->
            game_manager(RoomMap, GameRooms -- [Game])
    end.

sync_up({FstPlayer, FstUsername}, {SndPlayer, SndUsername}) ->
    %Avisar os utilizadores para entrarem no jogo
    {Player1Sim, Player2Sim, Game} = simulation:start_game(),
    FstPlayer ! {start_game, Player1Sim, self()},
    SndPlayer ! {start_game, Player2Sim, self()},
    receive
        %sei lá
        {ok, FstPlayer} -> 
            receive
                {ok, SndPlayer} -> game([{FstUsername, FstPlayer}, {SndUsername, SndPlayer}], Game)
                after 6000 -> 
                    game_manager ! {end_game, self()},
                    FstPlayer ! {end_game, self()}
            end;
        {ok, SndPlayer} -> 
            receive
                {ok, SndPlayer} -> game([{FstUsername, FstPlayer}, {SndUsername, SndPlayer}], Game)
                after 6000 -> 
                    game_manager ! {end_game, self()},
                    SndPlayer ! {end_game, self()}
            end;
        %1 minuto de espera para conexão parece justo, se não der é preciso avisar do fim do jogo
        {abort, FstPlayer} -> ok
        after 6000 -> 
            game_manager ! {end_game, self()},
            SndPlayer ! {end_game, self()}
    end.

ready([{FstUsername, FstPlayer}]) ->
    receive 
        {abort, FstPlayer} -> 
            game_manager ! {end_game, self()};
        %Antes de começar o jogo é preciso verificar se ainda estão vivos os jogadores
        %Problemas de concorrência podem fazer com que o jogo comece mas um dos jogadores se desconecte antes de o saber
        {start, SndUsername, SndPlayer, game_manager} ->
            sync_up({FstUsername, FstPlayer}, {SndUsername, SndPlayer})
    end.

game([{FstUsername, FstPlayer}, {SndUsername, SndPlayer}], Game) -> 
    receive
        {abort, FstPlayer} ->
            %pontuar o outro jogador
            %Só precisava de enviar o vencedor porque é o único que importa mas pode ser que possa estar inválido
            end_game({SndUsername, SndPlayer}, {FstUsername, FstPlayer});

        {abort, SndPlayer} -> 
            end_game({FstUsername, FstPlayer}, {SndUsername, SndPlayer})

        after ?GAMETIME ->
            end_game({FstUsername, FstPlayer}, {SndUsername, SndPlayer})
    end.

end_game({WinnerName, Winner}, {LoserName, Loser}) ->
    Winner ! {end_game, self()},
    Loser ! {end_game, self()},
    {ok, WinnerLevel, LoserLevel} = level_manager:end_game(WinnerName, LoserName),
    game_manager ! {end_game, self()}.

main_menu(Sock) ->
    receive
        {tcp, _, "create:" ++ Data} ->
            [Username, Password] = re:split(Data, "[:]"),
            case level_manager:create_account(Username, Password) of
                ok -> gen_tcp:send(Sock, "create:ok");
                user_exists -> gen_tcp:send(Sock, "create:error_user_exists")
            end,
            main_menu(Sock);
        {tcp, _, "login:" ++ Data} -> 
            [Username, Password] = re:split(Data, "[:]"),
            case level_manager:login(Username, Password) of
                ok ->
                    lobby ! {enter, "lobby", self()},
                    gen_tcp:send(Sock, "login:ok"),
                    user(Sock, Username);
                invalid_password ->
                    gen_tcp:send(Sock, "login:error_invalid_password"),
                    main_menu(Sock);
                _ ->
                    gen_tcp:send(Sock, "login:error_unknown_username"),
                    main_menu(Sock)
            end;
        {tcp_error, _, _} -> ok;
        {tcp_closed, _, _} -> ok;
        _ -> gen_tcp:send(Sock, "login:error_unknown_command")
    end.

acceptor(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock) end),
    main_menu(Sock).

user(Sock, Username) ->
    receive
        {line, Data} ->
            gen_tcp:send(Sock, "text:" ++ Data),
            user(Sock, Username);
        {tcp, _, Data} ->
            case Data of
                "logout" -> 
                    lobby ! {leave, self()},
                    gen_tcp:send(Sock, "logout:ok");
                "close:" ++ Data -> 
                    case level_manager:close_account(Username, Data) of
                        ok ->
                            lobby ! {leave, self()},
                            gen_tcp:send(Sock, "close:ok");

                        wrong_password -> gen_tcp:send(Sock, "close:error_wrong_password"), user(Sock, Username);
                        invalid -> gen_tcp:send(Sock, "close:error_invalid"), user(Sock, Username)
                    end;
                "game:ready" ->
                    {ok, Level} = level_manager:check_level(Username),
                    game_manager ! {ready, Level, Username, self()},
                    receive 
                        {ok, Game, game_manager} -> gen_tcp:send(Sock, "game:ready"), user_ready(Sock, Game, Username)
                        %{error_already_ready, game_manager} -> gen_tcp:send(Sock, "game:error_already_ready"), user(Sock, Room, Username)
                    end;
                _ -> 
                    lobby ! {line, Data},
                    user(Sock, Username)
            end;
        {tcp_closed, _} ->
            lobby ! {leave, self()};
        {tcp_error, _, _} ->
            lobby ! {leave, self()}
    end.

unready(Username, Game) ->
    Game ! {abort, self()},
    {ok, Level} = level_manager:check_level(Username),
    game_manager ! {unready, Level, self()}.

user_ready(Sock, Game, Username) -> 
    receive
        {start_game, Simulation, Game} ->
            lobby ! {leave, self()},
            gen_tcp:send(Sock, "game:start"),
            player(Sock, Game, Simulation, Username);
        {tcp, _, Data} ->
            case Data of
                "logout" -> 
                    unready(Username, Game),
                    lobby ! {leave, self()},
                    gen_tcp:send(Sock, "logout:ok");
                "close:" ++ Data -> 
                    case level_manager:close_account(Username, Data) of
                        ok -> 
                            unready(Username, Game),
                            lobby ! {leave, self()},
                            gen_tcp:send(Sock, "close:ok");

                        wrong_password -> gen_tcp:send(Sock, "close:error_wrong_password"), user_ready(Sock, Game, Username);
                        invalid -> gen_tcp:send(Sock, "close:error_invalid"), user_ready(Sock, Game, Username)
                    end;
                "game:unready" -> 
                    unready(Username, Game),
                    gen_tcp:send(Sock, "game:unready"), user(Sock,  Username);
                "msg:" ++ Data -> 
                    lobby ! {line, Data},
                    user_ready(Sock, Game, Username)
            end;
        {tcp_closed, _} ->
            unready(Username, Game),
            lobby ! {leave, self()};
        {tcp_error, _, _} ->
            unready(Username, Game),
            lobby ! {leave, self()}
    end.

player(Sock, Game, Simulation, Username) -> 
    receive
        {end_game, Game} -> user(Sock, Username);
        {tcp, _, Data} -> ok;
        {tcp_closed, _} -> ok;
        {tcp_error, _, _} -> ok;
        _ -> player(Sock, Game, Simulation, Username)
    end.
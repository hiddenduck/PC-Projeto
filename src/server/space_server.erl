-module(space_server).
-export([start/1, stop/0, abort_game/2, positions/4]). % server:start(1234)
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
%RoomMap é um mapa que associa níveis ao jogador que o começou e ao jogo, que é limpo sempre que um jogo de um dado nível começa
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
                    %Game = spawn(fun() -> sync_up({Other, Othername}, {User, Username}) end),
                    Game ! {start, Username, User, game_manager},
                    User ! {ok, Game, game_manager},
                    New_Map = maps:remove(Level, RoomMap),
                    game_manager(New_Map, [Game | GameRooms]);
                _ ->
                    %criar uma espera
                    Game = spawn(fun()-> ready([{User, Username}]) end),
                    User ! {ok, Game, game_manager},
                    game_manager(RoomMap#{Level => {User, Game}}, GameRooms)
            end;
        {end_game, Game} ->
            game_manager(RoomMap, GameRooms -- [Game])
    end.

%Função de espera que correrá depois da chamada ready de um utilizador
%Ainda não tenho a certeza da necessidade da sua existência, o game_manager pode se calhar dar spawn a um sync_up
%Se calhar é mesmo preciso para receber os aborts a tempo
%Um jogador dá ready. 
%O segundo jogador dá ready e depois o primeiro cancela o jogo, enquanto o ecrã dele ainda não recebeu o ok
%O primeiro tem de poder abortar o jogo, mas esse abort ainda não pode chegar ao sync_up se o game não existir
ready([{FstUsername, FstPlayer}]) ->
    receive 
        {abort, FstPlayer} -> 
            game_manager ! {end_game, self()};
        %Antes de começar o jogo é preciso verificar se ainda estão vivos os jogadores
        %Problemas de concorrência podem fazer com que o jogo comece mas um dos jogadores se desconecte antes de o saber
        {start, SndUsername, SndPlayer, game_manager} ->
            sync_up({FstUsername, FstPlayer}, {SndUsername, SndPlayer})
    end.

%Função chamada depois da ligação de ambos os jogadores, para começar a Simulação e sintonizar ambos os jogadores
%Se algum for cancelado dentro de um minuto o jogo não ocorre e pontos não são dados
%As simulações são passadas para cada um dos jogadores
sync_up({FstPlayer, FstUsername}, {SndPlayer, SndUsername}) ->
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
        {abort, SndPlayer } -> end_game(FstPlayer, -1, SndPlayer, -1)
        after 60000 -> end_game(SndPlayer, -1, FstPlayer, -1)
    end.

abort_game(Game, Winner) ->
    Game ! {abort, Winner}.

%Espera do jogo que recebe o final do tempo e também o cancelar dos jogadores.
%Dita os vencedores, chamando a função para marcar pontos, o que pode fazer com que os restantes esperem por correr depois
game([{FstUsername, FstPlayer}, {SndUsername, SndPlayer}]) -> 
    receive
        {abort, Player} ->
            case Player of
                FstPlayer -> 
                    {ok, WinnerLevel, LoserLevel} = level_manager:end_game(SndUsername, FstUsername),
                    end_game(SndPlayer,WinnerLevel,FstPlayer,LoserLevel);
                SndPlayer ->
                    {ok, WinnerLevel, LoserLevel} = level_manager:end_game(FstUsername, SndUsername),
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
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock) end),
    main_menu(Sock).

main_menu(Sock) ->
    receive
        {tcp, _, "register:" ++ Data} ->
            [Username, Password] = re:split(Data, "[:]"),
            case level_manager:create_account(Username, Password) of
                ok -> gen_tcp:send(Sock, "register:ok");
                user_exists -> gen_tcp:send(Sock, "register:user_exists")
            end,
            main_menu(Sock);
        {tcp, _, "login:" ++ Data} -> 
            [Username, Password] = re:split(Data, "[:]"),
            case level_manager:login(Username, Password) of
                ok ->
                    lobby ! {enter, "lobby", self()},
                    {ok, Level} = file_manager:check_level(Username),
                    gen_tcp:send(Sock, "login:ok:" ++ integer_to_list(Level)),
                    user(Sock, Username);
                invalid_password ->
                    gen_tcp:send(Sock, "login:invalid_password"),
                    main_menu(Sock);
                _ ->
                    gen_tcp:send(Sock, "login:unknown_username"),
                    main_menu(Sock)
            end;
        {tcp_error, _, _} -> ok;
        {tcp_closed, _, _} -> ok;
        _ -> gen_tcp:send(Sock, "login:unknown_command")
    end.

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
                "ready:true" ->
                    {ok, Level} = level_manager:check_level(Username),
                    game_manager ! {ready, Level, Username, self()},
                    receive 
                        {ok, Game, game_manager} -> gen_tcp:send(Sock, "ready:ok"), user_ready(Sock, Game, Username)
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
            %game:start
            gen_tcp:send(Sock, "game:s"),
            Reader = spawn(fun() -> player_tosim(Sock, Game, Simulation, Username, self()) end),
            player_fromsim(Sock, Game, Simulation, Username, Reader);
        {tcp, _, Data} ->
            case Data of
                "logout" -> 
                    unready(Username, Game),
                    lobby ! {leave, self()},
                    gen_tcp:send(Sock, "logout:ok");
                "close:" ++ Passwd -> 
                    case level_manager:close_account(Username, Passwd) of
                        ok -> 
                            unready(Username, Game),
                            lobby ! {leave, self()},
                            gen_tcp:send(Sock, "close:ok");

                        wrong_password -> gen_tcp:send(Sock, "close:error_wrong_password"), user_ready(Sock, Game, Username);
                        invalid -> gen_tcp:send(Sock, "close:error_invalid"), user_ready(Sock, Game, Username)
                    end;
                "ready:false" -> 
                    unready(Username, Game),
                    gen_tcp:send(Sock, "ready:ok"), user(Sock,  Username);
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

%Player position: x,y,alpha
%Inimigo position: x,y,alpha
%Boxs
%Pontuação
%p_p:5
%e_p:5

positions(Player, Enemy, To, Game) ->
    To ! {positions, Player, Enemy, Game}.

player_fromsim(Sock, Game, Simulation, Username, ToSim) ->
    receive
        {victory, Level, Game} -> 
            gen_tcp:send(Sock, "game:w:" ++ integer_to_list(Level)),
            user(Sock, Username);
        {defeat, Level, Game} -> 
            gen_tcp:send(Sock, "game:l"),
            user(Sock, Username)
        after 0 ->
            receive
                {positions, Player, Enemy, Game} -> 
                    %pos:x:y:alpha
                    %posE:x:y:alpha
                    gen_tcp:send(Sock, "game:"),
                    player_fromsim(Sock, Game, Simulation, Username, ToSim);
                {boxes, Add, Remove, Game} ->
                    %box:+:x:y:color
                    %box:-:x:y:color
                    Str = 
                    gen_tcp:send(Sock, "box:"),
                    player_fromsim(Sock, Game, Simulation, Username, ToSim);
                {score, Player, Enemy, Game} ->
                    %box:+:x:y:color
                    %box:-:x:y:color
                    gen_tcp:send(Sock, "box:"),
                    player_fromsim(Sock, Game, Simulation, Username, ToSim);
            end
    end.

%rotate
%speed_up
%telogo

player_tosim(Sock, Game, Simulation, Username, FromSim) -> 
    receive
        {abort, FromSim} ->
            ok;
        {tcp, _, Data} -> 
            ["move", Left, Front, Right] = re:split(Data, "[:]"),
            if Left =:= "t", Right =:= "f" -> 
                simulation:change_angle(Simulation,1)
            end,
            if Front =:= "t" -> 
                simulation:change_speed(Simulation)
            end,
            if Left =:= "f", Right =:= "t" -> 
                simulation:change_angle(Simulation,-1)
            end;
        {tcp_closed, _} -> ok;
        {tcp_error, _, _} -> ok;
        _ -> player_tosim(Sock, Game, Simulation, Username, FromSim)
    end.
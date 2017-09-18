-module(mnesis_acceptor).

-export([start_link/1, stop/0]).

start_link([Num, Port]) ->
    {ok, _} = ranch:start_listener(
        ?MODULE,
        Num,
        ranch_tcp,
        [{port, Port}, {max_connections, infinity}],
        mnesis_interface,
        []
    ).

stop() ->
    ranch:stop_listener(?MODULE).
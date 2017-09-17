-module(mnesis_interface_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    
%% supervisor.

init([]) ->
    %RanchSupSpec = {ranch_sup, {ranch_sup, start_link, []},
    %permanent, 5000, supervisor, [ranch_sup]},
    %ListenerSpec = ranch:child_spec(?MODULE, Num,
    %    ranch_tcp, [{port, Port}, {max_connections, infinity}],
    %    mnesis_interface, []
    %),
    %{ok, {{one_for_one, 10, 10}, [RanchSupSpec, ListenerSpec]}}.    
	{ok, {{one_for_one, 10, 10}, []}}.
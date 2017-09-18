-module(mnesis_interface_sup).
-behaviour(supervisor).

%% API.
-export([start_link/1]).

%% supervisor.
-export([init/1]).

%% API.

%-spec start_link(N::integer(), P::integer()) -> {ok, pid()}.
start_link([]) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    
%% supervisor.

init([_Num, _Port]) ->
    %RanchSupSpec = {ranch_sup, {ranch_sup, start_link, []},
    %permanent, 5000, supervisor, [ranch_sup]},
    %ListenerSpec = ranch:child_spec(?MODULE, Num,
    %    ranch_tcp, [{port, Port}, {max_connections, infinity}],
    %    mnesis_interface, []
    %),
    %{ok, {{one_for_one, 10, 10}, [RanchSupSpec, ListenerSpec]}}.
    %Procs = [
    %{mnesis_acceptor, {mnesis_acceptor, start_link, [Num, Port]},
    %    permanent, 5000, worker, [mnesis_acceptor]}
    %],
    %{ok, {{one_for_one, 1, 5}, Procs}}.
	{ok, {{one_for_one, 10, 10}, []}}.
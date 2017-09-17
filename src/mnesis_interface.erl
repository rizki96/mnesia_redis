-module(mnesis_interface).
-behaviour(gen_server).
-behaviour(ranch_protocol).

-include("mnesis_operation.hrl").

-export([start_link/4, init/4]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(TIMEOUT, 5000).

%-record(state, {socket, transport}).
-record(state, {
    socket :: inet:socket(),
    peername :: {inet:ip_address(), non_neg_integer()},
    transport :: module(),
    database = mnesis_mnesia_table0 :: atom(),
    trans = false :: boolean(),
    error = false :: boolean(),
    optlist = []:: [{atom(), integer(), [binary()]}]
}).

start_link(Ref, Socket, Transport, Opts) ->
	Pid = proc_lib:spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
	{ok, Pid}.

init([]) -> {ok, undefined}.

-spec init(ranch:ref(), inet:socket(), module(), list()) -> ok.
init(Ref, Socket, Transport, _Opts) ->
	ok = ranch:accept_ack(Ref),
	% using gen_server
	ok = Transport:setopts(Socket, [{active, once}]),
	mnesis_opt:init_mnesis(),
	gen_server:enter_loop(?MODULE, [],
	#state{socket=Socket, peername=inet:peername(Socket), transport=Transport},
	?TIMEOUT).
	% using blocking server
	%mnesis_operation:enter_loop(Socket, inet:peername(Socket), Transport).

handle_info({tcp, Socket, Data}, _State=#state{
	socket=Socket, peername=Peername, transport=Transport})
	when byte_size(Data) > 1 ->
		{_Reply, NewState} = mnesis_operation:no_loop(Data, #state{socket=Socket, peername=Peername, transport=Transport}),
		{noreply, NewState, ?TIMEOUT};
handle_info({tcp_closed, _Socket}, State) ->
	{stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
	{stop, Reason, State};
handle_info(timeout, State) ->
	timer:sleep(20),
	{stop, normal, State};
handle_info(_Info, State) ->
	{stop, normal, State}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
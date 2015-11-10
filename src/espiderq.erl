-module(espiderq).
-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2, stop/1]).
-export([req/2, req/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API implementation
start_link(ConnectArgs) ->
    gen_server:start_link(?MODULE, {init, ConnectArgs}, []).

start_link(RegisterName, ConnectArgs) ->
    gen_server:start_link({local, RegisterName}, ?MODULE, {init, ConnectArgs}, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

req(Pid, Request) ->
    gen_server:call(Pid, {req, Request}).

req(Pid, Request, Timeout) ->
    gen_server:call(Pid, {req, Request}, Timeout).

%% gen_server implementation
init({init, ConnectArgs}) ->
    {ok, Socket} = ezmq:start([{type, dealer}]),
    ok = erlang:apply(fun ezmq:connect/5, [Socket|ConnectArgs]),
    spawn_link(fun() -> recv_loop(Socket) end),
    {ok, {socket, Socket}}.

handle_info(_Msg, State) ->
    {noreply, State}.

handle_call({req, Request}, From, {socket, Socket}) ->
    FromBin = erlang:term_to_binary(From),
    ok = ezmq:send(Socket, [<<>>, FromBin, proto:encode(Request)]),
    {noreply, {socket, Socket}};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, {socket, Socket}) ->
    ezmq:close(Socket);
terminate(_Reason, _State) ->
    ok.

%% internal
recv_loop(Socket) ->
    {ok, [<<>>, FromBin, ReplyBin]} = ezmq:recv(Socket),
    From = erlang:binary_to_term(FromBin),
    gen_server:reply(From, proto:decode(ReplyBin)),
    recv_loop(Socket).

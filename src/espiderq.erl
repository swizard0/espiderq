-module(espiderq).
-behaviour(gen_server).
-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(ConnectArgs) ->
    gen_server:start_link(?MODULE, {init, ConnectArgs}, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init({init, ConnectArgs}) ->
    {ok, Socket} = ezmq:start([{type, dealer}]),
    ok = erlang:apply(fun ezmq:connect/5, [Socket|ConnectArgs]),
    {ok, {socket, Socket}}.

handle_info(_Msg, State) ->
    {noreply, State}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.



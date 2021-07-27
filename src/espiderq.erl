-module(espiderq).
-behaviour(gen_server).

-export([start_link/1, start_link/2, stop/1]).
-export([req/2, req/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API

start_link(ConnectArgs) ->
    gen_server:start_link(?MODULE, ConnectArgs, []).

start_link(RegisterName, ConnectArgs) ->
    io:format("starting ~p under the name of ~p~n", [?MODULE, RegisterName]),
    gen_server:start_link({local, RegisterName}, ?MODULE, ConnectArgs, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

req(Pid, Request) ->
    gen_server:call(Pid, {req, Request}).

req(Pid, Request, Timeout) ->
    gen_server:call(Pid, {req, Request}, Timeout).

%% callbacks
init(ConnectArgs_) ->
    {ok, Socket} = chumak:socket(dealer),
    ConnectArgs = case ConnectArgs_ of
                      [tcp, {A,B,C,D} | Rest] when is_integer(A), is_integer(B), is_integer(C), is_integer(D) ->
                          [tcp, lists:flatten(io_lib:format("~w.~w.~w.~w", [A, B, C, D])) | Rest];
                      _ -> ConnectArgs_
                  end,
    case erlang:apply(chumak, connect, [Socket | ConnectArgs]) of
        {ok, Pid} ->
            io:format("~p started, pid: ~p, socket: ~p, client: ~p~n", [?MODULE, self(), Socket, Pid]);
        {error, Reason} ->
            io:format("~p start failed: connection failed, reason: ~p~n", [?MODULE, Reason]),
            exit({connect_error, Reason})
    end,
    spawn_link(fun() -> recv_loop(Socket) end),
    {ok, {socket, Socket}}.


handle_info(_Msg, State) ->
    {noreply, State}.

handle_call({req, Request}, From, State = {socket, Socket}) ->
    case chumak:send_multipart(Socket, [<<>>, erlang:term_to_binary(From), proto:encode(Request)]) of
        ok -> ok;
        {error, Reason} -> exit({send_error, Reason})
    end,
    {noreply, State};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% internal

recv_loop(Socket) ->
    case chumak:recv_multipart(Socket) of
        {ok, [<<>>, FromBin, ResponseBin]} ->
            From = erlang:binary_to_term(FromBin),
            Response = proto:decode(ResponseBin),
            gen_server:reply(From, Response);
        {ok, Other} ->
            io:format("unexpected recv: ~p~n", [Other]);
        {error, Reason} ->
            exit({recv_error, Reason})
    end,
    recv_loop(Socket).

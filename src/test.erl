-module(test).
-export([run/7, random_delays/4]).

worker_fold(_SpiderQ, 0, _Proc, Seed) ->
    {ok, Seed};
worker_fold(SpiderQ, ReqsCount, Proc, Seed) ->
    {lent, Id, Data} =
        espiderq:req(SpiderQ, {lend, 5000}),
    {ok, NextSeed} =
        Proc(Seed, Id, Data),
    repaid = 
        espiderq:req(SpiderQ, {repay, Id, case random:uniform(2) of 
                                              1 -> penalty;
                                              2 -> reward 
                                          end}),
    worker_fold(SpiderQ, ReqsCount - 1, Proc, NextSeed).

spawn_loop(_SpiderQ, 0, _WorkerReqsCount, _Proc, _InitSeed) ->
    ok;
spawn_loop(SpiderQ, WorkersCount, WorkerReqsCount, Proc, InitSeed) ->
    spawn_link(fun() ->
                       {ok, Result} = worker_fold(SpiderQ, WorkerReqsCount, Proc, InitSeed),
                       exit({worker_done, Result})
               end),
    spawn_loop(SpiderQ, WorkersCount - 1, WorkerReqsCount, Proc, InitSeed).

run(ConnectArgs, WorkersCount, WorkerReqsCount, MapProc, MapSeed, ReduceProc, ReduceSeed) ->
    {ok, SpiderQ} = espiderq:start_link(ConnectArgs),
    process_flag( trap_exit, true ),
    ok = spawn_loop(SpiderQ, WorkersCount, WorkerReqsCount, MapProc, MapSeed),
    {ExecutionMs, {ok, Result}} = timer:tc(fun() -> wait_done(SpiderQ, WorkersCount, ReduceProc, ReduceSeed) end),
    process_flag( trap_exit, false ),
    io:format(" ;; ~p~n", [[{result, Result},
                            {total_ms, ExecutionMs div 1000},
                            {reqs, (WorkersCount * WorkerReqsCount)}]]),
    {ok,
     {result, Result},
     {total_ms, ExecutionMs div 1000},
     {reqs, (WorkersCount * WorkerReqsCount)},
     {rps, case ExecutionMs < 1000000 of
               true -> WorkersCount * WorkerReqsCount;
               false -> (WorkersCount * WorkerReqsCount) / (ExecutionMs div 1000000)
           end}}.

wait_done(_SpiderQ, 0, _Proc, Seed) ->
    {ok, Seed};
wait_done(SpiderQ, WorkersCount, Proc, Seed) ->
    receive
        {'EXIT', _Pid, {worker_done, Result}} ->
            {ok, NextSeed} = Proc(Seed, Result),
            wait_done(SpiderQ, WorkersCount - 1, Proc, NextSeed);
        Msg ->
            error({unexpected_msg_received, Msg})
    after 3000 ->
            {stats, Stats} = espiderq:req(SpiderQ, stats),
            io:format("~p workers in progress, spiderq stats: ~p~n", [WorkersCount, Stats]),
            wait_done(SpiderQ, WorkersCount, Proc, Seed)
    end.

random_delays(ConnectArgs, WorkersCount, WorkerReqsCount, MaxDelayMs) ->
    run(ConnectArgs, WorkersCount, WorkerReqsCount,
        fun(map_seed, _Id, _Data) ->
                CurrentDelayMs = random:uniform(MaxDelayMs),
                ok = timer:sleep(CurrentDelayMs),
                {ok, map_seed}
        end,
        map_seed,
        fun(reduce_seed, map_seed) -> {ok, reduce_seed} end,
        reduce_seed).

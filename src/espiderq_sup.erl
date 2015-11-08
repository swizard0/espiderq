-module(espiderq_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% API implementation
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor implementation
init([]) ->
    {ok, {{one_for_one, 5, 10},
          [{espiderq,
            {espiderq, start_link, []}, 
            permanent,
            infinity, 
            worker, 
            [espiderq]}]}}.

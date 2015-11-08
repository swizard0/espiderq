-module(espiderq_app).
-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%% application implementation
start(_StartType, _StartArgs) ->
    espiderq_sup:start_link().

stop(_State) ->
    ok.

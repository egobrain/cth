-module(cth_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

%% =============================================================================
%% API
%% =============================================================================

start(_StartType, _StartArgs) ->
    cth_sup:start_link().

stop(_State) ->
    ok.

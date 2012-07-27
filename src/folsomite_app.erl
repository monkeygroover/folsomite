-module(folsomite_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Application callbacks
start(normal, no_arg) ->
    folsom_metrics:new_histogram(
      {folsomite, send_stats}, slide_uniform, {60, 1028}),
    folsomite_sup:start_link().

stop(_) -> ok.

-module(folsomite_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Application callbacks
start(normal, no_arg) -> folsomite_sup:start_link().

stop(_) -> ok.

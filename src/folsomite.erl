-module(folsomite).
-export([start/0]).

%% api
start() -> start(folsomite).

%% internal
start(App) -> start_ok(App, application:start(App, permanent)).
start_ok(_, ok) -> ok;
start_ok(_App, {error, {already_started, _App}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) ->
    ok = start(Dep),
    start(App);
start_ok(App, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).

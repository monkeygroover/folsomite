-module(folsomite_graphite_client_sup).
-behaviour(supervisor).

-export([get_client/0]).
-export([start_link/0]).
-export([init/1]).

-define(APP, folsomite).

%% api
get_client() ->
    case start_client(get_env(graphite_host), get_env(graphite_port)) of
        {error, {already_started, Pid}} -> {ok, Pid};
        {ok, Pid} -> {ok, Pid};
        {error, Reason} -> {error, Reason}
    end.

%% management api
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, no_arg).

%% supervisor callback
init(no_arg) -> {ok, {{one_for_one, 5, 10}, []}}.


%% internal
start_client(Host, Port) ->
      supervisor:start_child(
        ?MODULE,
        {?APP,
         {folsomite_graphite_client, start_link, [Host, Port]},
         temporary,
         brutal_kill,
         worker,
         [folsomite_graphite_client]}).

get_env(Name) ->
    {ok, Value} = application:get_env(?APP, Name),
    Value.

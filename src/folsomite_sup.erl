-module(folsomite_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

%% Management API functions
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_arg).

%% Supervisor callbacks
init(no_arg) ->
    GraphiteClientSup = {folsomite_graphite_client_sup,
                         {folsomite_graphite_client_sup, start_link, []},
                         permanent,
                         1000,
                         supervisor,
                         [egraphite_corral]},
    Worker = {folsomite_server,
              {folsomite_server, start_link, []},
              permanent,
              1000,
              worker,
              [folsomite_server]},
    {ok, {{one_for_all, 5, 3600}, [GraphiteClientSup, Worker]}}.

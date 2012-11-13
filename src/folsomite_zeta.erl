-module(folsomite_zeta).
-export([event/3, event/4, host_event/3, host_event/4, get_tags/0]).

-define(APP, folsomite).

event(K, V, Opts) ->
    event(node_prefix(), K, V, [{tags, get_tags()}|Opts]).

event(Prefix, K, V, Opts) ->
    send_event(Prefix ++ " " ++ K, V, ok, Opts).

host_event(K, V, Opts) ->
    host_event(node_prefix(), K, V, [{tags, get_tags()}|Opts]).

host_event(Prefix, K, V, Opts) ->
    send_event({hostname(), Prefix ++ " " ++ K}, V, ok, Opts).

send_event(Prefix, V, State, Opts) ->
    zeta:ev(Prefix, V, State, Opts).

hostname() ->
    net_adm:localhost().

node_prefix() ->
    NodeList = atom_to_list(node()),
    [A, _] = string:tokens(NodeList, "@"),
    A.

get_tags() ->
    case application:get_env(?APP, tags) of
        {ok, Value} ->
            Value;
        undefined ->
            []
    end.


-module(folsomite_zeta).
-export([event/3, event/4, hostname/0, node_prefix/0]).

-define(APP, folsomite).

event(K, V, Opts) ->
    event(K, V, node_prefix(), [{tags, get_tags()}|Opts]).

event(Prefix, K, V, Opts) ->
    event(hostname(), Prefix, K, V, Opts).

event(Hostname, Prefix, K, V, Opts) ->
    zeta:ev({Hostname, Prefix ++ " " ++ K}, V, ok, Opts).

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


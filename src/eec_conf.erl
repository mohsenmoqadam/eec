-module(eec_conf).

-export([ get/1
	, get/2
	, get_es_host/0
	, get_es_port/0
	, set/2
	, load/1
	, get_root/0
	, get_path/0
	, fix_test_profile/0
	]).

-include("eec.hrl").

-spec get(atom()) -> {ok, any()} | {error, undefined}.
get(Key) ->
    case application:get_env(eec, Key) of
	{ok, Value} ->
	    {ok, Value};
	_ ->
	    {error, undefined}
    end.

get(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        false ->
            undefined;
        {Key, Value} ->
            {ok, Value}
    end.

-spec get_es_host() -> binary().
get_es_host() ->
    {ok, Host} = eec_conf:get('es.host'),
    Host.

-spec get_es_port() -> integer().
get_es_port() ->
    {ok, Port} = eec_conf:get('es.port'),
    Port.

-spec set(atom(), any()) -> ok.
set(Key, Value) ->
    application:set_env(eec, Key, Value).

-spec load(string()) -> ok.
load(Path) ->
    {ok, [ConfigList]} = file:consult(Path),
    _ = [application:set_env(Application, Key, Val)
         || {Application, Items} <- ConfigList,
            {Key, Val} <- Items],
    ok.

-spec get_path() -> string().
get_path() ->
    code:lib_dir(eec).

-spec get_root() -> string().
get_root() ->
    get_path() ++ "/../../../../".

%% @NOTE: Rebar3 ct command doesn't have any
%% understanding about config/test.sys.config
%% so we have to load it manually
-spec fix_test_profile() -> ok.
-ifdef(TEST).
fix_test_profile() ->
    Path = get_root() ++ "/config/test.sys.config",
    ok = load(Path),
    ok.
-else.
fix_test_profile() ->
    ok.
-endif.

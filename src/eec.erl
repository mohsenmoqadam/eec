-module(eec).

-export([ create_index/1
        , create_index/2
        , create_index/3
        , create_index_template/2
        , create_index_template/3
        , stats_index/0
        , stats_index/1
        , stats_index/2
        , nodes_info/0
        , nodes_info/1
        , nodes_info/2
        , put_mapping/3
        , put_mapping/4
        , get_mapping/0
        , get_mapping/1
        , get_mapping/2
        , get_mapping/3
        , get_settings/0
        , get_settings/1
        , get_settings/2
        , get_index_templates/0
        , get_index_templates/1
        , get_index_templates/2
        , index_doc/3
        , index_doc/4
        , index_doc_with_opts/5
        , index_doc_with_id/4
        , index_doc_with_id/5
        , index_doc_with_id_opts/6
        , update_doc/4
        , update_doc/5
        , update_doc_opts/6
        , upsert_doc/4
        , upsert_doc/5
        , upsert_doc_opts/6
        , bulk_index_docs/2
        , bulk_index_docs/1
        , search/2
        , search/3
        , search/5
        , count/2
        , count/3
        , count/5
        , search_limit/4
        , search_scroll/4
        , search_scroll/1
        , multi_search/2
        , get_doc/3
        , get_doc/4
        , get_multi_doc/3
        , get_doc_opts/5
        , flush_index/1
        , flush_index/2
        , flush_all/0
        , flush_all/1
        , refresh_all/0
        , refresh_all/1
        , refresh_index/1
        , refresh_index/2
        , delete_doc/3
        , delete_doc/4
        , delete_doc_by_query/3
        , delete_doc_by_query/4
        , delete_doc_by_query_doc/3
        , delete_doc_by_query_doc/4
        , delete_index/1
        , delete_index/2
        , delete_index_template/1
        , delete_index_template/2
        , index_exists/1
        , index_exists/2
        , index_template_exists/1
        , index_template_exists/2
        , optimize_index/1
        , optimize_index/2
        , percolator_add/3
        , percolator_add/4
        , percolator_del/2
        , percolator_del/3
        , percolate/3
        , percolate/4
        , reindex/1
        , reindex/2
        , aliases/1
        , aliases/2
        , bulk_operation/1
        , bulk_operation/2
        , put_setting/2
        , put_setting/3
]).


-include("eec.hrl").

-spec create_index(binary()) -> {ok, eec_success_result()} | {error, any()}.
create_index(Index) ->
    create_index(#eec_params{}, Index, <<>>).

-spec create_index(#eec_params{}, binary()) -> {ok, eec_success_result()} | {error, any()}.
create_index(#eec_params{} = Params, Index) when is_binary(Index) ->
    create_index(Params, Index, <<>>);
create_index(Index, Doc) when is_binary(Index), (is_binary(Doc) orelse is_list(Doc) orelse is_tuple(Doc) orelse is_map(Doc)) ->
    create_index(#eec_params{}, Index, Doc).

-spec create_index(#eec_params{}, binary(), eec_json() | binary()) -> {ok, eec_success_result()} | {error, any()}.
create_index(Params, Index, Doc) when is_binary(Index), (is_binary(Doc) orelse is_list(Doc) orelse is_tuple(Doc) orelse is_map(Doc)) ->
    eec_resource:put(Params, Index, [], [], maybe_encode_doc(Doc), Params#eec_params.http_client_options).

-spec create_index_template(Index :: binary(), Doc :: eec_json() | binary()) -> {ok, eec_success_result()} | {error, any()}.
create_index_template(Index, Doc) when is_binary(Index), (is_binary(Doc) orelse is_list(Doc) orelse is_tuple(Doc) orelse is_map(Doc)) ->
    create_index_template(#eec_params{}, Index, Doc).

-spec create_index_template(#eec_params{}, binary(), eec_json() | binary()) -> {ok, eec_success_result()} | {error, any()}.
create_index_template(Params, Index, Doc) when is_binary(Index), (is_binary(Doc) orelse is_list(Doc) orelse is_tuple(Doc) orelse is_map(Doc)) ->
    eec_resource:put(Params, <<"_template/", Index/binary>>, [], [], maybe_encode_doc(Doc), Params#eec_params.http_client_options).

stats_index() ->
    stats_index(#eec_params{}).

stats_index(Params) ->
    stats_index(Params, []).

stats_index(Params, Index) ->
    eec_resource:get(Params, filename:join(commas(Index),"_stats"), [], [],
                      Params#eec_params.http_client_options).

-spec nodes_info() -> {ok, eec_success_result()} | {error, any()}.
nodes_info() ->
    nodes_info(#eec_params{}).

-spec nodes_info(#eec_params{}) -> {ok, eec_success_result()} | {error, any()}.
nodes_info(#eec_params{} = Params) ->
    nodes_info(Params, []).

-spec nodes_info(#eec_params{}, [binary()]) -> {ok, eec_success_result()} | {error, any()}.
nodes_info(#eec_params{} = Params, Nodes) when erlang:is_list(Nodes) ->
    eec_resource:get(Params, filename:join("_nodes", commas(Nodes)), [], [],
                      Params#eec_params.http_client_options).

-spec put_mapping(binary(), binary(), eec_json() | binary()) -> {ok, eec_success_result()} | {error, any()}.
put_mapping(Index, Type, Doc) ->
    put_mapping(#eec_params{}, Index, Type, Doc).

-spec put_mapping(#eec_params{}, binary(), binary(), eec_json() | binary()) -> {ok, eec_success_result()} | {error, any()}.
put_mapping(Params, Index, Type, Doc) ->
    eec_resource:put(Params, filename:join([Index, Type, "_mapping"]), [], [], maybe_encode_doc(Doc), Params#eec_params.http_client_options).

-spec get_mapping() -> {ok, eec_success_result()} | {error, any()}.
get_mapping() ->
    get_mapping(#eec_params{}, <<"_all">>, <<"_all">>).

-spec get_mapping(#eec_params{} | binary()) -> {ok, eec_success_result()} | {error, any()}.
get_mapping(#eec_params{} = Params) ->
    get_mapping(Params, <<"_all">>, <<"_all">>);
get_mapping(Index) when is_binary(Index) ->
    get_mapping(#eec_params{}, Index, <<"_all">>).

-spec get_mapping(#eec_params{} | binary(), binary()) -> {ok, eec_success_result()} | {error, any()}.
get_mapping(#eec_params{} = Params, Index) when is_binary(Index) ->
    get_mapping(Params, Index, <<"_all">>);
get_mapping(Index, Type) when is_binary(Index), is_binary(Type) ->
    get_mapping(#eec_params{}, Index, Type).

-spec get_mapping(#eec_params{}, binary(), binary()) -> {ok, eec_success_result()} | {error, any()}.
get_mapping(#eec_params{} = Params, Index, Type) when is_binary(Index), is_binary(Type) ->
    eec_resource:get(Params, filename:join([Index, <<"_mapping">>, Type]), [], [], [], Params#eec_params.http_client_options).

-spec get_settings() -> {ok, eec_success_result()} | {error, any()}.
get_settings() ->
    get_settings(#eec_params{}, <<"_all">>).

-spec get_settings(#eec_params{} | binary()) -> {ok, eec_success_result()} | {error, any()}.
get_settings(#eec_params{} = Params) ->
    get_settings(Params, <<"_all">>);
get_settings(Index) when is_binary(Index) ->
    get_settings(#eec_params{}, Index).

-spec get_settings(#eec_params{}, binary()) -> {ok, eec_success_result()} | {error, any()}.
get_settings(#eec_params{} = Params, Index) when is_binary(Index) ->
    eec_resource:get(Params, filename:join([Index, <<"_settings">>]), [], [], [], Params#eec_params.http_client_options).

-spec get_index_templates() -> {ok, eec_success_result()} | {error, any()}.
get_index_templates() ->
    get_index_templates(#eec_params{}, <<>>).

-spec get_index_templates(binary() | #eec_params{}) -> {ok, eec_success_result()} | {error, any()}.
get_index_templates(IndexTemplate) when is_binary(IndexTemplate) ->
    get_index_templates(#eec_params{}, IndexTemplate);
get_index_templates(#eec_params{} = Params) ->
    get_index_templates(Params, <<>>).

-spec get_index_templates(#eec_params{}, binary()) -> {ok, eec_success_result()} | {error, any()}.
get_index_templates(#eec_params{http_client_options = HttpClientOptions} = Params, IndexTemplate) ->
    eec_resource:get(Params, filename:join([<<"_template">>, IndexTemplate]), [], [], [], HttpClientOptions).

-spec index_doc(binary(), binary(), eec_json() | binary()) -> {ok, eec_success_result()} | {error, any()}.
index_doc(Index, Type, Doc) ->
    index_doc(#eec_params{}, Index, Type, Doc).

-spec index_doc(#eec_params{}, binary(), binary(), eec_json() | binary()) -> {ok, eec_success_result()} | {error, any()}.
index_doc(Params, Index, Type, Doc) ->
    index_doc_with_opts(Params, Index, Type, Doc, []).

-spec index_doc_with_opts(#eec_params{}, binary(), binary(), eec_json() | binary(), list()) -> {ok, eec_success_result()} | {error, any()}.
index_doc_with_opts(Params, Index, Type, Doc, Opts) when is_list(Opts) ->
    eec_resource:post(Params, filename:join(Index, Type), [], Opts, maybe_encode_doc(Doc), Params#eec_params.http_client_options).

-spec index_doc_with_id(binary(), binary(), binary(), eec_json() | binary()) -> {ok, eec_success_result()} | {error, any()}.
index_doc_with_id(Index, Type, Id, Doc) ->
    index_doc_with_id_opts(#eec_params{}, Index, Type, Id, Doc, []).

-spec index_doc_with_id(#eec_params{}, binary(), binary(), binary(), eec_json() | binary()) -> {ok, eec_success_result()} | {error, any()}.
index_doc_with_id(Params, Index, Type, Id, Doc) ->
    index_doc_with_id_opts(Params, Index, Type, Id, Doc, []).

-spec index_doc_with_id_opts(#eec_params{}, binary(), binary(), binary(), eec_json() | binary(), list()) -> {ok, eec_success_result()} | {error, any()}.
index_doc_with_id_opts(Params, Index, Type, undefined, Doc, Opts) ->
    index_doc_with_opts(Params, Index, Type, Doc, Opts);
index_doc_with_id_opts(Params, Index, Type, Id, Doc, Opts) when is_list(Opts) ->
    eec_resource:post(Params, filename:join([Index, Type, Id]), [], Opts, maybe_encode_doc(Doc), Params#eec_params.http_client_options).

-spec update_doc(binary(), binary(), binary(), eec_json()) -> {ok, eec_success_result()} | {error, any()}.
update_doc(Index, Type, Id, Doc) ->
    update_doc_opts(#eec_params{}, Index, Type, Id, Doc, []).

-spec update_doc(#eec_params{}, binary(), binary(), binary(), eec_json()) -> {ok, eec_success_result()} | {error, any()}.
update_doc(Params, Index, Type, Id, Doc) ->
    update_doc_opts(Params, Index, Type, Id, Doc, []).

-spec update_doc_opts(#eec_params{}, binary(), binary(), binary(), eec_json(), list()) -> {ok, eec_success_result()} | {error, any()}.
update_doc_opts(Params, Index, Type, Id, Doc, Opts) when is_list(Opts), (is_list(Doc) orelse is_tuple(Doc) orelse is_map(Doc)) ->
    DocBin = eec_jsco:encode(Doc),
    Body = <<"{\"doc\":", DocBin/binary, "}">>,
    eec_resource:post(Params, filename:join([Index, Type, Id, "_update"]), [], Opts,
        Body,
        Params#eec_params.http_client_options).

-spec upsert_doc(binary(), binary(), binary(), eec_json()) -> {ok, eec_success_result()} | {error, any()}.
upsert_doc(Index, Type, Id, Doc) ->
    upsert_doc_opts(#eec_params{}, Index, Type, Id, Doc, []).

-spec upsert_doc(#eec_params{}, binary(), binary(), binary(), eec_json()) -> {ok, eec_success_result()} | {error, any()}.
upsert_doc(Params, Index, Type, Id, Doc) ->
    upsert_doc_opts(Params, Index, Type, Id, Doc, []).

-spec upsert_doc_opts(#eec_params{}, binary(), binary(), binary(), eec_json(), list()) -> {ok, eec_success_result()} | {error, any()}.
upsert_doc_opts(Params, Index, Type, Id, Doc, Opts) when is_list(Opts), (is_list(Doc) orelse is_tuple(Doc) orelse is_map(Doc)) ->
    DocBin = eec_jsco:encode(Doc),
    Body = <<"{\"doc_as_upsert\":true,\"doc\":", DocBin/binary, "}">>,
    eec_resource:post(Params, filename:join([Index, Type, Id, "_update"]), [], Opts,
                       Body,
                       Params#eec_params.http_client_options).

-spec bulk_index_docs(list()) -> {ok, list} | {error, any()}.
bulk_index_docs(IndexTypeIdJsonTuples) ->
    bulk_index_docs(#eec_params{}, IndexTypeIdJsonTuples).

-spec bulk_index_docs(#eec_params{}, list()) -> {ok, list()} | {error, any()}.
bulk_index_docs(Params, IndexTypeIdJsonTuples) ->
    bulk_operation(Params, [{index, IndexTypeIdJsonTuple} || IndexTypeIdJsonTuple <- IndexTypeIdJsonTuples]).

-spec search(binary() | list(), eec_json() | binary()) -> {ok, eec_success_result()} | {error, any()}.
search(Index, Query) ->
    search(#eec_params{}, Index, <<>>, Query, []).

-spec search(binary() | list() | #eec_params{}, binary() | list(), eec_json() | binary()) -> {ok, eec_success_result()} | {error, any()}.
search(Params, Index, Query) when is_record(Params, eec_params) ->
    search(Params, Index, <<>>, Query, []);
search(Index, Type, Query) ->
    search(#eec_params{}, Index, Type, Query, []).

-spec search_limit(binary() | list(), binary(), eec_json() | binary(), integer()) -> {ok, eec_success_result()} | {error, any()}.
search_limit(Index, Type, Query, Limit) when is_integer(Limit) ->
    search(#eec_params{}, Index, Type, Query, [{<<"size">>, integer_to_list(Limit)}]).

-spec count(binary() | list(), eec_json() | binary()) -> {ok, eec_success_result()} | {error, any()}.
count(Index, Query) ->
    count(#eec_params{}, Index, <<>>, Query, []).

-spec count(binary() | list() | #eec_params{}, binary() | list(), eec_json() | binary()) -> {ok, eec_success_result()} | {error, any()}.
count(Params, Index, Query) when is_record(Params, eec_params) ->
    count(Params, Index, <<>>, Query, []);
count(Index, Type, Query) ->
    count(#eec_params{}, Index, Type, Query, []).

-spec count(#eec_params{}, list() | binary(), list() | binary(), eec_json() | binary(), list()) -> {ok, eec_success_result()} | {error, any()}.
count(Params, Index, Type, Query, Opts) ->
    search_helper(<<"_count">>, Params, Index, Type, Query, Opts).

-spec search_scroll(binary() | list(), binary(), eec_json() | binary(), list()) -> {ok, eec_success_result()} | {error, any()}.
search_scroll(Index, Type, Query, Timeout) ->
    search(#eec_params{}, Index, Type, Query, [{<<"scroll">>, list_to_binary(Timeout)}]).

-spec search_scroll(eec_json() | binary()) -> {ok, eec_success_result()} | {error, any()}.
search_scroll(Query) ->
     Params = #eec_params{},
     eec_resource:post(Params, filename:join([<<"_search">>, <<"scroll">>]), [], [], eec_jsco:encode(Query), Params#eec_params.http_client_options).

-spec search(#eec_params{}, list() | binary(), list() | binary(), eec_json() | binary(), list()) -> {ok, eec_success_result()} | {error, any()}.
search(Params, Index, Type, Query, Opts) ->
    search_helper(<<"_search">>, Params, Index, Type, Query, Opts).

-spec multi_search(#eec_params{}, list({HeaderInformation :: headers(), SearchRequest :: eec_json() | binary()})) -> {ok, ResultJson :: eec_success_result()} | {error, Reason :: any()}.
multi_search(Params, HeaderJsonTuples) ->
    Body = lists:map(fun({HeaderInformation, SearchRequest}) ->
        [ eec_jsco:encode(HeaderInformation), <<"\n">>, maybe_encode_doc(SearchRequest), <<"\n">> ]
    end, HeaderJsonTuples),
    eec_resource:get(Params, <<"/_msearch">>, [], [], iolist_to_binary(Body), Params#eec_params.http_client_options).

-spec get_doc(binary(), binary(), binary()) -> {ok, eec_success_result()} | {error, any()}.
get_doc(Index, Type, Id) ->
    get_doc(#eec_params{}, Index, Type, Id).

-spec get_doc(#eec_params{}, binary(), binary(), binary()) -> {ok, eec_success_result()} | {error, any()}.
get_doc(Params, Index, Type, Id) ->
    get_doc_opts(Params, Index, Type, Id, []).

-spec get_doc_opts(#eec_params{}, binary(), binary(), binary(), list()) -> {ok, eec_success_result()}
                                                                          | {error, any()}.
get_doc_opts(Params, Index, Type, Id, Opts) ->
    eec_resource:get(Params, filename:join([Index, Type, Id]), [], Opts, Params#eec_params.http_client_options).

-spec get_multi_doc(binary(), binary(), list()) -> {ok, eec_success_result()} | {error, any()}.
get_multi_doc(Index, Type, Data) ->
     Params = #eec_params{},
     eec_resource:post(Params, filename:join([Index, Type, <<"_mget">>]), [], [], eec_jsco:encode(Data),
                        Params#eec_params.http_client_options).

flush_index(Index) ->
    flush_index(#eec_params{}, Index).

flush_index(Params, Index) ->
    eec_resource:post(Params, filename:join([commas(Index), <<"_flush">>]), [], [], [], Params#eec_params.http_client_options).

flush_all() ->
    refresh_all(#eec_params{}).

flush_all(Params) ->
    eec_resource:post(Params, <<"_flush">>, [], [], [], Params#eec_params.http_client_options).

refresh_index(Index) ->
    refresh_index(#eec_params{}, Index).

refresh_index(Params, Index) ->
    eec_resource:post(Params, filename:join([commas(Index), <<"_refresh">>]), [], [], [], Params#eec_params.http_client_options).

refresh_all() ->
    refresh_all(#eec_params{}).

refresh_all(Params) ->
    eec_resource:post(Params, <<"_refresh">>, [], [], [], Params#eec_params.http_client_options).

delete_doc(Index, Type, Id) ->
    delete_doc(#eec_params{}, Index, Type, Id).

delete_doc(Params, Index, Type, Id) ->
    eec_resource:delete(Params, filename:join([Index, Type, Id]), [], [], Params#eec_params.http_client_options).

delete_doc_by_query(Index, Type, Query) ->
    delete_doc_by_query(#eec_params{}, Index, Type, Query).

delete_doc_by_query(Params, Index, Type, Query) ->
    eec_resource:delete(Params, filename:join([Index, Type]), [], [{<<"q">>, Query}], Params#eec_params.http_client_options).

delete_doc_by_query_doc(Index, Type, Doc) ->
    delete_doc_by_query_doc(#eec_params{}, Index, Type, Doc).

delete_doc_by_query_doc(Params, Index, any, Doc) ->
    eec_resource:post(Params, filename:join([Index, <<"_delete_by_query">>]), [], [], eec_jsco:encode(Doc), Params#eec_params.http_client_options);

delete_doc_by_query_doc(Params, Index, Type, Doc) ->
    eec_resource:post(Params, filename:join([Index, Type, <<"_delete_by_query">>]), [], [], eec_jsco:encode(Doc), Params#eec_params.http_client_options).

delete_index(Index) ->
    delete_index(#eec_params{}, Index).

delete_index(Params, Index) ->
    eec_resource:delete(Params, Index, [], [], [],
                         Params#eec_params.http_client_options).

-spec delete_index_template(binary()) -> {ok, eec_success_result()} | {error, any()}.
delete_index_template(Index) ->
    delete_index_template(#eec_params{}, Index).

-spec delete_index_template(#eec_params{}, binary()) -> {ok, eec_success_result()} | {error, any()}.
delete_index_template(Params, Index) ->
    eec_resource:delete(Params, <<"_template/", Index/binary>>, [], [], [],
        Params#eec_params.http_client_options).

-spec index_exists(binary()) -> {ok, boolean()} | {error, any()}.
index_exists(Index) ->
    index_exists(#eec_params{}, Index).

-spec index_exists(#eec_params{}, binary()) -> {ok, boolean()} | {error, any()}.
index_exists(Params, Index) ->
    exists(eec_resource:head(Params, Index, [], [], Params#eec_params.http_client_options)).

-spec index_template_exists(binary()) -> {ok, boolean()} | {error, any()}.
index_template_exists(IndexTemplate) ->
    index_template_exists(#eec_params{}, IndexTemplate).

-spec index_template_exists(#eec_params{}, binary()) -> {ok, boolean()} | {error, any()}.
index_template_exists(Params, IndexTemplate) ->
    exists(eec_resource:head(Params, filename:join([<<"_template">>, IndexTemplate]), [], [], Params#eec_params.http_client_options)).

%%=== PRIVATE
exists(ok) -> {ok, true};
exists({error, 404}) -> {ok, false};
exists({error, _Else} = Error) -> Error.

optimize_index(Index) ->
    optimize_index(#eec_params{}, Index).

optimize_index(Params, Index) ->
    eec_resource:post(Params, filename:join([commas(Index), <<"_optimize">>]), [], [], [], Params#eec_params.http_client_options).

percolator_add(Index, Name, Query) ->
    percolator_add(#eec_params{}, Index, Name, Query).

percolator_add(Params, Index, Name, Query) ->
    eec_resource:put(Params, filename:join([<<"_percolator">>, commas(Index), Name]), [], [], eec_jsco:encode(Query), Params#eec_params.http_client_options).

percolator_del(Index, Name) ->
    percolator_del(#eec_params{}, Index, Name).

percolator_del(Params, Index, Name) ->
    eec_resource:delete(Params, filename:join([<<"_percolator">>, commas(Index), Name]), [], [], [], Params#eec_params.http_client_options).

percolate(Index, Type, Doc) ->
    percolate(#eec_params{}, Index, Type, Doc).

percolate(Params, Index, Type, Doc) ->
    eec_resource:get(Params, filename:join([commas(Index), Type, <<"_percolate">>]), [], [], eec_jsco:encode(Doc), Params#eec_params.http_client_options).

reindex(Body) ->
    reindex(#eec_params{}, Body).

reindex(Params, Body) ->
    eec_resource:post(Params, filename:join([<<"_reindex">>]), [], [], eec_jsco:encode(Body), Params#eec_params.http_client_options).

aliases(Body) ->
    aliases(#eec_params{}, Body).

aliases(Params, Body) ->
    eec_resource:post(Params, filename:join([<<"_aliases">>]), [], [], eec_jsco:encode(Body), Params#eec_params.http_client_options).

-type index() :: binary().
-type type() :: binary().
-type id() :: binary() | undefined.
-type metadata_tuple() :: {index(), type(), id()} |
                          {index(), type(), id(), headers()} |
                          {index(), type(), id(), eec_json()} |
                          {index(), type(), id(), eec_json(), headers()}.
-type operation() :: {index | create | delete | update, metadata_tuple()}.

-spec bulk_operation([operation()]) -> {ok, list()} | {error, any()}.
bulk_operation(OperationIndexTypeIdJsonTuples) ->
    bulk_operation(#eec_params{}, OperationIndexTypeIdJsonTuples).

-spec bulk_operation(#eec_params{}, [operation()]) -> {ok, list()} | {error, any()}.
bulk_operation(Params, OperationIndexTypeIdJsonTuples) ->
    Body = lists:map(fun
                       Build({delete, {Index, Type, Id}}) ->
                         Build({delete, {Index, Type, Id, [], no_body}});
                       Build({delete, {Index, Type, Id, HeaderInformation}}) ->
                         Build({delete, {Index, Type, Id, HeaderInformation, no_body}});
                       Build({Operation, {Index, Type, Id, Doc}}) ->
                         Build({Operation, {Index, Type, Id, [], Doc}});
                       Build({Operation, {Index, Type, Id, HeaderInformation, Doc}}) ->
                         Header = build_header(Operation, Index, Type, Id, HeaderInformation),
                         Header ++ build_body(Operation, Doc)
                     end, OperationIndexTypeIdJsonTuples),

    eec_resource:post(Params, <<"/_bulk">>, [], [], iolist_to_binary(Body), Params#eec_params.http_client_options).

-spec put_setting(binary(), eec_json() | binary()) -> {ok, eec_success_result()} | {error, any()}.
put_setting(Index, Doc) ->
  put_setting(#eec_params{}, Index, Doc).

-spec put_setting(#eec_params{}, binary(), eec_json() | binary()) -> {ok, eec_success_result()} | {error, any()}.
put_setting(Params, Index, Doc) ->
  eec_resource:put(Params, filename:join([Index, "_settings"]), [], [], maybe_encode_doc(Doc), Params#eec_params.http_client_options).

%%=== Internal functions
-spec search_helper(binary(), #eec_params{}, list() | binary(), list() | binary(), eec_json() | binary(), list()) -> {ok, eec_success_result()} | {error, any()}.
search_helper(Endpoint, Params, Index, Type, Query, Opts) when is_binary(Query) ->
    eec_resource:get(Params, filename:join([commas(Index), Type, Endpoint]), [], [{<<"q">>, Query}]++Opts, Params#eec_params.http_client_options);
search_helper(Endpoint, Params, Index, Type, Query, Opts) ->
    eec_resource:post(Params, filename:join([commas(Index), Type, Endpoint]), [], Opts, eec_jsco:encode(Query), Params#eec_params.http_client_options).

-spec commas(list(binary()) | binary()) -> binary().
commas(Bin) when is_binary(Bin) ->
    Bin;
commas([]) ->
    <<>>;
commas([H | T]) ->
    << H/binary, << <<",", B/binary>> || B <- T >>/binary >>.

build_header(Operation, Index, Type, Id, HeaderInformation) ->
    Header1 = [
      {<<"_index">>, Index},
      {<<"_type">>, Type}
      | HeaderInformation
    ],

    Header2 = case Id =:= undefined of
                true -> Header1;
                false -> [{<<"_id">>, Id} | Header1]
              end,

    [eec_jsco:encode([{erlang:atom_to_binary(Operation, utf8), Header2}])].

build_body(delete, no_body) ->
    [<<"\n">>];
build_body(update, Doc) ->
    DocBin = maybe_encode_doc(Doc),
    Json = <<"{\"doc\":", DocBin/binary, "}">>,
    [<<"\n">>, Json, <<"\n">>];
build_body(_Operation, Doc) ->
    Json = maybe_encode_doc(Doc),
    [<<"\n">>, Json, <<"\n">>].

-spec maybe_encode_doc(binary() | eec_json()) -> binary().
maybe_encode_doc(Bin) when is_binary(Bin) -> Bin;
maybe_encode_doc(Doc) when is_list(Doc); is_tuple(Doc); is_map(Doc) -> eec_jsco:encode(Doc).

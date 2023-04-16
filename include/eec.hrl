%% -*- mode:erlang -*-

-ifndef(HEADER_EEC).
-define(HEADER_EEC, true).

-type header() :: {binary(), binary()}.
-type headers() :: [header()].
-type eec_json() :: tuple() | list() | map().
%% Hackney async references actually are just that, references... but it seems
%% to be an undocumented implementation detail; doc (and specs) only says `any()'
-type eec_success_result() :: eec_json() | {async, HackneyRef :: any()}.

-record(eec_params, {
          host        = eec_conf:get_es_host() :: binary(),
          port        = eec_conf:get_es_port() :: integer(),

          % These are passed verbatim to the underlying http client in use.
          http_client_options = []:: [term()], 

          % Keeping the following two options for backwards compatibility.
          timeout     = infinity :: integer() | infinity,
          ctimeout    = infinity :: integer() | infinity
         }).

-ifdef(TEST).
-define(LOG_ERROR(Format, Args), ct:print(default, 50, Format, Args)).
-define(LOG_INFO(Format, Args), ?LOG_ERROR(Format, Args)).
-define(LOG_DEBUG(Format, Args), ?LOG_ERROR(Format, Args)).
-else.
-define(LOG_ERROR(Format, Args), lager:error(Format, Args)).
-define(LOG_INFO(Format, Args), lager:info(Format, Args)).
-define(LOG_DEBUG(Format, Args), lager:debug(Format, Args)).
-endif.

-endif.

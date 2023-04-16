-module(eec_jsco).

-export([ encode/1
	, decode/1
	]).

encode(Map) ->
    jiffy:encode(Map).

decode(BinaryJson) ->
    jiffy:decode(BinaryJson, [return_maps, {null_term, undefined}]).

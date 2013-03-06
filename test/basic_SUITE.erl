-module(basic_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("wsecli.hrl").
-export([all/0]).
-export([decode_test/1, decode_test1/1]).

all() -> [decode_test, decode_test1].

-spec decode_test(_Config) -> ok.
decode_test(_Config)->
	F = #frame{fin=1, opcode = 9, mask = 0, payload_len = 0, payload = <<>>},
	M = {<<>>,[#message{frames=[F], payload = [], type=ping}]},
	io:format("M is ~p",[M]),
	B1 = <<137,0>>,
	WM = wsecli_message:decode(list_to_binary([B1])),
	io:format("WM is ~p",[WM]),
	true = (M =:= WM)
	.

decode_test1(_Config)->
	F = #frame{fin=1, opcode = 10, mask = 0, payload_len = 0, payload = <<>>},
	M = {<<>>,[#message{frames=[F], payload = [], type=pong}]},
	B1 = <<138,0>>,
	WM = wsecli_message:decode(list_to_binary([B1])),
	true = (M =:= WM)
	.

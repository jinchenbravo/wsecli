-module(basic_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("wsecli.hrl").
-export([all/0]).
-export([decode_test/1, encode_test/1]).

all() -> [decode_test, encode_test].

-spec decode_test(_Config) -> ok.
decode_test(_Config)->
	%
	% test decode ping message
	%
	F = #frame{fin=1, opcode = 9, mask = 0, payload_len = 0, payload = <<>>},
	M = [#message{frames=[F], payload = [], type=ping}],
	io:format("M is ~p",[M]),
	B1 = <<137,0>>,
	{RB,WM} = wsecli_message:decode(B1),
	io:format("WM is ~p",[WM]),
	M = WM,
	%
	% test decode pong message
	%
	F1 = #frame{fin=1, opcode = 10, mask = 0, payload_len = 0, payload = <<>>},
	M1 = [#message{frames=[F1], payload = [], type=pong}],
	B2 = <<138,0>>,
	{RB1,WM1} = wsecli_message:decode(B2),
	true = (M1 =:= WM1),
	%
	% test decode close message
	%
	F2 = #frame{fin=1, opcode = 8, mask = 0, payload_len = 0, payload = <<>>},
	M2 = [#message{frames=[F2], payload = [], type=close}],
	io:format("M2 is ~p",[M2]),
	B3 = <<136,0>>,
	{RB2,WM2} = wsecli_message:decode(B3),
	io:format("WM2 is ~p",[WM2]),	
	M2 = WM2
	.

encode_test(_Config)->
	B = wsecli_message:encode(<<>>,ping),
	io:format("B is ~p",[B]),
	P = [<<137,0>>],
	B = P,

	B1 = wsecli_message:encode(<<>>,pong),
	io:format("B1 is ~p",[B1]),
	P1 = [<<138,0>>],
	B1 = P1,

	B2 = wsecli_message:encode(<<>>,close),
	io:format("B2 is ~p",[B2]),
	P2 = [<<136,0>>],
	B2 = P2		
	.
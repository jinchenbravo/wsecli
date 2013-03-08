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
	M2 = WM2,

	%
	% if the actual binary length is longer than the payloadlen in the header, then after make frame(s) the remaining binary
	% needs to be returned.
	% In this case, becasue the second byte is 126, so according to RFC 6455 the next 16 bits will be the payloadlen, and it 
	% has the value of 130 then the last byte 131 has to be returned as a remianing binary
	%
	B4 = <<130,126,0,130,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,
	37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,
	77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,
	113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131>>,
	{RB4,WM4} = wsecli_message:decode(B4),
	io:format("WM4 is ~p",[WM4]),
	io:format("RB4 is ~p",[RB4]),
	RB4 = <<131>>,

	%
	% if the payloadlen is 126, but the actual binary length is smaller, which means it's the first part of one frame.
	% then the whole thing has to be returned as a remaining binary.
	%
	B5 = <<130,126,1,2,3,4,5>>,
	{RB5,WM5} = wsecli_message:decode(B5),
	io:format("WM5 is ~p",[WM5]),
	io:format("RB5 is ~p",[RB5]),
	RB5 = B5,

	%
	%
	%

	B8 = <<2,126,1,2,3,4,5>>,
	{RB8,WM8} = wsecli_message:decode(B8),
	io:format("WM8 is ~p",[WM8]),
	io:format("RB8 is ~p",[RB8]),
	RB8 = B8,

	%
	% if the first bit is set to 0, then it is considered as fragment of one message
	% decode fragment WS messages, the return message should contain 2 frames
	%
	B6 = <<2,2,2,3>>,
	{RB6,WM6} = wsecli_message:decode(B6),
	io:format("WM6 is ~p",[WM6]),
	io:format("RB6 is ~p",[RB6]),
	B7 = <<130,1,1>>,
	TWM6 = lists:nth(1, WM6),
	{RB7,WM7} = wsecli_message:decode(B7,TWM6),
	io:format("WM7 is ~p",[WM7]),
	io:format("RB7 is ~p",[RB7]),
	FF1 = #frame{fin=1, opcode = 2, mask = 0, payload_len = 1, payload = <<1>>},
	FF2 = #frame{fin=0, opcode = 2, mask = 0, payload_len = 2, payload = <<2,3>>},
	MM1 = [#message{frames=[FF1,FF2], payload = <<1,2,3>>, type=binary}],
	io:format("MM1 is ~p",[MM1]),
	MM1 = WM7,

	ok.

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
	B2 = P2,

	B3 = wsecli_message:encode("a",text),
	io:format("B3 is ~p",[B3]),
	% {RB3,WM3} = wsecli_message:decode(B3),
	% io:format("WM3 is ~p",[WM3]),
	B4 = wsecli_message:encode("b",text),
	io:format("B4 is ~p",[B4]),

	ok.
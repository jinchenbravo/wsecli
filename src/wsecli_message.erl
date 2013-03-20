%Copyright [2012] []

%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at

%       http://www.apache.org/licenses/LICENSE-2.0

%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.
% Jin chen 2012-10-18

%% @hidden

-module(wsecli_message).
-include("wsecli.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([encode/2, decode/1, decode/2]).

-define(FRAGMENT_SIZE, 4096).
-type message_type() :: begin_message | continue_message.

-spec encode(Data::string() | binary(), Type::atom()) -> binary().
encode(Data, Type) when is_list(Data)->
  encode(list_to_binary(Data), Type);

encode(Data, Type)->
  lists:reverse(encode(Data, Type, [])).

-spec decode(Data::binary()) -> list(#message{}).
decode(Data) ->
  decode(Data, begin_message, #message{}).

-spec decode(Data::binary(), Message::#message{}) -> list(#message{}).
 decode(Data, Message) ->
   decode(Data, continue_message, Message).


%
% Internal
%
-spec encode(Data::binary(), Type :: atom(), Acc ::list()) -> list().
encode(Data, Type, _Acc) when Type =:= ping ; Type =:= pong ; Type =:= close->
  [frame(Data, [fin, {opcode, Type}])];
  %Frame = wsecli_framing:frame(Data, [fin, {opcode, Type}]),
  %wsecli_framing:to_binary(Frame);

encode(<<Data:?FRAGMENT_SIZE/binary>>, Type, Acc) ->
  [frame(Data, [fin, {opcode, Type}]) | Acc];

encode(<<Data:?FRAGMENT_SIZE/binary, Rest/binary>>, Type, []) ->
  encode(Rest, continuation, [frame(Data, [{opcode, Type}]) | []]);

encode(<<Data:?FRAGMENT_SIZE/binary, Rest/binary>>, Type, Acc) ->
  encode(Rest, Type, [frame(Data, [{opcode, Type}]) | Acc]);

encode(<<>>, _Type, Acc) ->
  Acc;

encode(<<Data/binary>>, Type, Acc) ->
  [frame(Data, [fin, {opcode, Type}]) | Acc].

-spec frame(Data::binary(), Options::list()) -> binary().
frame(Data, Options) ->
  Frame = wsecli_framing:frame(Data, Options),
  wsecli_framing:to_binary(Frame).

-spec decode(Data::binary(), Type :: message_type(), Message::#message{}) -> list(#message{}).
decode(Data, begin_message, _Message) ->
  {Bin, Frames} = wsecli_framing:from_binary(Data),
%  lager:info("inside decode and Frames is ~p",[Frames]),
  {Bin, lists:reverse(process_frames(Frames, [], []))};
  % {Bin, lists:reverse(process_frames(begin_message, Frames, []))};

decode(Data, continue_message, Message) ->
   {Bin, Frames} = wsecli_framing:from_binary(Data),
   {Bin, lists:reverse(process_frames(Frames, Message#message.frames, []))}.

-spec process_frames(Frames::list(#frame{}), FrameAcc::list(#frame{}), MessageAcc::list(#message{})) -> list(#message{}).

process_frames([], [], Macc) ->
  Macc;

process_frames([], Facc, Macc) ->
  Msg = bm(Facc,fragmented),
  [Msg|Macc];

process_frames([Frame|Frames], Facc, Macc) ->
  case contextualize_frame(Frame) of
    close ->
      NewFacc = [Frame|Facc],
      Msg = bm(NewFacc,completed),
      NewMacc = [Msg|Macc],
      process_frames(Frames, [], NewMacc);
    continue ->
      NewFacc = [Frame|Facc],
      process_frames(Frames,NewFacc,Macc)
  end.

-spec contextualize_frame(Frame :: #frame{}) -> close | continue.
contextualize_frame(Frame) ->
  case {Frame#frame.fin, Frame#frame.opcode} of
    {1, _} -> close;
    {0, _} -> continue
  end.

-spec bm(Frames :: list(#frame{}), fragmented|completed) -> #message{}.
bm(Frames,MsgType) ->
  case MsgType of
    fragmented ->
      #message{frames = Frames, type = fragmented};
    completed ->
      build_message(#message{},Frames)
  end.

build_message(Message, Frames) ->
  [HeadFrame | _] = Frames,

  case HeadFrame#frame.opcode of
    1 ->
      Payload = build_payload_from_frames(text, Frames),
      Message#message{frames = Frames, type = text, payload = Payload};
    2 ->
      Payload = build_payload_from_frames(binary, Frames),
      Message#message{frames = Frames, type = binary, payload = Payload};
    8 ->
      Payload = build_payload_from_frames(close, Frames),
      Message#message{frames = Frames, type = close, payload = Payload};
    9 ->
      Payload = build_payload_from_frames(text, Frames),
      Message#message{frames = Frames, type = ping, payload = Payload};
    10 ->
      Payload = build_payload_from_frames(text, Frames),
      Message#message{frames = Frames, type = pong, payload = Payload}
  end.

build_payload_from_frames(close, [Frame]) ->
  case Frame#frame.payload of
%    <<>> -> {undefined, undefined};
    <<>> -> [];
    <<Status:16, Reason/binary>> -> {Status, binary_to_list(Reason)}
  end;

build_payload_from_frames(binary, Frames) ->
  concatenate_payload_from_frames(Frames);

build_payload_from_frames(text, Frames) ->
  Payload = concatenate_payload_from_frames(Frames),
  binary_to_list(Payload).

concatenate_payload_from_frames(Frames) ->
  concatenate_payload_from_frames(Frames, <<>>).

concatenate_payload_from_frames([], Acc) ->
  Acc;
concatenate_payload_from_frames([Frame | Rest], Acc) ->
  concatenate_payload_from_frames(Rest, <<Acc/binary, (Frame#frame.payload)/binary>>).

encode_test()->
  ?assertEqual(1, 1),
  ok.

decode_test()->
  %
  % test decode ping message
  %
  F = #frame{fin=1, opcode = 9, mask = 0, payload_len = 0, payload = <<>>},
  M = [#message{frames=[F], payload = [], type=ping}],
  io:format("M is ~p",[M]),
  B1 = <<137,0>>,
  {RB,WM} = decode(B1),
  io:format("WM is ~p",[WM]),
  ?assertEqual(M, WM),
  ?assertEqual(RB, <<>>),

  ok.
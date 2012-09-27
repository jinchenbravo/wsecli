%Copyright [2012] [Farruco Sanjurjo Arcay]

%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at

%       http://www.apache.org/licenses/LICENSE-2.0

%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.

%% @hidden

-module(wsecli_message).
-include("wsecli.hrl").

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
  {Bin, lists:reverse(process_frames(begin_message, Frames, []))};

decode(Data, continue_message, Message) ->
  {Bin, Frames} = wsecli_framing:from_binary(Data),
  {Bin, lists:reverse(process_frames(continue_message, Frames, [Message | []]))}.

-spec process_frames(Type:: message_type(), Frames :: list(#frame{}), Messages :: list(#message{})) -> list(#message{}).
process_frames(_, [], Acc) ->
  Acc;
process_frames(begin_message, Frames, Acc) ->
  wtf(Frames, #message{}, Acc);

process_frames(continue_message, Frames, [FramgmentedMessage | Acc]) ->
  wtf(Frames, FramgmentedMessage, Acc).

wtf([], _, Acc) ->
  Acc;
  
wtf([#frame{ payload = <<>> } | Frames], XMessage, Acc) ->
  %% skip this
  wtf(Frames, XMessage, Acc);

wtf([Frame | Frames], XMessage, Acc) ->
  case process_frame(Frame, XMessage) of
    {fragmented, Message} ->
      process_frames(continue_message, Frames, [Message#message{type = fragmented} | Acc]);
    {completed, Message} ->
      process_frames(begin_message, Frames, [Message | Acc])
  end.

-spec process_frame(Frame :: #frame{}, Message :: #message{})-> {fragmented | completed, #message{}}.
process_frame(Frame, Message) ->
  case contextualize_frame(Frame) of
    continue ->
      Frames = Message#message.frames,
      {fragmented, Message#message{frames = [Frame | Frames]}};
    close ->
      BuiltMessage = build_message(Message, lists:reverse([Frame | Message#message.frames])),
      {completed, BuiltMessage}
  end.

-spec contextualize_frame(Frame :: #frame{}) -> close | continue.
contextualize_frame(Frame) ->
  case {Frame#frame.fin, Frame#frame.opcode} of
    {1, _} -> close;
    {0, _} -> continue
  end.

build_message(Message, Frames) ->
  [HeadFrame | _] = Frames,

  case HeadFrame#frame.opcode of
    1 ->
      Payload = build_payload_from_frames(text, Frames),
      Message#message{type = text, payload = Payload};
    2 ->
      Payload = build_payload_from_frames(binary, Frames),
      Message#message{type = binary, payload = Payload};
    8 ->
      Payload = build_payload_from_frames(close, Frames),
      Message#message{type = close, payload = Payload};
    9 ->
      Payload = build_payload_from_frames(text, Frames),
      Message#message{type = ping, payload = Payload};
    10 ->
      Payload = build_payload_from_frames(text, Frames),
      Message#message{type = pong, payload = Payload}
  end.

build_payload_from_frames(close, [Frame]) ->
  case Frame#frame.payload of
    <<>> -> {undefined, undefined};
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


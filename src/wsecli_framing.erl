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

-module(wsecli_framing).
-include("wsecli.hrl").

-export([to_binary/1, from_binary/1, frame/1, frame/2]).

-define(OP_CODE_CONT, 0).
-define(OP_CODE_TEXT, 1).
-define(OP_CODE_BIN, 2).
-define(OP_CODE_CLOSE, 8).
-define(OP_CODE_PING, 9).
-define(OP_CODE_PONG, 10).

-spec to_binary(Frame::#frame{}) -> binary().
to_binary(Frame) ->
  Bin1 = <<
    (Frame#frame.fin):1,
    (Frame#frame.rsv1):1, (Frame#frame.rsv2):1, (Frame#frame.rsv3):1,
    (Frame#frame.opcode):4,
    (Frame#frame.mask):1,
    (Frame#frame.payload_len):7,
    (Frame#frame.extended_payload_len):(extended_payload_len_bit_width(Frame#frame.extended_payload_len, 16)),
    (Frame#frame.extended_payload_len_cont):(extended_payload_len_bit_width(Frame#frame.extended_payload_len_cont, 64))
  >>,

  Bin2 = case Frame#frame.masking_key of
    undefined -> Bin1;
    Key ->
      <<Bin1/binary, Key:32>>
  end,

  case Frame#frame.payload of
    undefined -> Bin2;
    Payload ->
      <<Bin2/binary, Payload/binary>>
  end.

    %(Frame#frame.masking_key):32,
    %(Frame#frame.payload)/binary

-spec from_binary(Data::binary()) -> list(#frame{}).
from_binary(Data) ->
  lager:info("inside from_binary1 and Data is ~p",[Data]),
  DL = byte_size(Data),
  lager:info("inside from_binary1 and DataSize is ~p",[DL]),
  from_binary(Data, []).

from_binary(<<Head:9, 126:7, PayloadLen:16, Payload:PayloadLen/binary, Rest/binary>>, Acc)->
  lager:info ("inside from_binary2 and PayloadLen is ~p",[PayloadLen]),
  lager:info ("inside from_binary2 and Payload is ~p",[Payload]),
  lager:info ("inside from_binary2 and Rest is ~p",[Rest]),  
  from_binary(Rest, [decode_frame(<<Head:9, 126:7, PayloadLen:16, Payload:PayloadLen/binary>>) | Acc]);

from_binary(<<Head:9, 127:7, PayloadLen:64, Payload:PayloadLen/binary, Rest/binary>>, Acc)->
  lager:info ("inside from_binary3 and PayloadLen is ~p",[PayloadLen]),
  lager:info ("inside from_binary3 and Payload is ~p",[Payload]),
  lager:info ("inside from_binary3 and Rest is ~p",[Rest]),  
  from_binary(Rest, [decode_frame(<<Head:9, 127:7, PayloadLen:64, Payload:PayloadLen/binary>>) | Acc]);

from_binary(<<Head:9, PayloadLen:7, Payload:PayloadLen/binary, Rest/binary>>, Acc) ->
  lager:info ("inside from_binary4 and PayloadLen is ~p",[PayloadLen]),
  lager:info ("inside from_binary4 and Payload is ~p",[Payload]),
  lager:info ("inside from_binary4 and Rest is ~p",[Rest]),  
  from_binary(Rest, [decode_frame(<<Head:9, PayloadLen:7, Payload:PayloadLen/binary>>) | Acc]);

from_binary(Bin, Acc) when is_binary(Bin) ->
  lager:info ("inside from_binary5 and Bin is ~p",[Bin]),  
  {Bin, lists:reverse(Acc)}.


decode_frame(Data = <<Fin:1, Rsv1:1, Rsv2:1, Rsv3:1, Opcode:4, Mask:1, _/bits>> ) ->
  % TODO: ensure that Mask is not set

  Frame = #frame{
    fin = Fin,
    rsv1 = Rsv1, rsv2 = Rsv2, rsv3 = Rsv3,
    opcode = Opcode,
    mask = Mask
  },

  Frame2 = binary_payload_length(Data, Frame),
  binary_payload(Data, Frame2).

-spec binary_payload_length(Data::binary(), Frame::#frame{}) -> #frame{}.
binary_payload_length(Data, Frame) ->
  <<_:9, PayloadLen:7, _/binary>> = Data,
  case PayloadLen of
    126 ->
      <<_:16, ExtendedPayloadLen:16, _/binary>> = Data,
      Frame#frame{payload_len = PayloadLen, extended_payload_len = ExtendedPayloadLen};
    127 ->
      <<_:16, ExtendedPayloadLenCont:64, _/binary>> = Data,
      Frame#frame{payload_len = PayloadLen, extended_payload_len_cont = ExtendedPayloadLenCont};
    _ ->
      Frame#frame{payload_len = PayloadLen}
  end.

-spec binary_payload(Data::binary(), Frame::#frame{}) -> #frame{}.
binary_payload(Data, Frame) ->
  case Frame#frame.payload_len of
    126 ->
      <<_:32, Payload/binary>> = Data;
    127 ->
      <<_:80, Payload/binary>> = Data;
    _ ->
      <<_:16, Payload/binary>> = Data
  end,

  case Frame#frame.opcode of
    _ ->
      Frame#frame{ payload = Payload }
  end.


extended_payload_len_bit_width(PayloadLen, Max) ->
  case PayloadLen of
    0 -> 0;
    _ -> Max
  end.

-spec frame(Data::binary() | string()) -> #frame{}.
frame(Data) when is_binary(Data) ->
  frame(Data, [{opcode, binary}]);

frame(Data) when is_list(Data)->
  frame(list_to_binary(Data), [{opcode, text}]).

-spec frame(Data::string() | binary(), Options::list()) -> #frame{}.
frame(Data, Options) when is_list(Data) ->
  frame(list_to_binary(Data), Options);

%don't like having this function clause just for close frames
frame({CloseCode, Reason}, Options) ->
  BinReason = list_to_binary(Reason),
  Data = <<CloseCode:16, BinReason/binary>>,
  frame(Data, Options);

frame(Data, Options) ->
  Frame = #frame{},
  Frame2 = length(Frame, Data),
  Frame3 = mask(Frame2, Data),
  apply_options(Frame3, Options).

-spec apply_options(Frame::#frame{}, Options::list()) -> #frame{}.
apply_options(Frame, [fin | Tail]) ->
  T = Frame#frame{fin = 1},
  apply_options(T, Tail);

apply_options(Frame, [{opcode, continuation} | Tail]) ->
  T = Frame#frame{opcode = ?OP_CODE_CONT},
  apply_options(T, Tail);

apply_options(Frame, [{opcode, text} | Tail]) ->
  T = Frame#frame{opcode = ?OP_CODE_TEXT},
  apply_options(T, Tail);

apply_options(Frame, [{opcode, binary} | Tail]) ->
  T = Frame#frame{opcode = ?OP_CODE_BIN},
  apply_options(T, Tail);

apply_options(Frame, [{opcode, close} | Tail]) ->
  T = Frame#frame{opcode = ?OP_CODE_CLOSE},
  apply_options(T, Tail);

apply_options(Frame, [{opcode, ping} | Tail]) ->
  T = Frame#frame{opcode = ?OP_CODE_PING},
  apply_options(T, Tail);

apply_options(Frame, [{opcode, pong} | Tail]) ->
  T = Frame#frame{opcode = ?OP_CODE_PONG},
  apply_options(T, Tail);

apply_options(Frame, []) ->
  Frame.

-spec length(Frame::#frame{}, Data :: binary()) -> #frame{}.
length(Frame, Data) ->
  %Len = string:len(Data),
  Len = byte_size(Data),
  if
    Len =< 125 ->
      Frame#frame{
        payload_len = Len,
        extended_payload_len = 0,
        extended_payload_len_cont = 0
      };
    (Len > 125) and (Len =< 65536) ->
      Frame#frame{
        payload_len = 126,
        extended_payload_len = Len,
        extended_payload_len_cont = 0
      };
    Len > 65536 ->
      Frame#frame{
        payload_len = 127,
        extended_payload_len = 0,
        extended_payload_len_cont = Len
      }
  end.

-spec mask(Frame::#frame{}, Data::binary()) -> #frame{}.
mask(Frame, <<>>) ->
  Frame#frame{mask = 0};

mask(Frame, Data) ->
  <<MaskKey:32>> = crypto:rand_bytes(4),
  %BinData = list_to_binary(Data),

  Frame#frame{
    mask = 1,
    masking_key = MaskKey,
    payload = mask(Data, MaskKey, <<>>)
  }.


%
% Masking code got at Cowboy source code
%
-spec mask(Data::binary(), MaskKey::integer(), Acc::binary()) -> binary().
mask(<<Data:32, Rest/bits>>, MaskKey, Acc) ->
  T = Data bxor MaskKey,
  mask(Rest, MaskKey, <<Acc/binary, T:32>>);

mask(<<Data:24>>, MaskKey, Acc) ->
  <<MaskKey2:24, _/bits>> = <<MaskKey:32>>,
  T = Data bxor MaskKey2,
  <<Acc/binary, T:24>>;

mask(<<Data:16>>, MaskKey, Acc) ->
  <<MaskKey2:16, _/bits>> = <<MaskKey:32>>,
  T = Data bxor MaskKey2,
  <<Acc/binary, T:16>>;

mask(<<Data:8>>, MaskKey, Acc) ->
  <<MaskKey2:8, _/bits>> = <<MaskKey:32>>,
  T = Data bxor MaskKey2,
  <<Acc/binary, T:8>>;

mask(<<>>, _, Acc) ->
  Acc.

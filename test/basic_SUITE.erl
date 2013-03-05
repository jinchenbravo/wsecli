-module(basic_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("wsecli.hrl").
-export([all/0]).
-export([encode_test/1]).

all() -> [encode_test].

-spec encode_test(_Config) -> ok.
encode_test(_Config)->
	
	B1 = <<137>>,
	B2 = wsecli_message:encode(<<137>>,binary),
	io:format("B2 is ~p",[B2]).



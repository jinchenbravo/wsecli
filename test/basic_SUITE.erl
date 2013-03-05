-module(basic_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("wsecli.hrl").
-export([all/0]).
-export([encode_test/1]).

all() -> [encode_test].

-spec encode_test(_Config) -> ok.
encode_test(_Config)->
	mock_http_server:start(self(), 8081).


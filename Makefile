REBAR = ./rebar
DIALYZER = dialyzer
TOUCH = touch

.PHONY: all test clean

all: deps compile

deps: get-deps compile-deps

compile:
	@$(REBAR) compile skip_deps=true

escriptize: 
	@$(REBAR) escriptize

clean:
	@$(REBAR) clean

get-deps:
	@$(REBAR) get-deps

compile-deps:
	@$(REBAR) compile
test: compile eunit
	@$(REBAR) ct
eunit: compile
	@$(REBAR) skip_deps=true eunit


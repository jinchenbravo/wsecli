REBAR = ./rebar
DIALYZER = dialyzer
TOUCH = touch

all: deps compile

deps: get-deps compile-deps

compile:
	@$(REBAR) compile skip_deps=true

escriptize: 
	@$(REBAR) escriptize

clean: testclean
	@$(REBAR) clean

get-deps:
	@$(REBAR) get-deps

compile-deps:
	@$(REBAR) compile

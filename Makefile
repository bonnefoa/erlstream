REBAR=./rebar

all: compile

deps:
	@$(REBAR) get-deps

compile: deps
	@$(REBAR) compile

eunit:
	@$(REBAR) eunit

clean:
	@$(REBAR) clean

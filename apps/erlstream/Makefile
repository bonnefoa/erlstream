REBAR=./rebar
ERL=erl
EBIN=ebin
EBIN_TEST=ebin_test
MOCHI=deps/mochiweb/ebin
OAUTH=deps/oauth/ebin

all: compile

deps:
	@$(REBAR) get-deps

compile: deps
	@$(REBAR) compile

eunit:
	@$(REBAR) eunit

clean:
	@$(REBAR) clean

console:compile
	${ERL} -pa ${EBIN} -pa ${OAUTH} -pa ${MOCHI} -pa ${EBIN_TEST}

launch:compile
	${ERL} -pa ${EBIN} -pa ${OAUTH} -pa ${MOCHI} -pa ${EBIN_TEST} -erlstream conf '"priv/config.txt"' -noshell -eval 'erlstream:start()' 

compile_test:
	$(ERL) -noinput -eval "case make:all() of up_to_date -> halt(0); _ -> halt(1) end."

test:compile compile_test
	${ERL} -pa ${EBIN} -pa ${OAUTH} -pa ${MOCHI} -pa ${EBIN_TEST} -noshell -eval 'eunit:test("${EBIN_TEST}", [verbose])' -s init stop

build_plt:
	dialyzer --build_plt                 \
		--output_plt .dialyzer-R14B01.plt  \
		--apps kernel stdlib sasl erts ssl \
		tools os_mon runtime_tools crypto  \
	inets xmerl webtool snmp public_key  \
	mnesia eunit syntax_tools compiler   

check_plt:build_plt
	dialyzer ./ebin --plt .dialyzer-R14B \
	-Wunmatched_returns                  \
		-Werror_handling                   \
		-Wrace_conditions                  \
		-Wbehaviours                       \
		-Wunderspecs


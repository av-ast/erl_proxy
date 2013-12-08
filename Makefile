all:
	./rebar compile

no_deps:
	./rebar compile skip_deps=true

full:
	./rebar get-deps compile

clean:
	./rebar clean

test:
	./rebar eunit skip_deps=true

run:
	erl -pa ebin -pa deps/*/ebin \
		-s erl_proxy_app -sname erl_proxy_node -smp enable \
		-lager colored true \
					 handlers '[{lager_console_backend, debug}]' \
		-sasl errlog_type error

PLT_NAME=.erl_proxy.plt

$(PLT_NAME):
	@ERL_LIBS=deps dialyzer --build_plt --output_plt $@ --apps \
		erts kernel stdlib crypto ssl xmerl syntax_tools \
		public_key compiler \
		deps/*/ebin

dialyze: $(PLT_NAME)
	@dialyzer --plt $(PLT_NAME) --no_native \
		-Werror_handling -Wunderspecs \
		ebin

.PHONY: all no_deps full clean test run dialyze

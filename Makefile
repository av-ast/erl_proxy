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

.PHONY: all no_deps full clean test run

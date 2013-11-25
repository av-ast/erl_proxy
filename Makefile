all:
	./rebar compile

no_deps:
	./rebar compile skip_deps=true

full:
	./rebar get-deps compile

clean:
	./rebar clean

.PHONY: test

test:
	./rebar eunit skip_deps=true

run:
	erl -pa ebin -pa deps/*/ebin -s erl_proxy_app -sname erl_proxy_node -smp enable


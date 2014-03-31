all: get_deps compile release

test:
	./rebar eunit skip_deps=true

get_deps:
	./rebar get-deps

compile:
	./rebar compile

no_deps:
	./rebar compile skip_deps=true

release:
	./rebar generate

clean:
	rm -rf ebin deps rel/erl_proxy

run:
	erl -pa ebin -pa deps/*/ebin \
		-config config/sys.config \
		-sname erl_proxy -s erl_proxy_app -s sync go

.PHONY: test get-deps compile no_deps release clean all run

PROJECT = erl_proxy
DEPS = cowboy lager eredis lhttpc uri sync

dep_cowboy = pkg://cowboy master
dep_lager = https://github.com/basho/lager.git 2.0.1
dep_eredis = https://github.com/wooga/eredis.git
dep_lhttpc = https://github.com/esl/lhttpc.git
dep_uri = https://github.com/av-ast/uri.git
dep_sync = https://github.com/rustyio/sync.git

ERLC_OPTS += +'{parse_transform, lager_transform}'

include erlang.mk

test:
	./rebar eunit skip_deps=true

no_deps:
	./rebar compile skip_deps=true

run:
	erl -pa ebin -pa deps/*/ebin \
		-config config/sys.config \
		-sname erl_proxy -s erl_proxy_app start_with_deps

.PHONY: test no_deps run

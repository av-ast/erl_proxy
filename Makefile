PROJECT = erl_proxy
DEPS = cowboy lager eredis lhttpc uri

dep_cowboy = pkg://cowboy master
dep_lager = https://github.com/basho/lager.git 2.0.1
dep_eredis = https://github.com/wooga/eredis.git
dep_lhttpc = https://github.com/esl/lhttpc.git
dep_uri = https://github.com/av-ast/uri.git

# ERLC_OPTS += +'{parse_transform, eunit_autoexport}'

include erlang.mk

test:
	./rebar eunit skip_deps=true

.PHONY: all no_deps full clean test run dialyze xref

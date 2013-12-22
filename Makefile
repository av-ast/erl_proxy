PROJECT = erl_proxy

DEPS = cowboy eredis lhttpc lager uri

dep_cowboy = https://github.com/extend/cowboy.git
dep_lhttpc = https://github.com/esl/lhttpc.git
dep_lager = https://github.com/basho/lager.git
dep_eredis = https://github.com/wooga/eredis.git
dep_uri = https://github.com/av-ast/uri.git

include erlang.mk

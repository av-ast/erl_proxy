-module(client_request_handler).

%% Standard callbacks.
-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

%% Custom callbacks.
-export([create_client_request/2]).
-export([get_client_requests/2]).

-define(P(Data), io:format("~n~p~n", [Data])).

init({tcp,http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"HEAD">>], Req, State}.

content_types_provided(Req, State) ->
	{[{<<"application/json">>, get_client_requests}], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, create_client_request}],
		Req, State}.

get_client_requests(Req, State) ->
	Body = <<"{\"rest\": \"GET request accepted!\"}">>,
	{Body, Req, State}.

create_client_request(Req, State) ->
	{ok, [{Body, true}], Req2} = cowboy_req:body_qs(Req),
  ?P(Body),
  ?P(jiffy:decode(Body)),
	case cowboy_req:method(Req2) of
		{<<"POST">>, Req3} ->
			{{true, <<"BLAH BLAH">>}, Req3, State};
		{_, Req3} ->
			{true, Req3, State}
	end.



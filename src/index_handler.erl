-module(index_handler).
-behaviour(cowboy_handler).

-export([init/2]).
-export([terminate/3]).

init(Req, Opts) ->
    {ok, HTML} = index_dtl:render([]),
    Req2 = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/html">>}],
        HTML,
        Req),
    {ok, Req2, Opts}.

terminate(_Reason, _Req, _Opts) ->
    ok.

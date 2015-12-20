-module(error404_handler).
-behaviour(cowboy_handler).

-export([init/2]).
-export([terminate/3]).


init(Req, Opts) ->
    	{ok, HTML} = error404_dtl:render([]),
    	Req2 = cowboy_req:reply(404, [{<<"content-type">>, <<"text/html">>}], HTML, Req),
    {ok, Req2,  Opts}.


terminate(_Reason, _Req, _State) ->
	ok.

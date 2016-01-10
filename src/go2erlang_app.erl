-module(go2erlang_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [	
		{"/user/create", user_add_handler, []},
		{"/user/:email", user_handler, []},
		{"/", index_handler, []},
		{'_', error404_handler, []}
	]}
    ]),
    {ok, _} = cowboy:start_http(my_http_listener, 100, [{port, 8080}],[
		{env, [
			{dispatch, Dispatch}
		]}, {onresponse, fun error_hook/4}
	]),
    go2erlang_sup:start_link().

stop(_State) ->
	ok.



error_hook(401, Headers, <<>>, Req) ->
	Body = login_dtl:render([]),
	Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
		{<<"content-length">>, integer_to_list(iolist_size(Body))}),
	cowboy_req:reply(200, Headers2, Body, Req);
error_hook(404, Headers, <<>>, Req) ->
	Path = cowboy_req:path(Req),
	Body = ["404 Not Found: \"", Path,
		"\" is not the path you are looking for.\n"],
	Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
		{<<"content-length">>, integer_to_list(iolist_size(Body))}),
	cowboy_req:reply(404, Headers2, Body, Req);
error_hook(Code, Headers, <<>>, Req) when is_integer(Code), Code >= 400 ->
	Body = ["HTTP Error ", integer_to_list(Code), $\n],
	Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
		{<<"content-length">>, integer_to_list(iolist_size(Body))}),
	cowboy_req:reply(Code, Headers2, Body, Req);
error_hook(_Code, _Headers, _Body, Req) ->
	Req.

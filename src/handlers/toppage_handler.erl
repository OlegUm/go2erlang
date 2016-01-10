%% Feel free to use, reuse and abuse the code in this file.

%% @doc Cookie handler.
-module(toppage_handler).

-export([init/2]).

init(Req, Opts) ->
	NewValue = integer_to_list(random:uniform(1000000)),
	io:format("toppage_handler NewValue  = ~p~n",[NewValue]), %% Debug
	
	io:format("toppage_handler. Headers=~p~n",[cowboy_req:headers(Req)]), %% Debug 
	io:format("toppage_handler. Req  = ~p~n",[Req]), %% Debug
	
	Req2 = cowboy_req:set_resp_cookie(
		<<"server">>, NewValue, [{path, <<"/">>}], Req),
	#{client := ClientCookie, server := ServerCookie}
		= cowboy_req:match_cookies([{client, [], <<>>}, {server, [], <<>>}], Req2),


	io:format("toppage_handler. Headers2=~p~n",[cowboy_req:headers(Req2)]), %% Debug 
	io:format("toppage_handler. Req2  = ~p~n",[Req2]), %% Debug
	io:format("toppage_handler ServerCookie  = ~p~n",[ServerCookie]), %% Debug

	{ok, Body} = toppage_dtl:render([
		{client, ClientCookie},
		{server, ServerCookie}
	]),
	

	Req3 = cowboy_req:reply(200,
		[{<<"content-type">>, <<"text/html">>}],
		Body, Req2),
	
	io:format("toppage_handler. Headers3=~p~n",[cowboy_req:headers(Req3)]), %% Debug 
	io:format("toppage_handler. Req3  = ~p~n",[Req3]), %% Debug

	{ok, Req3, Opts}.

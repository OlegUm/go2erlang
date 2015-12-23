-module(user_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([allowed_methods/2]).
-export([user/2]).


init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.


content_types_provided(Req, State) ->
	{[
		{{<<"text">>, <<"html">>, '*'}, user}, 
		{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, user}
	
	], Req, State}.


%% методы с которыми работает контроллер
allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}.

%% открываем форму регистрации пользователя
 user(Req, State) ->
		io:format("user handler ... ~n"),
		Email=cowboy_req:binding(email, Req, "No email"),
		io:format("email = ~p~n",[Email]),
		User=users:get([Email]),
		io:format("user/email = ~p~p~p~n",[User,"/",Email]),
		{ok, Body} = user_dtl:render([{email,Email},{user,User}]),
	{Body, Req, State}.



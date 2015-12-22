-module(user_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([allowed_methods/2]).
-export([create_user/2]).
-export([reg_form/2]).


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
		io:format("user = ~p~n",[State]),
		{ok, Body} = user_dtl:render([]),
	{Body, Req, State}.



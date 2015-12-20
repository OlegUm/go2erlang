-module(user_add_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([allowed_methods/2]).
-export([create_user/2]).
-export([reg_form/2]).


init(Req, Opts) ->
	io:format("Init User_add_handler~n"),
	{cowboy_rest, Req, Opts}.


content_types_provided(Req, State) ->
	{[
		{{<<"text">>, <<"html">>, '*'}, reg_form}, 
		{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, reg_form}
	
	], Req, State}.

%% получаем данные от формы регистрации
content_types_accepted(Req, State) ->
	{[
		{{<<"text">>, <<"html">>, '*'}, create_user},
		{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, create_user}	

	], Req, State}.

%% методы с которыми работает контроллер
allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}.

%% открываем форму регистрации пользователя
 reg_form(Req, State) ->
		io:format("reg_form, State = ~p~n",[State]),
		{ok, Body} = user_add_dtl:render([]),
	{Body, Req, State}.

%% Функция обработает входящий POST запрос
create_user(Req, State) ->		                                	
		{ok, PostVals, Req2} = cowboy_req:body_qs(Req), %% Выделяем запрос

		%% для этого куска кода есть менее красивое но более очевидное решение 
		%% см. http://stackoverflow.com/questions/16053870/erlang-proplistget-value-2-or-pattern-matching
		%% Name = proplists:get_value(<<"name">>, PostVals),
		%% Email = proplists:get_value(<<"email">>, PostVals),
		[Email, Name] = [ V || Key <- [<<"email">>,<<"name">>], {K, V} <- PostVals, K =:= Key ],
		io:format("Name/Email = ~p~p~p~n",[Name, "/", Email]),

	{true, Req2, State}.

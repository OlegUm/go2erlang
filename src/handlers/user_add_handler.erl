-module(user_add_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([allowed_methods/2]).
-export([create_user/2]).
-export([reg_form/2]).

-include("debug.hrl").

init(Req, Opts) ->
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
		{ok, Body} = user_add_dtl:render([]),
	{Body, Req, State}.

%% Функция обработает входящий POST запрос
create_user(Req, State) ->		                                	
		{ok, PostVals, Req2} = cowboy_req:body_qs(Req), %% Выделяем запрос

		%% для этого куска кода есть менее красивое но более очевидное решение 
		%% см. http://stackoverflow.com/questions/16053870/erlang-proplistget-value-2-or-pattern-matching
		%% Name = proplists:get_value(<<"name">>, PostVals),
		%% Email = proplists:get_value(<<"email">>, PostVals),
		
		case users:add([ V || Key <- [<<"email">>,<<"name">>], {K, V} <- PostVals, K =:= Key ]) of
			{ok,Email} -> 
			      Path = <<"/user/">>,
			      Path_and_mail = << Path/binary, Email/binary >>,
			      {ok, Req3} = cowboy_session:set(<<"authorized">>, 1, Req2),
			      {{true, Path_and_mail}, Req3, State};
			{false, already_exist} -> 
			{ok, Body} = user_add_dtl:render([{already_exist, <<"Такой пользователь существует!"/utf8>>}]),
      			Req3 = cowboy_req:set_resp_body(Body, Req2),
			{false, Req3, State} 
		end.
	

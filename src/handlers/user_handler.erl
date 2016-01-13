-module(user_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([allowed_methods/2]).
-export([user/2]).
-export([is_authorized/2]).
-export([post_user/2]).

-include("debug.hrl").


init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.


content_types_provided(Req, State) ->
	{[
		{{<<"text">>, <<"html">>, '*'}, user}, 
		{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, user}
	
	], Req, State}.

content_types_accepted(Req, State) ->
	{[
		{{<<"text">>, <<"html">>, '*'}, post_user},
		{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, post_user}	

	], Req, State}.


%% методы с которыми работает контроллер
allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}.

%% проверяем, авторизован ли пользователь
is_authorized(Req, State) ->	
	{ok, PostVals, Req2} = cowboy_req:body_qs(Req), %% Выделяем запрос
	EmailPassword= [ V || Key <- [<<"email">>,<<"password">>], {K, V} <- PostVals, K =:= Key ],

	?DBG("EmailPassword=~p~n",EmailPassword), 	

	case EmailPassword of
		[_|_] ->   %% Если в запросе есть логин-пароль
			?DBG("~n",""), 	
			WfP= users:verify_password(EmailPassword),
			?DBG("~n",""), 
			case WfP of
				ok -> 
					{ok,Req3}=cowboy_session:set(<<"authorized">>,1, Req2),
				      	{true, Req3, State};
				_ ->   {{false, <<"401 Unauthorized">>}, Req2, State}
			end;
		[] ->  	%% Если в запросе нет пароля	
			{Value, Req3} = cowboy_session:get(<<"authorized">>, Req2),
			case Value of
				1 -> {true, Req3, State};
				_ -> {{false, <<"401 Unauthorized">>}, Req3, State}
			end
	end.


%% открываем форму регистрации пользователя
 user(Req, State) ->
		Email=cowboy_req:binding(email, Req, "No email"),
		User=users:get([Email]),
		{ok, Body} = user_dtl:render([{email,Email},{user,User}]),
	{Body, Req, State}.


post_user(Req, State) ->
    	{{true,cowboy_req:path(Req)}, Req, State}.



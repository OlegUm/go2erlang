%% @doc Модуль работы с пользователями. Определяет функции добавления и удаления пользователей.
%% В модуле используется БД PostgreSQL
%% Модуль организован как gen_server


-module(users).
-behaviour(gen_server).
-author("Oleg Um <olan-ol@yandex.ru").


%% функции обратного вызова gen_server 
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([start_link/0]).

%% экспорт функций API модуля
-export([	
		add/1,
		get/1,
		verify_password/2
	]).


%% Передаем вызовы функций API модуля процессу  gen_server
-spec start_link() -> {ok, pid()}.
start_link() -> gen_server:start_link({local,?MODULE},?MODULE, [],[]).

add(User) -> gen_server:call(?MODULE, {add,User}). %% Добавляем пользователя в базу
get(Email) -> gen_server:call(?MODULE, {get,Email}). %% Получаем пользователя из базы по его мейлу


%% gen_server.
init([]) ->
	C = pgsql_connection:open("users_db", "oleg", "qwerty"),
	io:format("init. Connect = ~p~n",[C]),
	{ok, C}.


handle_call({add,User}, _From, C) ->		
		Reply= add_(C ,User),
		{reply, Reply, C};
handle_call({get,Email}, _From, C) ->
		Reply= get_(C, Email),
		{reply, Reply, C}.
handle_call({verify_password,Email,Password}, _From, C) ->
		Reply= verify_password_(C, Email, Password),
		{reply, Reply, C}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



%% @doc Добавление пользователя
%%    @spec( add_(User::[ binary() ] ) -> {false, already_exist} | {ok, binary()}  ).
      -spec( add_(any(),User::[ binary() ] ) -> {false, already_exist} | {ok, binary()}  ).
add_(C,[Email,Name]) -> 
		%% Сгенерим случайный числовой пароль
		Password=rand:uniform(100000),
		
		%% Добавим запись в базу
		case add_bd(C,[Email,Name,Password]) of
			{{insert,0,1},[]} -> 
				%% Если пользователь новый - отсылаем mail
				[Pass_string]=io_lib:format("~p",[Password]),		
				Email_body = unicode:characters_to_binary([
					<<"Subject: Ваш пароль \r\nFrom: Test server <test@test.com> \r\nTo:"/utf8>>, Name, 
					<<"<"/utf8>>, Email, 
					<<"> \r\nContent-Type: text/plain;\r\n\t charset=utf-8 \r\n\r\n Ваш пароль = "/utf8>>,
					Pass_string
				], unicode, utf8),
			  	e_mail:send({<<"test@test.com">>, [Email],Email_body}),
				{ok, Email};
			{error,{pgsql_error,[_, {code,<<"23505">>}|_Tail]}} ->
        			%% Если пользователь уже существует
			{false, already_exist}
		end.



%% @doc Запись пользователя в БД
add_bd(C,[Email,Name,Password]) -> 
	pgsql_connection:extended_query("INSERT INTO users (email, name, password) VALUES ($1, $2, $3)", [Email,Name,Password], C).

get_(C, Email) ->
	case pgsql_connection:extended_query("SELECT * FROM users WHERE email = $1",[Email],C) of
		{{select,0},[]} -> not_exist;
		{{select,1},[{_,_,User,_}]} -> User
	end.

verify_password_(C, Email,Password) ->
	case pgsql_connection:extended_query("SELECT * FROM users WHERE email = $1 and password = $1",[Email,Password],C) of
		{{select,0},[]} -> not_exist;
		{{select,1},[_]} -> ok
	end.		

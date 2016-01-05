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
		get/1
	]).


%% Передаем вызовы функций API модуля процессу  gen_server
-spec start_link() -> {ok, pid()}.
start_link() -> gen_server:start_link({local,?MODULE},?MODULE, [],[]).

add(User) -> gen_server:call(?MODULE, {add,User}). %% Добавляем пользователя в базу
get(Email) -> gen_server:call(?MODULE, {get,Email}). %% Получаем пользователя из базы по его мейлу


%% gen_server.
init([]) ->
	{ok,C} = epgsql:connect("localhost", "oleg", "qwerty", [{database, "users_db"}, {timeout, 4000}]),
	io:format("init. Connect = ~p~n",[C]),
	{ok, C}.

%%handle_call({reset_all}, _From, State) ->
%%		Reply= reset_all_(),
%%		{reply, Reply, State};
handle_call({add,User}, _From, State) ->		
		Reply= add_(State ,User),
		{reply, Reply, State};
handle_call({get,Email}, _From, State) ->
		Reply= get_(State, Email),
		{reply, Reply, State}.


handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



%% @doc Добавление пользователя
%%    @spec( add(User::[ binary() ] ) -> {false, already_exist} | {ok, binary()}  ).
%%      -spec( add_(User::[ binary() ] ) -> {false, already_exist} | {ok, binary()}  ).
add_(C,[Email,Name]) -> 

		%% Сгенерим случайный числовой пароль
		Password=rand:uniform(100000),
		
		%% Добавим запись в базу
		case add_bd(C,[Email,Name,Password]) of
			{ok,_} -> 
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
			{error,{error,_,<<"23505">>, _, _}} ->
        			%% Если пользователь уже существует
			{false, already_exist}
		end.



%% @doc Запись пользователя в БД
add_bd(C,[Email,Name,Password]) -> 
	epgsql:equery(C, "INSERT INTO users (email, name, password) VALUES ($1, $2, $3)",[Email,Name,Password]).

get_(C, Email) ->
	case epgsql:equery(C, "SELECT * FROM users WHERE email = $1",[Email]) of
		{ok, _, []} -> not_exist;
		{ok, _, [{_, _, User,_}]} -> User
	end.		

	








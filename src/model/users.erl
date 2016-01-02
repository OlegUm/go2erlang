%% @doc Модуль работы с пользователями. Определяет функции добавления и удаления пользователей.
%% В модуле используется БД Mnesia
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
-export([	reset_all/0,
		add/1,
		get/1,
		check_password/1
	]).


%% определение структуры БД пользователей
-record (user, {email	::binary(),
		name	::binary(), 
		password::number() }).

-record(state, {
}).



%% эта библиотека нужна для работы функции ets:fun2ms
-include_lib("stdlib/include/ms_transform.hrl").


%% Передаем вызовы функций API модуля процессу 
-spec start_link() -> {ok, pid()}.
start_link() -> gen_server:start_link({local,?MODULE},?MODULE, [],[]).

reset_all() -> gen_server:call(?MODULE, {reset_all}). %% Очищаем базу
%% start() -> gen_server:call(?MODULE, {start}). %% Запускаем базу
add(User) -> gen_server:call(?MODULE, {add,User}). %% Добавляем пользователя в базу
get(Email) -> gen_server:call(?MODULE, {get,Email}). %% Получаем пользователя из базы по его мейлу
check_password(Email_Password) -> gen_server:call(?MODULE, {get_user,Email_Password}). %% Проверяем соответствие мейла и пароля



%% gen_server.

init([]) ->
	io:format("users:start([])~n"),
	{ok, #state{}}.

handle_call({reset_all}, _From, State) ->
		Reply= reset_all_(),
		{reply, Reply, State};
handle_call({add,User}, _From, State) ->		
		Reply= add_(User),
		{reply, Reply, State};
handle_call({get,Email}, _From, State) ->
		Reply= get_(Email),
		{reply, Reply, State};
handle_call({check_password, Email_Password}, _From, State) ->
		Reply= check_password_(Email_Password),
		{reply, Reply, State}.


handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.










%% @doc Функция обнуления базы данных
%% @spec( reset_all() -> {atomic,ok} ).
-spec( reset_all_() -> {atomic,ok} ).	
reset_all_()->
	stopped=mnesia:stop(),
   	ok=mnesia:delete_schema([node()]),
    	ok=mnesia:create_schema([node()]),
    	ok=mnesia:start(),
	{atomic,ok}=mnesia:create_table(user,[{attributes,record_info(fields,user)},
                           {disc_copies,[node()]},
                           {type,set}]).





%% @doc Добавление пользователя
%%    @spec( add(User::[ binary() ] ) -> {false, already_exist} | {ok, binary()}  ).
      -spec( add_(User::[ binary() ] ) -> {false, already_exist} | {ok, binary()}  ).
add_([Email,Name]) -> 

		%% Сгенерим случайный числовой пароль
		Password=rand:uniform(100000),
		
		%% Добавим запись в базу
		case add_bd([Email,Name,Password]) of
			{atomic,{atomic,ok}} -> %% Если пользователь новый - отсылаем mail
				[Pass_string]=io_lib:format("~p",[Password]),		
				Email_body = unicode:characters_to_binary([
					<<"Subject: Ваш пароль \r\nFrom: Test server <test@test.com> \r\nTo:"/utf8>>, Name, 
					<<"<"/utf8>>, Email, 
					<<"> \r\nContent-Type: text/plain;\r\n\t charset=utf-8 \r\n\r\n Ваш пароль = "/utf8>>,
					Pass_string
				], unicode, utf8),
			  	e_mail:send({<<"test@test.com">>, [<<"olan-ol@yandex.ru">>],Email_body}),
				{ok, Email};
			{atomic,{already_exist,_}} -> {false, already_exist}
		end.

%% @doc Получить пользователя по его мейлу
%% @spec( get(Email:: [ binary()] ) -> not_exist | binary() ).
   -spec( get_(Email:: [binary()] ) -> not_exist | binary() ).

get_([Email]) ->
	
%% @doc Поиск совпадения мейла 
MPw = fun(Ne) -> 
        mnesia:select(user,ets:fun2ms(fun(#user{email=E,name=N,password=P}) when  E =:= Ne -> {E,N,P} end))
    end,
 
	{atomic,R} = mnesia:transaction(MPw,[Email]),
        case R of
          [] -> not_exist;
          [{Email,User,_}] -> User
        end.





%% @doc Функция проверки пароля пользователя
%% @spec(check_password([ binary() | number() ]) -> ok | not_exist).
   -spec(check_password([ binary() | number() ]) -> ok | not_exist).

check_password_([Email, Password]) ->
	
%% @doc Поиск совпадения мейла и пароля
MPw = fun(Ne,Pw) -> 
        mnesia:select(user,ets:fun2ms(fun(#user{email=E,name=N,password=P}) when  E =:= Ne, P =:= Pw -> {E,N,P} end))
    end,
	{atomic,R} = mnesia:transaction(MPw,[Email,Password]),
        case R of
          [] -> not_exist;
          R -> ok
        end.



%% @doc Запись пользователя в БД
add_bd([Email,Name,Password]) -> 

%% Запустим БД. Если БД уже запущена - эта операция ничего не изменит.
%% ok=mnesia:start(),

%% Функция записи
Wr = fun(E,N,P) -> mnesia:write(#user{email=E,name=N,password=P}) end,

%% Поиск мейла
M = fun(Ne) -> 
        mnesia:select(user,ets:fun2ms(fun(#user{email=E,name=N,password=P}) when  E =:= Ne -> {E,N,P} end))
    end,

%% Функция добавления пользователя с проверкой уникальности
Add = fun(E,N,P) ->
       		 {atomic,R} = mnesia:transaction(M,[E]),
        case R of
          [] -> mnesia:transaction(Wr,[E,N,P]);
          R -> {already_exist,R}
        end
       end,
%% 
mnesia:transaction(Add,[Email,Name,Password]).





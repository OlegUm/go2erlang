-module(users).
-author("Oleg Um <olan-ol@yandex.ru").

%% определение структуры БД пользователей
-record (user, {email, name, password}).

%% эта библиотека нужна для работы функции ets:fun2ms
-include_lib("stdlib/include/ms_transform.hrl").

-export([	reset_all/0,
		add/1,
		check_password/1
	]).

	
reset_all()->
	stopped=mnesia:stop(),
   	ok=mnesia:delete_schema([node()]),
    	ok=mnesia:create_schema([node()]),
    	ok=mnesia:start(),
	{atomic,ok}=mnesia:create_table(user,[{attributes,record_info(fields,user)},
                           {disc_copies,[node()]},
                           {type,set}]).

%% Добавление пользователя
add([Email,Name]) -> 

		%% Сгенерим случайный числовой пароль
		Password=rand:uniform(100000),
		
		%% Добавим запись в базу
		case add_bd([Email,Name,Password]) of
			{atomic,{atomic,ok}} -> 
				%% Отсылаем mail		
				Email_body = unicode:characters_to_binary([
					<<"Subject: Ваш пароль \r\nFrom: Test server <test@test.com> \r\nTo:"/utf8>>, Name, 
					<<"<"/utf8>>, Email, 
					<<"> r\n\Content-Type: text/plain;\r\n\t charset=utf-8 \r\n\r\n Ваш пароль = "/utf8>>,
					Pass_string
				], unicode, utf8),
			  e_mail:send({<<"test@test.com">>, [<<"olan-ol@yandex.ru">>],Email_body}),
			ok;
			{atomic,{already_exist,_}} -> already_exist
		end.




%% Функция проверки пароля пользователя
%% -spec(check_passwor([Email::binary(), Password::string()]) -> ok | not_exist).
check_password([Email,Password]) ->
	
%% Поиск совпадения мейла и пароля
MPw = fun(Ne,Pw) -> 
        mnesia:select(user,ets:fun2ms(fun(#user{email=E,name=N,password=P}) when  E =:= Ne, P =:= Pw -> {E,N,P} end))
    end,
 
	{atomic,R} = mnesia:transaction(MPw,[Email,Password]),
        case R of
          [] -> not_exist;
          R -> ok
        end.



%% Запись пользователя в БД
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





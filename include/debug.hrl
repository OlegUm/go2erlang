
%% Если включить этот макрос - включится выдача функций DBG
-define(debug,1). 

%% DBG макрос обрабатывает до 6 переменных
-ifdef(debug).

-define(DBG(Def,X), 
	{current_function, {_, Fn, Ar}} = process_info(self(), current_function),
	format_utc_timestamp(),
	io:format(" DBG-> ~p:~p/~p line.~p~n  --> "++Def,[?MODULE, Fn, Ar,?LINE,X])).
-define(DBG(Def,X,Y), 
	format_utc_timestamp(),
	{current_function, {_, Fn, Ar}} = process_info(self(), current_function),
	io:format(" DBG-> ~p:~p/~p line.~p~n  --> "++Def,[?MODULE, Fn, Ar,?LINE,X,Y])).
-define(DBG(Def,X,Y,Z), 
	format_utc_timestamp(),
	{current_function, {_, Fn, Ar}} = process_info(self(), current_function),
	io:format(" DBG-> ~p:~p/~p line.~p~n  --> "++Def,[?MODULE, Fn, Ar,?LINE,X,Y,Z])).
-define(DBG(Def,X,Y,Z,F),
	format_utc_timestamp(), 
	{current_function, {_, Fn, Ar}} = process_info(self(), current_function),
	io:format(" DBG-> ~p:~p/~p line.~p~n  --> "++Def,[?MODULE, Fn, Ar,?LINE,X,Y,Z,F])).
-define(DBG(Def,X,Y,Z,F,D),
	format_utc_timestamp(), 
	{current_function, {_, Fn, Ar}} = process_info(self(), current_function),
	io:format(" DBG-> ~p:~p/~p line.~p~n  --> "++Def,[?MODULE, Fn, Ar,?LINE,X,Y,Z,D])).
-define(DBG(Def,X,Y,Z,F,D,M),
	format_utc_timestamp(), 
	{current_function, {_, Fn, Ar}} = process_info(self(), current_function),
	io:format(" DBG-> ~p:~p/~p line.~p~n  --> "++Def,[?MODULE, Fn, Ar,?LINE,X,Y,Z,D,M])).

-export([format_utc_timestamp/0]).

format_utc_timestamp() ->
    TS = {_,_,Micro} = os:timestamp(),
    {_,{Hour,Minute,Second}} = 
	calendar:now_to_universal_time(TS),
    io:format("~2w:~2..0w:~2..0w.~6..0w", [Hour,Minute,Second,Micro]).



-else.
-define(DBG(Def,X), "").
-endif. 

-module(e_mail).

-export([send/1]).

%% Определим тип email() и структуру функции аналогично send/2 из deps/gensmtp/gen_smtp_client.erl
-type email() :: {string() | binary(), [string() | binary(), ...], string() | binary() | function()}.
-spec send(Email :: email()) -> {'ok', pid()} | {'error', any()}.

send(Email) ->  
	gen_smtp_client:send(Email,[
				%% это настройки заранее созданного почтового ящика gmail
          			  {relay, "smtp.gmail.com"}, 
          			  {username, "gen.smtp.test"}, 
         			  {password, "gen_smtp_test"}
  			     	]).

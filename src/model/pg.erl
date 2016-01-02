-module(pg).

-export([connect/0]).

-include("deps/epgsql/include/epgsql.hrl").

%% -type host() :: inet:ip_address() | inet:hostname().

%% -type connect_option() ::
%%    {database, DBName     :: string()}             |
%%    {port,     PortNum    :: inet:port_number()}   |
%%    {ssl,      IsEnabled  :: boolean() | required} |
%%    {ssl_opts, SslOptions :: [ssl:ssl_option()]}   | % @see OTP ssl app, ssl_api.hrl
%%    {timeout,  TimeoutMs  :: timeout()}            | % default: 5000 ms
%%    {async,    Receiver   :: pid()}. % process to receive LISTEN/NOTIFY msgs

%%-spec connect(host(), string(), string(), [connect_option()])
%%		 -> {ok, Connection :: connection()} | {error, Reason :: connect_error()}. 

   
%% @doc connects to Postgres
%% where
%% `Host'     - host to connect to
%% `Username' - username to connect as, defaults to `$USER'
%% `Password' - optional password to authenticate with
%% `Opts'     - proplist of extra options
%% returns `{ok, Connection}' otherwise `{error, Reason}'

connect() ->
	connect("localhost", "oleg", "qwerty", [{database, "test_db"}, {timeout, 4000}]).


connect(Host, Username, Password, Opts) ->
		epgsql:connect( Host, Username, Password, Opts).


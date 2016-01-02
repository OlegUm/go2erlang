-module(go2erlang_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [ {users, {users, start_link, []},
				permanent, 5000, worker, [users]}],
	{ok, {{one_for_one, 10, 10}, Procs}}.

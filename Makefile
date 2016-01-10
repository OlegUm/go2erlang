PROJECT = go2erlang
DEPS = cowboy erlydtl gen_smtp eunit pgsql cowboy_session
dep_cowboy = git https://github.com/ninenines/cowboy master 

dep_cowboy_session =  git https://github.com/rusblaze/cowboy_session feature/cowboy_2.0_fix

include erlang.mk

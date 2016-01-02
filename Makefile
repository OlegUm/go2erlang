PROJECT = go2erlang
DEPS = cowboy erlydtl gen_smtp epgsql eunit
LOCAL_DEPS = mnesia edoc
dep_cowboy = git https://github.com/ninenines/cowboy master 

include erlang.mk

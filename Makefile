PROJECT = go2erlang
DEPS = cowboy erlydtl gen_smtp
LOCAL_DEPS = mnesia
dep_cowboy = git https://github.com/ninenines/cowboy master 
include erlang.mk

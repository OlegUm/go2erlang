PROJECT = go2erlang
DEPS = cowboy erlydtl gen_smtp
dep_cowboy = git https://github.com/ninenines/cowboy master 
include erlang.mk

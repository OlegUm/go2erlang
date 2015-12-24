PROJECT = go2erlang
DEPS = cowboy erlydtl gen_smtp
LOCAL_DEPS = mnesia edoc
dep_cowboy = git https://github.com/ninenines/cowboy master 
EDOC_OPTS = [{dir,"doc"}, {source_path, ["src"]}, {doclet, edown_doclet}]
include erlang.mk

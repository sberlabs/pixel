PROJECT = pixel

DEPS = cowboy jiffy uuid gen_leader gproc
GPROC_DIST=true

dep_cowboy = pkg://cowboy master
dep_jiffy = https://github.com/davisp/jiffy master
dep_uuid = https://github.com/avtobiff/erlang-uuid master
dep_gproc = https://github.com/esl/gproc.git master
dep_gen_leader = https://github.com/garret-smith/gen_leader_revival.git master

include erlang.mk


all: ebin/user_default.beam

export ERL_LIBS=/usr/lib/rabbitmq/lib

ebin:
	mkdir -p ebin

ebin/user_default.beam: ebin user_default.erl Makefile
	rm -f $@
	erlc +debug_info  -o ebin user_default.erl

# Get defaults from running rabbit
ROOT=$(shell pwd)
N=$(shell rabbitmqctl eval "node()." | perl -pe "s/^'|'\$$//g")
C=$(shell rabbitmqctl eval "erlang:get_cookie()."| perl -pe "s/^'|'\$$//g")
TICK=$(shell rabbitmqctl eval "net_kernel:get_net_ticktime()."| perl -pe "s/^'|'\$$//g")

shell: all
	erl -pa $(shell pwd)/ebin -sname shell-$$$$ -remsh "${N}" -setcookie "${C}" -kernel net_ticktime "${TICK}" -eval "{Module, Binary, Filename} = code:get_object_code(user_default), rpc:call('${N}', code, load_abs, [\"${ROOT}/ebin/user_default\"])."




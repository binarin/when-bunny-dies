all: ebin/user_default.beam

export ERL_LIBS=/usr/lib/rabbitmq/lib

ebin:
	mkdir -p ebin

ebin/user_default.beam: ebin user_default.erl Makefile
	rm -f $@
	erlc +debug_info  -o ebin user_default.erl

# Get defaults from running rabbit
N=$(shell rabbitmqctl eval "node().")
C=$(shell rabbitmqctl eval "erlang:get_cookie().")
TICK=$(shell rabbitmqctl eval "net_kernel:get_net_ticktime().")

shell: all
	erl -pa $(shell pwd)/ebin -sname shell-$$$$ -remsh "${N}" -setcookie ${C} -kernel net_ticktime "${TICK}" -eval "{Module, Binary, Filename} = code:get_object_code(user_default), rpc:call('${N}', code, load_binary, [Module, Filename, Binary])."




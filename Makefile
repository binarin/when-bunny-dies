all:
	@echo TARGETS:
	@echo
	@echo * Attach interactive shell to local rabbit:
	@echo   make shell
	@echo
	@echo * Perform tracing of message routing process (works when HiPE is disabled)
	@echo "make troute f=/path/to/produced/trace e=<exchange-name> r=<routing-key>"
	@echo
	@echo * Dump inner details of (topic) message routing process (last resort, when HiPE is enabled)
	@echo "make tdebug d=/path/to/dump/directory/ e=<exchange-name> r=<routing-key>"

export ERL_LIBS=/usr/lib/rabbitmq/lib:${ROOT}/unpacked-plugins-dir

recon:
	git clone https://github.com/ferd/recon

recon/ebin/recon_trace.beam recon/ebin/recon_lib.beam: recon
	cd recon && ./rebar compile

.PHONY: inject-recon

inject-recon: recon/ebin/recon_trace.beam recon/ebin/recon_lib.beam
	rabbitmqctl -n "${N}" eval "rpc:call('${N}', code, purge, [recon_trace]), rpc:call('${N}', code, load_abs, [\"${ROOT}/recon/ebin/recon_trace\"]), rpc:call('${N}', code, purge, [recon_lib]), rpc:call('${N}', code, load_abs, [\"${ROOT}/recon/ebin/recon_lib\"])."

ebin:
	mkdir -p ebin

PLUGINS_DIR=$(shell find /usr/lib/rabbitmq/lib -name plugins -type d)
PLUGINS=$(notdir $(basename $(notdir $(wildcard $(PLUGINS_DIR)/*.ez))))

unpacked-plugins-dir:
	mkdir -p unpacked-plugins-dir

unpack-plugins: unpacked-plugins-dir $(addprefix unpacked-plugins-dir/,$(PLUGINS))

unpacked-plugins-dir/%: $(PLUGINS_DIR)/%.ez
	cd unpacked-plugins-dir && unzip $<
	touch $@

ebin/user_default.beam: ebin user_default.erl Makefile unpack-plugins
	rm -f $@
	erlc +debug_info  -o ebin user_default.erl

# Get defaults from running rabbit
ROOT=$(shell pwd)
N=$(shell rabbitmqctl eval "node()." | perl -pe "s/^'|'\$$//g")
C=$(shell rabbitmqctl eval "erlang:get_cookie()."| perl -pe "s/^'|'\$$//g")
TICK=$(shell rabbitmqctl eval "net_kernel:get_net_ticktime()."| perl -pe "s/^'|'\$$//g")

inject-user-default: ebin/user_default.beam
	rabbitmqctl -n "${N}" eval "rpc:call('${N}', code, purge, [user_default]), rpc:call('${N}', code, load_abs, [\"${ROOT}/ebin/user_default\"])."

shell: inject-recon inject-user-default
	erl -hidden -pa $(shell pwd)/ebin -sname shell-$$$$ -remsh "${N}" -setcookie "${C}" -kernel net_ticktime "${TICK}"

troute: inject-recon inject-user-default
	@if [ -z "$(f)" ]; then echo filename not given; exit 1; fi
	@if [ -z "$(e)" ]; then echo exchange not given; exit 1; fi
	@if [ -z "$(r)" ]; then echo routing key not given; exit 1; fi
	rabbitmqctl -n "${N}" eval "user_default:trace_routing(\"${f}\", <<\"${e}\">>, <<\"${r}\">>)."

tdebug: inject-user-default
	@if [ -z "$(d)" ]; then echo directory not given; exit 1; fi
	@if [ -z "$(e)" ]; then echo exchange not given; exit 1; fi
	@if [ -z "$(r)" ]; then echo routing key not given; exit 1; fi
	mkdir -p $(d)
	rm -f $(d)/*
	chmod a+rwX -Rv $(d)
	rabbitmqctl -n "${N}" eval "user_default:dump_routing(\"${d}\", <<\"${e}\">>, <<\"${r}\">>)."

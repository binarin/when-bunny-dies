-module(user_default).

-compile([export_all]).
-include_lib("rabbitmq_server/include/rabbit.hrl").
-include_lib("rabbitmq_server/include/rabbit_framing.hrl").

queue(Name) ->
    rabbit_misc:dirty_read({rabbit_queue, {resource, <<"/">>, queue, atom_to_binary(Name, utf8)}}).

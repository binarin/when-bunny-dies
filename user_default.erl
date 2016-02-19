-module(user_default).

-compile([export_all]).
-include_lib("rabbitmq_server/include/rabbit.hrl").
-include_lib("rabbitmq_server/include/rabbit_framing.hrl").

ensure_ets() ->
    case catch ets:new(when_bunny_dies, [public, named_table]) of
        {'EXIT', _} ->
            %% Was created earlier
            ok;
        _ ->
            ets:give_away(when_bunny_dies, erlang:whereis(init), undefined)
    end,
    ok.

focus(Kind, Data) ->
    ensure_ets(),
    ets:insert(when_bunny_dies, {Kind, Data}).

queue(Name) ->
    {ok, Q} = rabbit_misc:dirty_read({rabbit_queue, {resource, <<"/">>, queue, atom_to_binary(Name, utf8)}}),
    Q.

qpid(Name) when is_atom(Name) ->
    qpid(queue(Name));
qpid(#amqqueue{pid = Pid}) ->
    Pid.

peer_port_conn(Port) ->
    Conn = hd([ Pid || Pid <- rabbit_networking:connections(),
                       element(2, hd(rabbit_reader:info(Pid, [peer_port]))) =:= Port]),
    focus(connection, Conn),
    Conn.

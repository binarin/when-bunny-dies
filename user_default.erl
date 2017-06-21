-module(user_default).

-compile([export_all]).
%% -include_lib("rabbitmq_server/include/rabbit.hrl").
%% -include_lib("rabbitmq_server/include/rabbit_framing.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").


with_connection(Params, Name, F) ->
    {ok, C} = amqp_connection:start(Params, Name),
    try F(C) of
        V -> V
    after
        amqp_connection:close(C)
    end.


dump_term(Dir, File, Term) ->
    file:write_file(Dir ++ "/" ++ File, io_lib:format("~p~n", [Term])).


dump_routing(Path, Exchange, RKey) ->
    mnesia:dump_to_textfile(Path ++ "/01-mnesia.dump"),
    dump_term(Path, "02-bindings.txt", rabbit_binding:list(<<"/">>)),
    dump_term(Path, "02-exchanges.txt", rabbit_exchange:list()),
    AllQueues = rabbit_amqqueue:list(),
    dump_term(Path, "03-queues.txt", AllQueues),
    DirectRouteAttempt = rabbit_exchange_type_topic:route(
                           #exchange{name=rabbit_misc:r(<<"/">>, exchange, Exchange)},
                           #delivery{message=#basic_message{routing_keys=[RKey]}}),
    dump_term(Path, "04-direct-route.txt", DirectRouteAttempt),
    RoutedQueues = lists:filter(fun(It) -> lists:member(It#amqqueue.name, DirectRouteAttempt) end, AllQueues),
    dump_term(Path, "05-direct-route-queues.txt", RoutedQueues),
    RoutedQueueInfo = lists:map(fun rabbit_amqqueue:info/1, RoutedQueues),
    dump_term(Path, "06-direct-route-queues-liveness.txt", RoutedQueueInfo).

trace_routing(FileName, Exchange, RKey) ->
    {ok, Dev} = file:open(FileName, [write]),
    try
        trace_routing1(Dev, Exchange, RKey)
    after
        timer:sleep(3000),
        file:close(Dev)
    end.

trace_routing1(TraceDev, Exchange, RKey) ->
    with_connection(#amqp_params_direct{}, <<"trace_routing_test">>,
                    fun(C) ->
                            {ok, Ch} = amqp_connection:open_channel(C),
                            MRef = erlang:monitor(process, Ch),
                            [ChPid] = [ proplists:get_value(pid, I) || X <- rabbit_channel:list(),
                                                                       node(X) == node(),
                                                                       I <- [rabbit_channel:info(X)],
                                                                       proplists:get_value(connection, I) == C ],
                            recon_trace:calls({'_', '_', [{'_',[],[{return_trace}]}]},
                                              1000,
                                              [{pid, ChPid}
                                              ,{io_server, TraceDev}
                                              ,{scope, local}
                                              ]),
                            amqp_channel:register_confirm_handler(Ch, self()),
                            amqp_channel:register_return_handler(Ch, self()),
                            amqp_channel:call(Ch, #'confirm.select'{}),
                            Payload = <<"test">>,
                            Publish = #'basic.publish'{exchange = Exchange,
                                                       routing_key = RKey,
                                                       mandatory = true},
                            amqp_channel:cast(Ch, Publish, #amqp_msg{payload = Payload}),
                            receive
                                {#'basic.return'{}, _} ->
                                    receive
                                        #'basic.ack'{} -> ok
                                    end,
                                    not_routed;
                                #'basic.ack'{} ->
                                    routed;
                                {'DOWN', _, _, _, Err} ->
                                    {failed, Err}
                            end
                    end).

ensure_ets() ->
    case catch ets:new(when_bunny_dies, [public, named_table]) of
        {'EXIT', _} ->
            %% Was created earlier
            ok;
        _ ->
            ets:give_away(when_bunny_dies, erlang:whereis(init), undefined)
    end,
    ok.

focus(Kind) ->
    [{Kind, Data}] = ets:lookup(when_bunny_dies, Kind),
    Data.

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

color(red) ->
    "\e[31m";
color(green) ->
    "\e[32m";
color(default) ->
    "\e[39m".

color_print(String, Color) ->
    io:format("~s~s~s", [color(Color), String, color(default)]).

format_table(Data) ->
    NameWidth = lists:max([ byte_size(atom_to_binary(Key, utf8)) || {Key, _} <- Data ]),
    [ begin
          color_print(Key, green),
          io:format("~s~p~n", [lists:duplicate(2 + NameWidth - byte_size(atom_to_binary(Key, utf8)), 32),
                               Value])
      end
      || {Key, Value} <- Data ],
    ok.

conn_info() ->
    conn_info(focus(connection)).

conn_info(Pid) when is_pid(Pid) ->
    format_table(rabbit_reader:info(Pid)).

channel_sup_sup_pid() ->
    channel_sup_sup_pid(focus(connection)).
channel_sup_sup_pid(Conn) ->
    {_, _, State} = sys:get_state(Conn),
    element(13, State).

conn_channels() ->
    conn_channels(focus(connection)).

conn_channels(Conn) when is_pid(Conn) ->
    ChannelSups = supervisor2:which_children(channel_sup_sup_pid(Conn)),
    Channels = [ begin
                     {_, Chan, _, _} = lists:keyfind(channel, 1, supervisor2:which_children(Sup)),
                     Chan
                 end
                 || {_, Sup, _, _} <- ChannelSups ],
    focus(channel, hd(Channels)),
    Channels.

chan_info() ->
    chan_info(focus(channel)).

chan_info(Chan) when is_pid(Chan) ->
    format_table(gen_server2:call(Chan, info)).

chan_state() ->
    chan_state(focus(channel)).

chan_state(Chan) when is_pid(Chan) ->
    {gs2_state, _, _, State, _, _, _, _, _, _} = sys:get_state(Chan),
    State.

chan_consumers() ->
    chan_consumers(focus(channel)).

chan_consumers(Chan) when is_pid(Chan) ->
    State = chan_state(Chan),
    ConsumerMapping = element(18, State),
    dict:to_list(ConsumerMapping).

ch_queue_monitors() ->
    ch_queue_monitors(focus(channel)).

ch_queue_monitors(Chan) ->
    State = chan_state(Chan),
    QueueMonitors = element(17, State),
    QueueMonitors.

ch_queue_consumers() ->
    ch_queue_consumers(focus(channel)).

ch_queue_consumers(Chan) when is_pid(Chan) ->
    State = chan_state(Chan),
    QueueConsumers = element(19, State),
    QueueConsumers.

ch_queue_names() ->
    ch_queue_names(focus(channel)).

ch_queue_names(Chan) when is_pid(Chan) ->
    State = chan_state(Chan),
    QueueNames = element(16, State),
    QueueNames.

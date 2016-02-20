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

chan_queue_monitors() ->
    chan_queue_monitors(focus(channel)).

chan_queue_monitors(Chan) ->
    State = chan_state(Chan),
    ConsumerMapping = element(18, State),
    ConsumerMapping.

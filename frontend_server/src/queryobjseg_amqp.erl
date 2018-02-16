-module(queryobjseg_amqp).
-export([loop/1]).

-include_lib("amqp_client/include/amqp_client.hrl").
% -export([start/2, stop/1]).

-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

-spec loop(pid()) -> ok.
loop(Channel) ->
    receive
        #'basic.consume_ok'{} ->
            loop(Channel);

        %% This is received when the subscription is cancelled
        #'basic.cancel_ok'{} ->
            lager:info("Channel closed?"),
            ok;

        %% A delivery
        {#'basic.deliver'{delivery_tag = Tag}, Content} ->
            %% Do something with the message payload
            %% (some work here)
            % ?PRINT(Content),
            %% Ack the message
            % ok = amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
            % ok = amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
            % ok = amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),

            lager:info("Message arrived"),
            {amqp_msg, Headers, Body} = Content,
            % ?PRINT(Body),
            Json = sr_json:decode(Body),
            DeviceId = maps:get(<<"device_id">>, Json),
            lager:info(DeviceId),
            Device = sumo:fetch(queryobjseg_devices, DeviceId),
            lager:info("Device: ~p", [Device]),
            FirebaseToken = maps:get(firebase_token, Device),
            case queryobjseg_segmentations_repo:exists(maps:get(<<"id">>, Json)) of
              true ->
                lager:info("Notify!"),
                gen_event:notify(queryobjseg_fcm_events_manager,
                                 {send_message, Json, FirebaseToken}),
                sumo:delete(queryobjseg_segmentations, maps:get(<<"id">>, Json))
            end,
            %% Loop
            loop(Channel)
    end.

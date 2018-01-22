-module(canillita_amqp).
-export([loop/1]).

-include_lib("amqp_client/include/amqp_client.hrl").
% -export([start/2, stop/1]).

-spec loop(pid()) -> ok.
loop(Channel) ->
    receive
        #'basic.consume_ok'{} ->
            loop(Channel);

        %% This is received when the subscription is cancelled
        #'basic.cancel_ok'{} ->
            ok;

        %% A delivery
        {#'basic.deliver'{delivery_tag = Tag}, Content} ->
            %% Do something with the message payload
            %% (some work here)

            io:format(Content),
            %% Ack the message
            amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),

            %% Loop
            loop(Channel)
    end.

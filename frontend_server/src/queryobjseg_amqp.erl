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
            ?PRINT("Ah?"),
            loop(Channel);

        %% This is received when the subscription is cancelled
        #'basic.cancel_ok'{} ->
            ok;

        %% A delivery
        {#'basic.deliver'{delivery_tag = Tag}, Content} ->
            %% Do something with the message payload
            %% (some work here)
            % ?PRINT(Content),
            lager:info("Message arrived"),
            % lager:info(Content),
            %% Ack the message
            amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),

            %% Loop
            loop(Channel)
    end.

-module(queryobjseg_ws).

-export([ init/3
        , websocket_init/3
        , websocket_handle/3
        , websocket_info/3
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_, Req, Opts) ->
	{upgrade, protocol, cowboy_websocket, Req, Opts}.

websocket_init(_, Req, State) ->
	erlang:start_timer(1000, self(), <<"Hello!">>),
	{ok, Req, State}.

websocket_handle({text, Msg}, Req, State) ->
  Json = sr_json:decode(Msg),
  Value = maps:get(<<"value">>, Json),
  % lager:info("~p", [Value]),
	{reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
	% erlang:start_timer(1000, self(), <<"How' you doin'? OOOO">>),
	{reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

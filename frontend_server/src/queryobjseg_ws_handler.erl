-module(queryobjseg_ws_handler).
-behaviour(trails_handler).

%% trails_handler behaviour callback
-export([trails/0]).

%% API
% -export([notify/1]).

%% lasse_handler behaviour callbacks
-export([ init/2
        , websocket_init/1
        , websocket_handle/2
        , websocket_info/2
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% trails_handler callback
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec trails() -> trails:trails().
trails() ->
  Metadata =
    #{ get =>
       #{ tags => ["results"]
        , summary =>
          "WARNING: Do not try to use this endpoint from this page."
          " Swagger doesn't understand WS"
        , description =>
          "Opens an [SSE] (http://www.w3.org/TR/eventsource/)"
          " connection to retrieve results updates"
        , externalDocs =>
          #{ description => "RFC"
           , url => "http://www.w3.org/TR/eventsource/"
           }
        , produces => ["application/json"]
        }
     },
  Path = "/results",
  % Options = #{},
  % Options = #{module => queryobjseg_ws, init_args => #{path => Path}},
  [trails:trail(Path, queryobjseg_ws, [], Metadata)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Req, Opts) ->
	{cowboy_websocket, Req, Opts}.

websocket_init(State) ->
	erlang:start_timer(1000, self(), <<"Hello!">>),
	{ok, State}.

websocket_handle({text, Msg}, State) ->
	{reply, {text, << "That's what she said! ", Msg/binary >>}, State};
websocket_handle(_Data, State) ->
	{ok, State}.

websocket_info({timeout, _Ref, Msg}, State) ->
	erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{reply, {text, Msg}, State};
websocket_info(_Info, State) ->
	{ok, State}.

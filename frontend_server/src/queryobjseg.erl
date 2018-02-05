-module(queryobjseg).
-behaviour(application).
-include_lib("amqp_client/include/amqp_client.hrl").

-export([start/0, start/2, start_phase/3]).
-export([stop/0, stop/1]).

-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ADMIN API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Starts the Application
-spec start() -> {ok, [atom()]} | {error, term()}.
start() -> {ok, _} = application:ensure_all_started(canillita).

%% @doc Stops the Application
-spec stop() -> ok | {error, term()}.
stop() -> ok = application:stop(canillita).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start(Type::application:start_type(), Args::any()) -> {ok, pid()}.
start(_Type, _Args) -> {ok, self()}.

-spec stop(State::[]) -> ok.
stop(_State) ->
  gen_event:delete_handler( canillita_newsitems_events_manager
                          , canillita_newsitems_events_handler
                          , []
                          ),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% START PHASES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
-spec start_phase(atom(), StartType::application:start_type(), []) ->
  ok | {error, _}.
start_phase(create_schema, _StartType, []) ->
  _ = application:stop(mnesia),
  Node = node(),
  case mnesia:create_schema([Node]) of
    ok -> ok;
    {error, {Node, {already_exists, Node}}} -> ok
  end,
  {ok, _} = application:ensure_all_started(mnesia),
  % Create persistency schema
  sumo:create_schema();
start_phase(start_amqp, _StartType, []) ->
  application:ensure_started(amqp_client),
  {ok, ConnInfo} = amqp_uri:parse(
    os:getenv(<<"AMQP_URL">>)),
  % "amqp://langvis:eccv2018-textseg@margffoy-tuay.com:5672/queryobjseg"
  {ok, Connection} = amqp_connection:start(ConnInfo),
  {ok, Channel} = amqp_connection:open_channel(Connection),
  Declare = #'exchange.declare'{exchange = <<"queryobj_in">>},
  #'exchange.declare_ok'{} = amqp_channel:call(Channel, Declare),

  #'queue.declare_ok'{queue = Q} = amqp_channel:call(
    Channel, #'queue.declare'{queue = <<"frontend">>}),

  Binding = #'queue.bind'{queue       = Q,
                          exchange    = <<"queryobj_in">>,
                          routing_key = <<"query.answers">>},
  #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),
  ?PRINT(Connection),
  register(rmqchannel, Channel),
  register(rmqconn, Connection),
  % Sub = #'basic.consume'{queue = Q},
  % _ = lager:info("Sub ~p", [Sub]),
% #'basic.consume_ok'{consumer_tag = Tag} =
  % amqp_channel:call(Channel, Sub),
  % _ = lager:info("Tag ~p", [Tag]),
  {ok, Channel2} = amqp_connection:open_channel(Connection),
  register(rmqchannel2, Channel2),
  Consumer = spawn(queryobjseg_amqp, loop, [Channel2]),
  register(rmqconsumer, Consumer),
  Sub = #'basic.consume'{queue = Q},
  #'basic.consume_ok'{consumer_tag = Tag} =
  amqp_channel:subscribe(Channel, Sub, Consumer),
  % io:format(Tag),
  % #amqp_msg{payload = Payload} = Content,
  ok;
start_phase(start_cowboy_listeners, _StartType, []) ->
  Handlers =
    [ queryobjseg_segmentations_handler
    , canillita_newspapers_handler
    , canillita_single_newspaper_handler
    , canillita_newsitems_handler
    , canillita_single_newsitem_handler
    , canillita_news_handler
    , cowboy_swagger_handler
    ],
  % application:ensure_started(amqp_client),
  % Get the trails for each handler
  Routes = trails:trails(Handlers),
  % Store them so Cowboy is able to get them
  trails:store(Routes),
  % Set server routes
  Dispatch = trails:single_host_compile(Routes),
  % Set the options for the TCP layer
  TransOpts = [{port, 4892}],
  % Set the options for the HTTP layer
  ProtoOpts = [{env, [{dispatch, Dispatch}, {compress, true}]}],
  % Start Cowboy HTTP server
  case cowboy:start_http(queryobseg_server, 1, TransOpts, ProtoOpts) of
    {ok, _} -> ok;
    {error, {already_started, _}} -> ok
  end;
start_phase(start_canillita_events_management, _StartType, []) ->
  % Set the handler for processing SumoDB events
  ok = gen_event:add_handler( canillita_newsitems_events_manager
                            , canillita_newsitems_events_handler
                            , []
                            ),
  % Create pg2 group to hold news listeners
  pg2:create(canillita_listeners).

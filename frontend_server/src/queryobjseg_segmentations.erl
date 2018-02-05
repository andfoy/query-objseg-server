%%% @doc Segmentations Model
-module(queryobjseg_segmentations).

-behaviour(sumo_doc).
-behaviour(sumo_rest_doc).
-include_lib("amqp_client/include/amqp_client.hrl").

-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

-type id() :: binary().
-type deviceid() :: binary().
-type b64img() :: binary().
-type phrase() :: binary().

-opaque segmentation() ::
  #{ id           => id() | undefined
   , device_id    => deviceid()
   , b64_img      => b64img()
   , phrase       => phrase()
   , created_at   => calendar:datetime()
   }.

-export_type(
  [ id/0
  , deviceid/0
  , b64img/0
  , phrase/0
  , segmentation/0
  ]).

%% sumo_doc behaviour
-export(
  [ sumo_schema/0
  , sumo_sleep/1
  , sumo_wakeup/1
  ]).

%% sumo_rest_doc behaviour
-export(
  [ to_json/1
  , from_json/1
  , update/2
  , location/2
  , duplication_conditions/1
  ]).

%% public API
-export(
  [ new/4
  , id/1
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo_doc behaviour callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(
    ?MODULE,
    [ sumo:new_field(id, binary, [id, unique])
    , sumo:new_field(device_id, binary, [not_null])
    , sumo:new_field(b64_img, binary, [not_null])
    , sumo:new_field(phrase, binary, [not_null])
    , sumo:new_field(created_at, datetime, [not_null])
    ]).

%% @doc Convert a segmentation from its system representation to sumo's
%%      internal one.
-spec sumo_sleep(Segmentation::segmentation()) -> sumo:model().
sumo_sleep(Segmentation) -> Segmentation.

%% @doc Convert a segmentation from sumo's internal representation to its
%%      system one.
-spec sumo_wakeup(Segmentation::sumo:doc()) -> segmentation().
sumo_wakeup(Segmentation) -> Segmentation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo_rest_doc behaviour callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Convert a segmentation from its system representation to json.
-spec to_json(Segmentation::segmentation()) -> sr_json:json().
to_json(Segmentation) ->
  #{ id         => sr_json:encode_null(maps:get(id, Segmentation))
   , device_id  => maps:get(device_id, Segmentation)
   , b64_img  => maps:get(b64_img, Segmentation)
   , phrase  => maps:get(phrase, Segmentation)
   , created_at   => sr_json:encode_date(maps:get(created_at, Segmentation))
   }.

%% @doc Convert a segmentation from json to its system representation.
-spec from_json(Json::sumo_rest_doc:json()) ->
  {ok, segmentation()} | {error, iodata()}.
from_json(Json) ->
  Now = sr_json:encode_date(calendar:universal_time()),
  try
    { ok
    , #{ id => sr_json:decode_null(maps:get(<<"id">>, Json, null))
       , device_id => maps:get(<<"device_id">>, Json)
       , b64_img => maps:get(<<"b64_img">>, Json)
       , phrase => maps:get(<<"phrase">>, Json)
       , created_at =>
           sr_json:decode_date(maps:get(<<"created_at">>, Json, Now))
       }
    }
  catch
    _: {badkey, Key} -> {error, <<"missing field: ", Key/binary>>}
  end.

-spec update(Segmentation::segmentation(), Json::sumo_rest_doc:json()) ->
  {ok, segmentation()} | {error, iodata()}.
update(Segmentation, Json) ->
  try
    % Connection = whereis(rmq),
    % {ok, Channel} = amqp_connection:open_channel(Connection),
    % Declare = #'queue.declare'{queue = <<"TEST_DIRECT_QUEUE">>, durable = true},
    % #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare),
    % Get = #'basic.get'{queue = <<"TEST_DIRECT_QUEUE">>, no_ack = true},
    % {#'basic.get_ok'{}, Content} = amqp_channel:call(Channel, Get),
    % #amqp_msg{payload = Payload} = Content,
    % amqp_channel:close(Channel),
    NewDescription = maps:get(<<"description">>, Json),
    UpdatedSegmentation =
      Segmentation#{description := NewDescription,
                 updated_at := calendar:universal_time()},
    {ok, UpdatedSegmentation}
  catch
    _:{badkey, Key} -> {error, <<"missing field: ", Key/binary>>}
  end.

%% @doc Specify the uri part that uniquely identifies a Segmentation.
-spec location(Segmentation::segmentation(), Path::sumo_rest_doc:path()) -> iodata().
location(Segmentation, Path) ->
  ?PRINT("New!"),
  Channel = whereis(rmqchannel),
  Payload = <<"foobar">>,
  Publish = #'basic.publish'{exchange = <<"queryobj_in">>, routing_key = <<"query.answers">>},
  amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}),
  iolist_to_binary([Path, $/, id(Segmentation)]).

%% @doc Optional callback duplication_conditions/1 to let sumo_rest avoid
%%      duplicated keys (and return `422 Conflict` in that case).
-spec duplication_conditions(segmentation()) ->
  sumo_rest_doc:duplication_conditions().
duplication_conditions(Segmentation) ->
  {id, id(Segmentation)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new(Id::id(), DeviceId::deviceid(), B64Img::b64img(), Phrase::phrase()) -> segmentation().
new(Id, DeviceId, B64Img, Phrase) ->
  % amqp_channel:close(Channel),
  Now = calendar:universal_time(),
  #{ id         => Id
   , device_id  => DeviceId
   , b64_img => B64Img
   , phrase => Phrase
   , created_at   => Now
   }.

-spec id(Segmentation::segmentation()) -> id().
id(#{id := Id}) -> Id.

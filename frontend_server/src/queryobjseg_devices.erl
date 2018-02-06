%%% @doc Devices Model
-module(queryobjseg_devices).

-behaviour(sumo_doc).
-behaviour(sumo_rest_doc).
-include_lib("amqp_client/include/amqp_client.hrl").

-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

-type deviceid() :: binary().
-type firebase_token() :: binary().

-opaque device() ::
  #{ device_id    => deviceid()
   , firebase_token      => firebase_token()
   , updated_at   => calendar:datetime()
   }.

-export_type(
  [ deviceid/0
  , firebase_token/0
  , device/0
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
  [ new/2
  , deviceid/1
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo_doc behaviour callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(
    ?MODULE,
    [ sumo:new_field(device_id, binary, [id, unique])
    , sumo:new_field(firebase_token, binary, [not_null])
    , sumo:new_field(updated_at, datetime, [not_null])
    ]).

%% @doc Convert a device from its system representation to sumo's
%%      internal one.
-spec sumo_sleep(Device::device()) -> sumo:model().
sumo_sleep(Device) -> Device.

%% @doc Convert a device from sumo's internal representation to its
%%      system one.
-spec sumo_wakeup(Device::sumo:doc()) -> device().
sumo_wakeup(Device) -> Device.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo_rest_doc behaviour callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Convert a device from its system representation to json.
-spec to_json(Device::device()) -> sr_json:json().
to_json(Device) ->
  #{ device_id  => maps:get(device_id, Device)
   % , firebase_token  => maps:get(firebase_token, Device)
   , updated_at   => sr_json:encode_date(maps:get(updated_at, Device))
   }.

%% @doc Convert a device from json to its system representation.
-spec from_json(Json::sumo_rest_doc:json()) ->
  {ok, device()} | {error, iodata()}.
from_json(Json) ->
  Now = sr_json:encode_date(calendar:universal_time()),
  try
    A = { ok
    , #{ device_id => maps:get(<<"device_id">>, Json)
       , firebase_token => maps:get(<<"firebase_token">>, Json)
       , updated_at =>
           sr_json:decode_date(maps:get(<<"updated_at">>, Json, Now))
       }
    },
    io:format("~p~n", [A]),
    A
  catch
    _: {badkey, Key} -> {error, <<"missing field: ", Key/binary>>}
  end.

-spec update(Device::device(), Json::sumo_rest_doc:json()) ->
  {ok, device()} | {error, iodata()}.
update(Device, Json) ->
  try
    % Connection = whereis(rmq),
    % {ok, Channel} = amqp_connection:open_channel(Connection),
    % Declare = #'queue.declare'{queue = <<"TEST_DIRECT_QUEUE">>, durable = true},
    % #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare),
    % Get = #'basic.get'{queue = <<"TEST_DIRECT_QUEUE">>, no_ack = true},
    % {#'basic.get_ok'{}, Content} = amqp_channel:call(Channel, Get),
    % #amqp_msg{payload = Payload} = Content,
    % amqp_channel:close(Channel),
    NewToken = maps:get(<<"firebase_token">>, Json),
    UpdatedDevice =
      Device#{firebase_token := NewToken,
                 updated_at := calendar:universal_time()},
    {ok, UpdatedDevice}
  catch
    _:{badkey, Key} -> {error, <<"missing field: ", Key/binary>>}
  end.

%% @doc Specify the uri part that uniquely identifies a Device.
-spec location(Device::device(), Path::sumo_rest_doc:path()) -> iodata().
location(Device, Path) ->
  ?PRINT("New!"),
  Channel = whereis(rmqchannel),
  Payload = <<"foobar">>,
  Publish = #'basic.publish'{exchange = <<"queryobj_in">>, routing_key = <<"query.answers">>},
  amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}),
  iolist_to_binary([Path, $/, deviceid(Device)]).

%% @doc Optional callback duplication_conditions/1 to let sumo_rest avoid
%%      duplicated keys (and return `422 Conflict` in that case).
-spec duplication_conditions(device()) ->
  sumo_rest_doc:duplication_conditions().
duplication_conditions(Device) ->
  {device_id, deviceid(Device)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new(DeviceId::deviceid(), FirebaseToken::firebase_token()) -> device().
new(DeviceId, FirebaseToken) ->
  % amqp_channel:close(Channel),
  Now = calendar:universal_time(),
  #{ device_id  => DeviceId
   , firebase_token => FirebaseToken
   , updated_at   => Now
   }.

-spec deviceid(Device::device()) -> deviceid().
deviceid(#{device_id := Id}) -> Id.

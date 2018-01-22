%%% @doc Newspapers Model
-module(canillita_newspapers).

-behaviour(sumo_doc).
-behaviour(sumo_rest_doc).
-include_lib("amqp_client/include/amqp_client.hrl").

-type name() :: binary().
-type description() :: binary().

-opaque newspaper() ::
  #{ name         => name()
   , description  => description()
   , created_at   => calendar:datetime()
   , updated_at   => calendar:datetime()
   }.

-export_type(
  [ name/0
  , description/0
  , newspaper/0
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
  , name/1
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo_doc behaviour callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(
    ?MODULE,
    [ sumo:new_field(name, binary, [id, unique])
    , sumo:new_field(description, string, [not_null])
    , sumo:new_field(created_at, datetime, [not_null])
    , sumo:new_field(updated_at, datetime, [not_null])
    ]).

%% @doc Convert a newspaper from its system representation to sumo's
%%      internal one.
-spec sumo_sleep(Newspaper::newspaper()) -> sumo:model().
sumo_sleep(Newspaper) -> Newspaper.

%% @doc Convert a newspaper from sumo's internal representation to its
%%      system one.
-spec sumo_wakeup(Newspaper::sumo:doc()) -> newspaper().
sumo_wakeup(Newspaper) -> Newspaper.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo_rest_doc behaviour callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Convert a newspaper from its system representation to json.
-spec to_json(Newspaper::newspaper()) -> sr_json:json().
to_json(Newspaper) ->
  #{ name         => maps:get(name, Newspaper)
   , description  => maps:get(description, Newspaper)
   , created_at   => sr_json:encode_date(maps:get(created_at, Newspaper))
   , updated_at   => sr_json:encode_date(maps:get(updated_at, Newspaper))
   }.

%% @doc Convert a newspaper from json to its system representation.
-spec from_json(Json::sumo_rest_doc:json()) ->
  {ok, newspaper()} | {error, iodata()}.
from_json(Json) ->
  io:format("New"),
  Channel = whereis(rmqchannel),
  Payload = <<"foobar">>,
  Publish = #'basic.publish'{exchange = <<"test_exchange">>, routing_key = <<"query.answers">>},
  amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}),
  Now = sr_json:encode_date(calendar:universal_time()),
  try
    { ok
    , #{ name => maps:get(<<"name">>, Json)
       , description => maps:get(<<"description">>, Json)
       , created_at =>
           sr_json:decode_date(maps:get(<<"created_at">>, Json, Now))
       , updated_at =>
           sr_json:decode_date(maps:get(<<"updated_at">>, Json, Now))
       }
    }
  catch
    _: {badkey, Key} -> {error, <<"missing field: ", Key/binary>>}
  end.

-spec update(Newspaper::newspaper(), Json::sumo_rest_doc:json()) ->
  {ok, newspaper()} | {error, iodata()}.
update(Newspaper, Json) ->
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
    UpdatedNewspaper =
      Newspaper#{description := NewDescription,
                 updated_at := calendar:universal_time()},
    {ok, UpdatedNewspaper}
  catch
    _:{badkey, Key} -> {error, <<"missing field: ", Key/binary>>}
  end.

%% @doc Specify the uri part that uniquely identifies a Newspaper.
-spec location(Newspaper::newspaper(), Path::sumo_rest_doc:path()) -> iodata().
location(Newspaper, Path) -> iolist_to_binary([Path, $/, name(Newspaper)]).

%% @doc Optional callback duplication_conditions/1 to let sumo_rest avoid
%%      duplicated keys (and return `422 Conflict` in that case).
-spec duplication_conditions(newspaper()) ->
  sumo_rest_doc:duplication_conditions().
duplication_conditions(Newspaper) ->
  {name, name(Newspaper)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new(Name::name(), Description::description()) -> newspaper().
new(Name, Description) ->
  % amqp_channel:close(Channel),
  Now = calendar:universal_time(),
  #{ name         => Name
   , description  => Description
   , created_at   => Now
   , updated_at   => Now
   }.

-spec name(Newspaper::newspaper()) -> name().
name(#{name := Name}) -> Name.

%%% @doc Datasets Model
-module(queryobjseg_datasets).

-behaviour(sumo_doc).
-behaviour(sumo_rest_doc).
% -include_lib("amqp_client/include/amqp_client.hrl").

-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

-type id() :: binary().
-type name() :: binary().
-type split() :: binary().

-opaque dataset() ::
  #{ id      => id() | undefined
   , name    => name()
   , split   => split()
   }.

-export_type(
  [ id/0
  , name/0
  , split/0
  , dataset/0
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
  [ new/3
  , split/1
  , name/1
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo_doc behaviour callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(
    ?MODULE,
    [ sumo:new_field(id, binary, [id, unique])
    , sumo:new_field(name, binary, [not_null])
    , sumo:new_field(split, binary, [not_null])
    ]).

%% @doc Convert a dataset from its system representation to sumo's
%%      internal one.
-spec sumo_sleep(Dataset::dataset()) -> sumo:model().
sumo_sleep(Dataset) -> Dataset.

%% @doc Convert a dataset from sumo's internal representation to its
%%      system one.
-spec sumo_wakeup(Dataset::sumo:doc()) -> dataset().
sumo_wakeup(Dataset) -> Dataset.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo_rest_doc behaviour callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Convert a dataset from its system representation to json.
-spec to_json(Dataset::dataset()) -> sr_json:json().
to_json(Dataset) ->
  #{ id  => maps:get(id, Dataset)
   , name  => maps:get(name, Dataset)
   , split   => maps:get(split, Dataset)
   }.

%% @doc Convert a dataset from json to its system representation.
-spec from_json(Json::sumo_rest_doc:json()) ->
  {ok, dataset()} | {error, iodata()}.
from_json(Json) ->
  Now = sr_json:encode_date(calendar:universal_time()),
  try
    A = { ok
    , #{ id => sr_json:decode_null(maps:get(<<"id">>, Json, null))
       , name => maps:get(<<"name">>, Json)
       , split => maps:get(<<"split">>, Json)
       }
    },
    io:format("~p~n", [A]),
    A
  catch
    _: {badkey, Key} -> {error, <<"missing field: ", Key/binary>>}
  end.

-spec update(Dataset::dataset(), Json::sumo_rest_doc:json()) ->
  {ok, dataset()} | {error, iodata()}.
update(Dataset, Json) ->
  try
    {ok, Dataset}
  catch
    _:{badkey, Key} -> {error, <<"missing field: ", Key/binary>>}
  end.

%% @doc Specify the uri part that uniquely identifies a Dataset.
-spec location(Dataset::dataset(), Path::sumo_rest_doc:path()) -> iodata().
location(Dataset, Path) ->
  ?PRINT("New!"),
  iolist_to_binary([Path, $/, name(Dataset)]).

%% @doc Optional callback duplication_conditions/1 to let sumo_rest avoid
%%      duplicated keys (and return `422 Conflict` in that case).
-spec duplication_conditions(dataset()) ->
  sumo_rest_doc:duplication_conditions().
duplication_conditions(Dataset) ->
  {'and', [{name, name(Dataset)}, {split, split(Dataset)}]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new(Id::id(), Name::name(), Split::split()) -> dataset().
new(Id, Name, Split) ->
  % amqp_channel:close(Channel),
  Now = calendar:universal_time(),
  #{ id  => Id
   , name => Name
   , split   => Split
   }.

-spec name(Dataset::dataset()) -> name().
name(#{name := Name}) -> Name.

-spec split(Dataset::dataset()) -> split().
split(#{split := Split}) -> Split.

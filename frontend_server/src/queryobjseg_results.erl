%%% @doc Results Model
-module(queryobjseg_results).

-behaviour(sumo_doc).
-behaviour(sumo_rest_doc).
% -include_lib("amqp_client/include/amqp_client.hrl").

-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

-type id() :: binary().
-type dataset() :: queryobjseg_datasets:name().
-type ref_id() :: binary().
-type ann_id() :: binary().
-type img_id() :: binary().
-type query_expr() :: binary().
-type mask() :: binary().
-type prev_id() :: id().
-type next_id() :: id().
-type split() :: queryobjseg_datasets:split().

-opaque result() ::
  #{ id        => id()
   , ref_id    => ref_id()
   , ann_id    => ann_id()
   , img_id    => img_id()
   , split   => split()
   , query_expr => query_expr()
   , mask => mask()
   , prev_id => prev_id()
   , next_id => next_id()
   }.

-export_type(
  [ id/0
  , ref_id/0
  , ann_id/0
  , img_id/0
  , split/0
  , query_expr/0
  , mask/0
  , prev_id/0
  , next_id/0
  , result/0
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
  [ new/9
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
    , sumo:new_field(ref_id, binary, [not_null])
    , sumo:new_field(img_id, binary, [not_null])
    , sumo:new_field(ann_id, binary, [not_null])
    , sumo:new_field(split, binary, [not_null])
    , sumo:new_field(query_expr, binary, [not_null])
    , sumo:new_field(mask, binary, [not_null])
    , sumo:new_field(prev_id, binary, [not_null])
    , sumo:new_field(next_id, binary, [not_null])
    ]).

%% @doc Convert a result from its system representation to sumo's
%%      internal one.
-spec sumo_sleep(Result::result()) -> sumo:model().
sumo_sleep(Result) -> Result.

%% @doc Convert a result from sumo's internal representation to its
%%      system one.
-spec sumo_wakeup(Result::sumo:doc()) -> result().
sumo_wakeup(Result) -> Result.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo_rest_doc behaviour callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Convert a result from its system representation to json.
-spec to_json(Result::result()) -> sr_json:json().
to_json(Result) ->
  #{ id  => maps:get(id, Result)
   , ref_id => maps:get(ref_id, Result)
   , img_id => maps:get(img_id, Result)
   , ann_id => maps:get(img_id, Result)
   , query_expr => maps:get(query_expr, Result)
   , mask => maps:get(mask, Result)
   , prev_id  => maps:get(prev_id, Result)
   , next_id => maps:get(next_id, Result)
   , split   => maps:get(split, Result)
   }.

%% @doc Convert a result from json to its system representation.
-spec from_json(Json::sumo_rest_doc:json()) ->
  {ok, result()} | {error, iodata()}.
from_json(Json) ->
  Now = sr_json:encode_date(calendar:universal_time()),
  try
    A = { ok
    , #{ id => sr_json:decode_null(maps:get(<<"id">>, Json, null))
       , ref_id => maps:get(<<"ref_id">>, Json)
        , img_id => maps:get(<<"img_id">>, Json)
        , ann_id => maps:get(<<"img_id">>, Json)
        , query_expr => maps:get(<<"query_expr">>, Json)
        , mask => maps:get(<<"mask">>, Json)
        , prev_id  => maps:get(<<"prev_id">>, Json)
        , next_id => maps:get(<<"next_id">>, Json)
        , split   => maps:get(<<"split">>, Json)
       }
    },
    io:format("~p~n", [A]),
    A
  catch
    _: {badkey, Key} -> {error, <<"missing field: ", Key/binary>>}
  end.

-spec update(Result::result(), Json::sumo_rest_doc:json()) ->
  {ok, result()} | {error, iodata()}.
update(Result, Json) ->
  try
    {ok, Result}
  catch
    _:{badkey, Key} -> {error, <<"missing field: ", Key/binary>>}
  end.

%% @doc Specify the uri part that uniquely identifies a Result.
-spec location(Result::result(), Path::sumo_rest_doc:path()) -> iodata().
location(Result, Path) ->
  ?PRINT("New!"),
  iolist_to_binary([Path, $/, id(Result)]).

%% @doc Optional callback duplication_conditions/1 to let sumo_rest avoid
%%      duplicated keys (and return `422 Conflict` in that case).
-spec duplication_conditions(result()) ->
  sumo_rest_doc:duplication_conditions().
duplication_conditions(Result) ->
  {id, id(Result)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new(Id::id(), RefId::ref_id(), AnnId::ann_id(), ImgId::img_id(),
          Split::split(), Query::query_expr(), Mask::mask(),
          PrevId::prev_id(), NextId::next_id()) -> result().
new(Id, RefId, AnnId, ImgId, Split, Query, Mask, PrevId, NextId) ->
  % amqp_channel:close(Channel),
  #{ id  => Id
   , ref_id => RefId
   , ann_id   => AnnId
   , img_id   => ImgId
   , split   => Split
   , query_expr   => Query
   , mask   => Mask
   , next_id   => NextId
   , prev_id => PrevId
   }.

-spec id(Result::result()) -> id().
id(#{id := Id}) -> Id.

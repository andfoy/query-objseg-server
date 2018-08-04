
-module(queryobjseg_unc).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ queryobjseg_results
        , [ sumo_sleep/1
          , sumo_wakeup/1
          , to_json/1
          , from_json/1
          , update/2
          , location/2
          , duplication_conditions/1
          , new/9
          , id/1
          ]}]).

-export([sumo_schema/0]).

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  % ListSchema = plain_schema(),
  sumo:new_schema(
    ?MODULE,
    [ sumo:new_field(id, binary, [id, unique])
    , sumo:new_field(ref_id, binary, [not_null])
    , sumo:new_field(img_id, binary, [not_null])
    , sumo:new_field(ann_id, binary, [not_null])
    , sumo:new_field(img_url, binary, [not_null])
    , sumo:new_field(split, binary, [not_null])
    , sumo:new_field(query_expr, binary, [not_null])
    , sumo:new_field(mask, binary, [not_null])
    , sumo:new_field(prev_id, binary, [not_null])
    , sumo:new_field(next_id, binary, [not_null])
    ]).

% -export([from_json/1]).
%
% -spec from_json(Json::sumo_rest_doc:json()) ->
%   {ok, queryobjseg_results:result()} | {error, iodata()}.
% from_json(Json) ->
%   queryobjseg_results:from_json(Json).

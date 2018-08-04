%%% @doc POST|GET /datasets/unc handler.
-module(queryobjseg_unc_handler).

-behaviour(trails_handler).

-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_entities_handler
        , [ init/3
          , rest_init/2
          , allowed_methods/2
          , resource_exists/2
          , content_types_accepted/2
          , content_types_provided/2
          , handle_get/2
          , handle_post/3
          ]
        }]).

%% Alias
-type state() :: sr_entities_handler:state().

-export([trails/0, handle_post/2]).

-spec trails() -> trails:trails().
trails() ->
  RequestBody =
    #{ name => <<"request body">>
     , in => body
     , description => <<"request body (as json)">>
     , required => true
     },
  Metadata =
    #{ get =>
       #{ tags => ["segmentations"]
        , description => "Returns the list of segmentations"
        , produces => ["application/json"]
        }
     , post =>
       # { tags => ["segmentations"]
         , description => "Creates a new segmentation"
         , consumes => ["application/json", "application/json; charset=utf-8"]
         , produces => ["application/json"]
         , parameters => [RequestBody]
         }
     },
  Path = "/datasets/unc",
  Options = #{path => Path, model => queryobjseg_unc, verbose => true},
  [trails:trail(Path, ?MODULE, Options, Metadata)].

-spec handle_post(Req::cowboy_req:req(), State::state()) ->
  { {true, binary()} | false | halt
  , cowboy_req:req()
  , state()
  }.
handle_post(Req, State) ->
  try
    {ok, Body, Req1}      = cowboy_req:body(Req),
    Json                  = sr_json:decode(Body),
    % {DeviceId, _Req} = cowboy_req:binding(device_id, Req),
    % ?PRINT(DeviceId),
    % Checks that the given device does exists
    Dataset = maps:get(<<"dataset">>, Json),
    case queryobjseg_datasets_repo:exists(Dataset) of
      true ->
        case queryobjseg_unc:from_json(Json) of
          {error, Reason} ->
            Req2 = cowboy_req:set_resp_body(sr_json:error(Reason), Req1),
            {false, Req2, State};
          {ok, Entity} ->
            handle_post(Entity, Req1, State)
        end;
      false ->
        {ok, Req1} = cowboy_req:reply(404, Req),
        {halt, Req1, State}
    end
  catch
    _:badjson ->
      Req3 =
        cowboy_req:set_resp_body(
          sr_json:error(<<"Malformed JSON request">>), Req),
      {false, Req3, State}
  end.

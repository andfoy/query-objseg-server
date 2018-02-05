%%% @doc POST|GET /segmentations handler.
-module(queryobjseg_segmentations_handler).

-behaviour(trails_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_entities_handler
        , [ init/3
          , rest_init/2
          , allowed_methods/2
          , resource_exists/2
          , content_types_accepted/2
          , content_types_provided/2
          , handle_get/2
          , handle_post/2
          ]
        }]).

-export([trails/0]).

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
  Path = "/segmentations",
  Options = #{path => Path, model => queryobjseg_segmentations, verbose => true},
  [trails:trail(Path, ?MODULE, Options, Metadata)].

%%% @doc POST|GET /datasets handler.
-module(queryobjseg_datasets_handler).

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
       #{ tags => ["datasets"]
        , description => "Returns the list of datasets"
        , produces => ["application/json"]
        }
     , post =>
       # { tags => ["datasets"]
         , description => "Creates a new device"
         , consumes => ["application/json", "application/json; charset=utf-8"]
         , produces => ["application/json"]
         , parameters => [RequestBody]
         }
     },
  Path = "/datasets",
  Options = #{path => Path, model => queryobjseg_datasets, verbose => true},
  [trails:trail(Path, ?MODULE, Options, Metadata)].

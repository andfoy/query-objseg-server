%%% @doc GET|PUT|DELETE /datasets/unc/:id handler
-module(queryobjseg_single_unc_handler).

-behaviour(trails_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_single_entity_handler
        , [ init/3
          , rest_init/2
          , allowed_methods/2
          , resource_exists/2
          , content_types_accepted/2
          , content_types_provided/2
          , handle_get/2
          , handle_put/2
          , delete_resource/2
          ]
        }]).

-export([ trails/0 ]).

-spec trails() -> trails:trails().
trails() ->
  RequestBody =
    #{ name => <<"request body">>
     , in => body
     , description => <<"request body (as json)">>
     , required => true
     },
  Id =
    #{ name => id
     , in => path
     , description => <<"Newspaper key">>
     , required => true
     , type => string
     },
  Metadata =
    #{ get =>
       #{ tags => ["devices"]
        , description => "Returns a device"
        , produces => ["application/json"]
        , parameters => [Id]
        }
     , put =>
       #{ tags => ["devices"]
        , description => "Updates or creates a new device"
        , consumes => ["application/json", "application/json; charset=utf-8"]
        , produces => ["application/json"]
        , parameters => [RequestBody, Id]
        }
     , delete =>
       #{ tags => ["devices"]
        , description => "Deletes a device"
        , parameters => [Id]
        }
     },
  Path = "/datasets/unc/:id",
  Options = #{path => Path, model => queryobjseg_unc, verbose => true},
  [trails:trail(Path, ?MODULE, Options, Metadata)].

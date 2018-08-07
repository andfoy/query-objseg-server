-module(queryobjseg_masks_handler).
-behaviour(trails_handler).

%% trails_handler behaviour callback
-export([trails/0]).

%% API
% -export([notify/1]).

%% lasse_handler behaviour callbacks
% -export([ init/2
%         , websocket_init/1
%         , websocket_handle/2
%         , websocket_info/2
%         ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% trails_handler callback
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec trails() -> trails:trails().
trails() ->
  Metadata =
    #{ get =>
       #{ tags => ["masks"]
        , summary =>
          "WARNING: Do not try to use this endpoint from this page."
          " Swagger doesn't understand WS"
        , description =>
          "Opens an [SSE] (http://www.w3.org/TR/eventsource/)"
          " connection to retrieve results updates"
        , externalDocs =>
          #{ description => "RFC"
           , url => "http://www.w3.org/TR/eventsource/"
           }
        , produces => ["application/json"]
        }
     },
  Path = "/masks/[...]",
  % Options = #{},
  % Options = #{module => queryobjseg_ws, init_args => #{path => Path}},
  [trails:trail(Path, cowboy_static, {dir, "/var/www/masks/"}, Metadata)].


-module(queryobjseg_unc).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ queryobjseg_results
        , [ sumo_schema/0
          , sumo_sleep/1
          , sumo_wakeup/1
          , to_json/1
          , from_json/1
          , update/2
          , location/2
          , duplication_conditions/1
          , new/9
          , id/1
          ]}]).

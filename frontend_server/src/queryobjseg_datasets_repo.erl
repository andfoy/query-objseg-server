%%% @doc Datasets repository
-module(queryobjseg_datasets_repo).

-export([exists/1]).

%% @doc Checks that there is a dataset with the given name.
-spec exists(Name::queryobjseg_datasets:name()) -> boolean().
exists(Name) ->
  notfound /= sumo:find_one(queryobjseg_datasets, {name, Name}).

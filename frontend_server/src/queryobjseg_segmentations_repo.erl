%%% @doc Segmentations repository
-module(queryobjseg_segmentations_repo).

-export([exists/1]).

%% @doc Checks that there is a segmentation with the given name.
-spec exists(SegmentationName::queryobjseg_segmentations:name()) -> boolean().
exists(SegmentationName) ->
  notfound /= sumo:fetch(queryobjseg_segmentations, SegmentationName).

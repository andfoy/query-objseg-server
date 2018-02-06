%%% @doc Devices repository
-module(queryobjseg_devices_repo).

-export([exists/1]).

%% @doc Checks that there is a device with the given name.
-spec exists(DeviceId::queryobjseg_devices:deviceid()) -> boolean().
exists(DeviceId) ->
  notfound /= sumo:fetch(queryobjseg_devices, DeviceId).

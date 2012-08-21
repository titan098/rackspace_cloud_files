-module(rackspace_cloud_files).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    rackspace_cloud_files_sup:start_link().

stop(_State) ->
    ok.
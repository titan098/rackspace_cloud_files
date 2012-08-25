-module(rackspace_cloud_files).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%%a wrapper to start the application
start() ->
	application:start(rackspace_cloud_files).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    rackspace_cloud_files_sup:start_link().

stop(_State) ->
    ok.
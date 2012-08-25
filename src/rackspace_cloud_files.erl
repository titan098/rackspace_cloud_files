%%% -------------------------------------------------------------------
%%% Author  : David Ellefsen
%%% Description : This module starts the rackspace_cloud_files application
%%%  it also contains a functions that will be executed by the rackspace_cloud_files_srv
%%% Licence: Apache 2.0 License
%%%
%%% Created : Aug 21, 2012
%%% -------------------------------------------------------------------
%
%   Copyright 2012 David Ellefsen
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.
%
-module(rackspace_cloud_files).

-behaviour(application).

-define(RACKSPACE_CLOUDFILE_SRV, rackspace_cloud_files_srv).

%% Application callbacks
-export([start/0, start/2, stop/1]).

-export([get_auth_token/3, close/0]).
-export([retrieve_account_metadata/0]).
-export([list_containers/0, create_container/1, create_container/2, delete_container/1, retrieve_container_metadata/1, modify_container_metadata/2]).
-export([list_objects/1, list_objects/2, get_object/2, get_object/3, upload_object/3, upload_object/4, upload_object/5, create_object/3, create_object/4, create_object/5]).
-export([copy_object/4, move_object/4, delete_object/2]).
-export([retrieve_object_metadata/2, modify_object_metadata/3]).

-export([cdn_list_container/0, cdn_list_container/1, cdn_list_container/2]).
-export([cdn_enable/3, cdn_disable/1]).
-export([cdn_retrieve_metadata/1, cdn_update_metadata/4]).
-export([cdn_purge_object/2, cdn_purge_object/3]).

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

%% --------------------------------------------------------------------
%% Normal Cloudfiles Functions
%% --------------------------------------------------------------------

get_auth_token(URL, Username, APIKey) ->
	rackspace_cloud_files_srv:get_auth_token(URL, Username, APIKey).

close() ->
	rackspace_cloud_files_srv:close().

list_containers() ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, list_containers, []}).

create_container(Container) ->
	create_container(Container, []).
create_container(Container, Metadata) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, create_container, [Container, Metadata]}).

delete_container(Container) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, delete_container, [Container]}).

retrieve_container_metadata(Container) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, retrieve_container_metadata, [Container]}).

modify_container_metadata(Container, Metadata) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, modify_container_metadata, [Container, Metadata]}).

retrieve_account_metadata() ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, retrieve_account_metadata, []}).

list_objects(Container) -> list_objects(Container, "").
list_objects(Container, Format) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, list_objects, [Container, Format]}).

get_object(Container, Object) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, get_object, [Container, Object]}).

get_object(Container, Object, OutFile) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, get_object, [Container, Object, OutFile]}).

upload_object(Container, Object, FileName) ->
	upload_object(Container, Object, FileName, [], []).
upload_object(Container, Object, FileName, Metadata) ->
	upload_object(Container, Object, FileName, Metadata,[]).
upload_object(Container, Object, FileName, Metadata, Options) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, upload_object, [Container, Object, FileName, Metadata, Options]}).

create_object(Container, Object, Data) ->
	create_object(Container, Object, Data, [], []).
create_object(Container, Object, Data, Metadata) ->
	create_object(Container, Object, Data, Metadata, []).
create_object(Container, Object, Data, Metadata, Options) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, create_object, [Container, Object, Data, Metadata, Options]}).

copy_object(SourceContainer, SourceObject, DestinationContainer, DestinationObject) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, copy_object, [SourceContainer, SourceObject, DestinationContainer, DestinationObject]}).	

move_object(SourceContainer, SourceObject, DestinationContainer, DestinationObject) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, move_object, [SourceContainer, SourceObject, DestinationContainer, DestinationObject]}).

delete_object(Container, Object) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, delete_object, [Container, Object]}).

retrieve_object_metadata(Container, Object) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, retrieve_object_metadata, [Container, Object]}).

modify_object_metadata(Container, Object, Metadata) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, modify_object_metadata, [Container, Object, Metadata]}).

%% --------------------------------------------------------------------
%% CDN Functions
%% --------------------------------------------------------------------

cdn_list_container() ->
	cdn_list_container("plain", "false").
cdn_list_container(Enabled) ->
	cdn_list_container("plain", Enabled).
cdn_list_container(Format, Enabled) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, cdn_list_container, [Format, Enabled]}).

cdn_enable(Container, TTL, LogRetention) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, cdn_enable, [Container, TTL, LogRetention]}).

cdn_disable(Container) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, cdn_disable, [Container]}).
	
cdn_retrieve_metadata(Container) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, cdn_retrieve_metadata, [Container]}).

cdn_update_metadata(Container, Enabled, TTL, LogRetention) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, cdn_update_metadata, [Container, Enabled, TTL, LogRetention]}).
	
cdn_purge_object(Container, Object) ->
	cdn_purge_object(Container, Object, "").

cdn_purge_object(Container, Object, PurgeEmail) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, cdn_purge_object, [Container, Object, PurgeEmail]}).

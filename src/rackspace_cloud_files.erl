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
-define(RACKSPACE_TIMEOUT, 60000). % 1 min

%% Application callbacks
-export([start/0, start/2, stop/1]).

-export([get_auth_token/3, close/0]).
-export([retrieve_account_metadata/0]).
-export([list_containers/0, create_container/1, create_container/2, delete_container/1, retrieve_container_metadata/1, modify_container_metadata/2]).
-export([list_objects/1, list_objects/2, get_object/2, get_object/3, upload_object/3, upload_object/4, upload_object/5, create_object/3, create_object/4, create_object/5]).
-export([copy_object/4, move_object/4, delete_object/2]).
-export([delete_object_at/3, delete_object_after/3]).
-export([retrieve_object_metadata/2, modify_object_metadata/3]).
-export([retrieve_object_headers/2, modify_object_headers/3]).
-export([tempurl_set_key/1, tempurl_create_url/5]).

-export([cdn_list_container/0, cdn_list_container/1, cdn_list_container/2]).
-export([cdn_enable/3, cdn_disable/1]).
-export([cdn_retrieve_metadata/1, cdn_update_metadata/4]).
-export([cdn_purge_object/2, cdn_purge_object/3]).
-export([cdn_get_object_url/3]).

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
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, list_containers, []}, ?RACKSPACE_TIMEOUT).

create_container(Container) ->
	create_container(Container, []).
create_container(Container, Metadata) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, create_container, [Container, Metadata]}, ?RACKSPACE_TIMEOUT).

delete_container(Container) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, delete_container, [Container]}, ?RACKSPACE_TIMEOUT).

retrieve_container_metadata(Container) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, retrieve_container_metadata, [Container]}, ?RACKSPACE_TIMEOUT).

modify_container_metadata(Container, Metadata) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, modify_container_metadata, [Container, Metadata]}, ?RACKSPACE_TIMEOUT).

retrieve_account_metadata() ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, retrieve_account_metadata, []}, ?RACKSPACE_TIMEOUT).

list_objects(Container) -> list_objects(Container, "").
list_objects(Container, Format) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, list_objects, [Container, Format]}, ?RACKSPACE_TIMEOUT).

get_object(Container, Object) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, get_object, [Container, Object]}, ?RACKSPACE_TIMEOUT).

get_object(Container, Object, OutFile) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, get_object, [Container, Object, OutFile]}, ?RACKSPACE_TIMEOUT).

upload_object(Container, Object, FileName) ->
	upload_object(Container, Object, FileName, [], []).
upload_object(Container, Object, FileName, Metadata) ->
	upload_object(Container, Object, FileName, Metadata,[]).
upload_object(Container, Object, FileName, Metadata, Options) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, upload_object, [Container, Object, FileName, Metadata, Options]}, ?RACKSPACE_TIMEOUT).

create_object(Container, Object, Data) ->
	create_object(Container, Object, Data, [], []).
create_object(Container, Object, Data, Metadata) ->
	create_object(Container, Object, Data, Metadata, []).
create_object(Container, Object, Data, Metadata, Options) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, create_object, [Container, Object, Data, Metadata, Options]}, ?RACKSPACE_TIMEOUT).

copy_object(SourceContainer, SourceObject, DestinationContainer, DestinationObject) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, copy_object, [SourceContainer, SourceObject, DestinationContainer, DestinationObject]}, ?RACKSPACE_TIMEOUT).	

move_object(SourceContainer, SourceObject, DestinationContainer, DestinationObject) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, move_object, [SourceContainer, SourceObject, DestinationContainer, DestinationObject]}, ?RACKSPACE_TIMEOUT).

delete_object(Container, Object) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, delete_object, [Container, Object]}, ?RACKSPACE_TIMEOUT).

retrieve_object_metadata(Container, Object) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, retrieve_object_metadata, [Container, Object]}, ?RACKSPACE_TIMEOUT).

modify_object_metadata(Container, Object, Metadata) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, modify_object_metadata, [Container, Object, Metadata]}, ?RACKSPACE_TIMEOUT).

retrieve_object_headers(Container, Object) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, retrieve_object_headers, [Container, Object]}, ?RACKSPACE_TIMEOUT).

modify_object_headers(Container, Object, Metadata) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, modify_object_headers, [Container, Object, Metadata]}, ?RACKSPACE_TIMEOUT).

delete_object_at(Container, Object, Time) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, modify_object_delete_at, [Container, Object, Time]}, ?RACKSPACE_TIMEOUT).

delete_object_after(Container, Object, Time) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, modify_object_delete_after, [Container, Object, Time]}, ?RACKSPACE_TIMEOUT).

tempurl_set_key(Key) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, tempurl_set_key, [Key]}, ?RACKSPACE_TIMEOUT).

tempurl_create_url(Method, Container, Object, Seconds, Key) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, tempurl_create_url, [Method, Container, Object, Seconds, Key]}, ?RACKSPACE_TIMEOUT).

%% --------------------------------------------------------------------
%% CDN Functions
%% --------------------------------------------------------------------

cdn_list_container() ->
	cdn_list_container("plain", "false").
cdn_list_container(Enabled) ->
	cdn_list_container("plain", Enabled).
cdn_list_container(Format, Enabled) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, cdn_list_container, [Format, Enabled]}, ?RACKSPACE_TIMEOUT).

cdn_enable(Container, TTL, LogRetention) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, cdn_enable, [Container, TTL, LogRetention]}, ?RACKSPACE_TIMEOUT).

cdn_disable(Container) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, cdn_disable, [Container]}, ?RACKSPACE_TIMEOUT).
	
cdn_retrieve_metadata(Container) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, cdn_retrieve_metadata, [Container]}, ?RACKSPACE_TIMEOUT).

cdn_update_metadata(Container, Enabled, TTL, LogRetention) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, cdn_update_metadata, [Container, Enabled, TTL, LogRetention]}, ?RACKSPACE_TIMEOUT).
	
cdn_purge_object(Container, Object) ->
	cdn_purge_object(Container, Object, "").

cdn_purge_object(Container, Object, PurgeEmail) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, cdn_purge_object, [Container, Object, PurgeEmail]}, ?RACKSPACE_TIMEOUT).

cdn_get_object_url(URLType, Container, Object) ->
	gen_server:call(?RACKSPACE_CLOUDFILE_SRV, {execute_function, cdn_get_object_url, [URLType, Container, Object]}, ?RACKSPACE_TIMEOUT).

%%% -------------------------------------------------------------------
%%% Author  : David Ellefsen
%%% Description : This module contains all of the functions required to
%%%  interact with the Rackspace Cloud Files API - if this module is used
%%%  directly, state information must be passed to each function as it is used
%%% Licence: Apache 2.0 License
%%%
%%% Created : Aug 25, 2012
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
-module(rackspace_cloud_files_api).

%%
%% Include files
%%

-include("rackspace_cloud_files.hrl").

%%
%% Exported Functions
%%
-export([get_auth_token/1, get_auth_token/3]).
-export([retrieve_account_metadata/1]).
-export([list_containers/1, create_container/2, create_container/3, delete_container/2, retrieve_container_metadata/2, modify_container_metadata/3]).
-export([list_objects/2, list_objects/3, get_object/3, get_object/4, upload_object/4, upload_object/5, upload_object/6, create_object/4, create_object/5, create_object/6]).
-export([copy_object/5, move_object/5, delete_object/3]).
-export([modify_object_delete_at/4, modify_object_delete_after/4]).
-export([retrieve_object_metadata/3, modify_object_metadata/4, retrieve_object_headers/3, modify_object_headers/4]).

-export([cdn_list_container/1, cdn_list_container/2, cdn_list_container/3]).
-export([cdn_enable/4, cdn_disable/2]).
-export([cdn_retrieve_metadata/2, cdn_update_metadata/5]).
-export([cdn_purge_object/3, cdn_purge_object/4]).
-export([cdn_get_object_url/4]).

-export([tempurl_set_key/2, tempurl_create_url/6]).

%%
%% API Functions
%%

%%
%% Gets the auth tokens from the Rackspace Auth Service
%%
get_auth_token(uk, Username, APIKey) ->
	get_auth_token("https://lon.identity.api.rackspacecloud.com/v1.0", Username, APIKey);
get_auth_token(us, Username, APIKey) ->
	get_auth_token("https://identity.api.rackspacecloud.com/v1.0", Username, APIKey);
get_auth_token(URL, Username, APIKey) ->
	{ok, Code, Header, Content} = ibrowse:send_req(URL, [{"X-Auth-User", Username},{"X-Auth-Key", APIKey}], get),
	
	case list_to_integer(Code) of
		Status when (Status >= 200) and (Status < 300) -> {ok, #state{url = URL,
																	  username = Username,
																	  apikey = APIKey,
																	  token = get_header("X-Auth-Token", Header),
																	  tokenage = epochSeconds(erlang:universaltime()),
																	  storage_url = get_header("X-Storage-Url", Header),
																	  cdn_url = get_header("X-CDN-Management-Url", Header)}};
		_ -> {error, {Code,Content}}
	end.

get_auth_token(#state{url = URL, username = Username, apikey = ApiKey}) ->
	{ok, State} = get_auth_token(URL, Username, ApiKey),
	State.

%% --------------------------------------------------------------------
%% Normal Cloudfiles Functions
%% --------------------------------------------------------------------

%%
%% Lists all of the containers.
%% Returns [string|string|string|...]
%%
list_containers(State) ->
	{ok, Code, _Header, Content} = send_authed_query(State, get),
	
	case list_to_integer(Code) of
		204 -> {ok, []};
		200 -> {ok, string:tokens(Content, "\n")};
		_ -> {error, Content}
	end.

%%
%% Create a container
%% Returns: ok on sucess
%%  {error, exists} if the container already exists
%%  {error, Content} if another error is returned
%%
create_container(State, Container) ->
	create_container(State, Container, []).

%%
%% Create a container while specifying container metadata
%% Metadata is of the form [{Key, Value}, {Key, Value}, ...]
%%
create_container(State, Container, Metadata) ->
		{ok, Code, _Header, Content} = send_authed_query(State, "/" ++ Container, create_metadata_headers("X-Container-Meta-", Metadata), put),
	
	case list_to_integer(Code) of
		201 -> ok;
		202 -> {error, exists};
		_ -> {error, Content}
	end.

%%
%% Delete a container
%% Returns: ok on success
%%  {error, does_not_exist} if the container does not exist
%%  {error, not_empty} if the container is not empty
%%  error on any other error
delete_container(State, Container) ->
	{ok, Code, _Header, _Content} = send_authed_query(State, "/" ++ Container, delete),
	
	case list_to_integer(Code) of
		204 -> ok;
		404 -> {error, does_not_exist};
		409 -> {error, not_empty};
		_ -> error
	end.

%%
%% Retrieve the container metadata
%% Returns: [{Key, Value}, {Key, Value}, ...]
%%  {error, does_not_exist} if the container does not exist
%%  error on any other error
%%
retrieve_container_metadata(State, Container) ->
	{ok, Code, Header, _Content} = send_authed_query(State, "/" ++ Container, head),
	
	case list_to_integer(Code) of
		204 -> {ok, [{"Object-Count", get_header("X-Container-Object-Count", Header)},
				{"Bytes-Used", get_header("X-Container-Bytes-Used", Header)} | extract_metadata_headers("X-Container-Meta-", Header)]};
		404 -> {error, does_not_exist};
		_ -> error
	end.

%%
%% Modify the container metadata
%% Returns: ok on success
%%  {error, does_not_exist} if the container does not exist
%%  error on any other error
%%
modify_container_metadata(State, Container, Metadata) ->
	{ok, Code, _Header, _Content} = send_authed_query(State, "/" ++ Container, create_metadata_headers("X-Container-Meta-", Metadata), post),
	
	case list_to_integer(Code) of
		204 -> ok;
		404 -> {error, does_not_exist};
		_ -> error
	end.

%%
%% Retrieves the account metadata
%% Returns: [{"Container-Count", string}, {"Bytes-Used", string}]
%%  {error, string} if an error occurs
%%
retrieve_account_metadata(State) ->
	{ok, Code, Header, Content} = send_authed_query(State, head),
	
	case list_to_integer(Code) of
		204 -> {ok, [{"Container-Count", get_header("X-Account-Container-Count", Header)},
				{"Bytes-Used", get_header("X-Account-Bytes-Used", Header)}]};
		_ -> {error, Content}
	end.

%%
%% List all objects in a container (plain)
%% Returns: [string, string, string]
%%
list_objects(State, Container) -> list_objects(State, Container, "").

%%
%% List all objects in a container (xml|json)
%% Returns: an XML or JSON formatted string object
%%
list_objects(State, Container, xml) -> list_objects(State, Container, "xml");
list_objects(State, Container, json) -> list_objects(State, Container, "json");
list_objects(State, Container, Format) ->
	{ok, Code, _Header, Content} = send_authed_query(State, "/" ++ Container ++ "?format=" ++ Format, get),
	
	case list_to_integer(Code) of
		204 -> {ok, []};
		200 -> {ok, string:tokens(Content, "\n")};
		_ -> {error, Content}
	end.

%%
%% Get an object from a container
%%  the hash returned by Cloudfiles will be verified and the data will be returned to the user
%% Returns: {content, Content} on success
%%  {error, ErrorMessage} || error on error.
%%
get_object(State, Container, Object) ->
	{ok, Code, Header, Content} = send_authed_query(State, "/" ++ Container ++ "/" ++ Object, get),

	case list_to_integer(Code) of
		200 -> check_return_content(list_to_binary(Content), get_header("Etag", Header));
		400 -> {error, does_not_exist};
		_ -> error
	end.

%%
%% Get an object and save the contents to a file
%%
get_object(State, Container, Object, OutFile) ->
	case get_object(State, Container, Object) of
		{ok, Content} ->
			{ok, IODevice} = file:open(OutFile, [write, binary]),
			file:write(IODevice, Content),
			file:close(IODevice),
			ok;
		{error, Error} -> {error, Error};
		_ -> error
	end.

%%
%% Upload an object to a specified container
%%  the container and the object name number be specified
%%
upload_object(State, Container, Object, FileName) ->
	upload_object(State, Container, Object, FileName, [], []).

%%
%% Upload an object to a specified container passing any Metadata as required
%%  the container and the object name number be specified
%%
upload_object(State, Container, Object, FileName, Metadata) ->
	upload_object(State, Container, Object, FileName, Metadata,[]).

%%
%% Upload an object to a specified container passing any Metadata and options as required
%%  the container and the object name number be specified
%%
upload_object(State, Container, Object, FileName, Metadata, Options) ->
	case file:read_file(FileName) of
		{ok, FileData} ->
			create_object(State, Container, Object, FileData, Metadata, Options);
		{error, ErrorMessage} -> {error, ErrorMessage};
		_ -> error
	end.

%%
%% Create an object with passed binary data
%% 
create_object(State, Container, Object, Data) ->
	create_object(State, Container, Object, Data, [], []).

%%
%% Create an object with passed binary data and any Metadata
%%
create_object(State, Container, Object, Data, Metadata) ->
	create_object(State, Container, Object, Data, Metadata, []).

%%
%% Create an object with passed binary data, Metatdata, and any passed options
%%
create_object(State, Container, Object, Data, Metadata, Options) ->
	%get any options that will be passed to ibrowse (chunked etc.)
	ConnOptions = get_object_options(Options),
	
	% Compress the object if required
	NewData = compress_object(Data, Options),
	
	%calculate an MD5 hash of the object and append any headers (metadata) etc.
	Headers = [{"Etag", md5(NewData)}] ++ get_object_headers(Options) ++ create_metadata_headers("X-Object-Meta-", Metadata),
	{ok, Code, _Header, _Content} = send_authed_query(State, "/" ++ Container ++ "/" ++ Object, Headers, put, NewData, ConnOptions),

	case list_to_integer(Code) of
		201 -> ok;
		401 -> {error, unauthorised};
		411 -> {error, length_required};
		422 -> {error, hash_mismatch};
		_ -> {error, Code}
	end.

%%
%% Copy an object from one container to another
%%
copy_object(State, SourceContainer, SourceObject, DestinationContainer, DestinationObject) ->
	Headers = [{"Destination", DestinationContainer ++ "/" ++ DestinationObject}],
	{ok, Code, _Header, _Content} = send_authed_query(State, "/" ++ SourceContainer ++ "/" ++ SourceObject, Headers, copy),

	case list_to_integer(Code) of
		201 -> ok;
		404 -> {error, does_not_exist};
		_ -> error
	end.

%%
%% Move an object from one container to another
%%
move_object(State, SourceContainer, SourceObject, DestinationContainer, DestinationObject) ->
	case copy_object(State, SourceContainer, SourceObject, DestinationContainer, DestinationObject) of
		ok -> delete_object(State, SourceContainer, SourceObject);
		_ -> error
	end.

%%
%% delete an object from a container
%%
delete_object(State, Container, Object) ->
	{ok, Code, _Header, _Content} = send_authed_query(State, "/" ++ Container ++ "/" ++ Object, delete),

	case list_to_integer(Code) of
		204 -> ok;
		404 -> {error, does_not_exist};
		_ -> error
	end.

%%
%% Retrieve the object metadata
%% Returns: [{Key, Value}, {Key, Value}, ...]
%%  {error, does_not_exist} if the object does not exist
%%  error on any other error
%%
retrieve_object_metadata(State, Container, Object) ->
	{ok, Code, Header, _Content} = send_authed_query(State, "/" ++ Container ++ "/" ++ Object, head),
	
	case list_to_integer(Code) of
		200 -> {ok, [{"Content-Length", get_header("Content-Length", Header)},
				{"Content-Type", get_header("Content-Type", Header)},
				{"Last-Modified", get_header("Last-Modified", Header)},
				{"Hash", get_header("Etag", Header)} | extract_metadata_headers("X-Object-Meta-", Header)]};
		404 -> {error, does_not_exist};
		_ -> error
	end.

%%
%% Modify the object headers
%% Headers are in the form of [{Header, Value}, {Header, Value}]
%%
modify_object_headers(State, Container, Object, Headers) ->
	{ok, Code, _Header, _Content} = send_authed_query(State, "/" ++ Container ++ "/" ++ Object, Headers, post),
	
	case list_to_integer(Code) of
		202 -> ok;
		404 -> {error, does_not_exist};
		_ -> error
	end.

%%
%% Retrieve the raw object headers
%%
retrieve_object_headers(State, Container, Object) ->
	{ok, Code, Header, _Content} = send_authed_query(State, "/" ++ Container ++ "/" ++ Object, head),
	
	case list_to_integer(Code) of
		200 -> {ok, Header};
		404 -> {error, does_not_exist};
		_ -> error
	end.

%%
%% Modify the object metadata
%% Returns: ok on success
%%  {error, does_not_exist} if the object does not exist
%%  error on any other error
%%
modify_object_metadata(State, Container, Object, Metadata) ->
	{ok, Code, _Header, _Content} = send_authed_query(State, "/" ++ Container ++ "/" ++ Object, create_metadata_headers("X-Object-Meta-", Metadata), post),
	
	case list_to_integer(Code) of
		202 -> ok;
		404 -> {error, does_not_exist};
		_ -> error
	end.

%%
%% Modify the amount of time until an existing object is deleted
%% Seconds is the number of seconds until the object is removed
%%
modify_object_delete_after(State, Container, Object, Seconds) ->
	{ok, Code, _Header, _Content} = send_authed_query(State, "/" ++ Container ++ "/" ++ Object, [{"X-Delete-After", Seconds}], post),
	
	case list_to_integer(Code) of
		202 -> ok;
		404 -> {error, does_not_exist};
		_ -> error
	end.

%%
%% Modify the time that the object will be deleted
%% DateTime is a Erlang DateTime term
%% UnixSeconds is the deletion time as a UNIX timestamp
%%
modify_object_delete_at(State, Container, Object, {{_Year,_Mon,_Day}, {_Hour,_Min,_Sec}} = DateTime) ->
	modify_object_delete_at(State, Container, Object, epochSeconds(DateTime));
modify_object_delete_at(State, Container, Object, UnixSeconds) when is_integer(UnixSeconds) ->	
	{ok, Code, _Header, _Content} = send_authed_query(State, "/" ++ Container ++ "/" ++ Object, [{"X-Delete-At", UnixSeconds}], post),
	
	case list_to_integer(Code) of
		202 -> ok;
		404 -> {error, does_not_exist};
		_ -> error
	end.

%% --------------------------------------------------------------------
%% CDN Functions
%% --------------------------------------------------------------------

%%
%% Get a list of CDN container
%% A format can be specified and the enabled status of the containers
%%
cdn_list_container(State) ->
	cdn_list_container(State, "plain", "false").
cdn_list_container(State, Enabled) ->
	cdn_list_container(State, "plain", Enabled).
cdn_list_container(State, xml, Enabled) -> cdn_list_container(State, "xml", Enabled);
cdn_list_container(State, json, Enabled) -> cdn_list_container(State, "json", Enabled);
cdn_list_container(State, Format, Enabled) ->
	{ok, Code, _Header, Content} = send_authed_cdn_query(State, "?format=" ++ Format ++ "&enabled_only=" ++ Enabled, get),
	
	case list_to_integer(Code) of
		204 -> {ok, []};
		200 -> {ok, string:tokens(Content, "\n")};
		_ -> {error, Content}
	end.

%%
%% Enable a container on the CDN
%% A TTL and a LogRetention flag can be specified
%%
cdn_enable(State, Container, TTL, LogRetention) ->
	Headers = [{"X-TTL", TTL}, {"X-Cdn-Enabled", "True"}, {"X-Log-Retention", LogRetention}],
	{ok, Code, Header, Content} = send_authed_cdn_query(State, "/" ++ Container, Headers, put),
	
	case list_to_integer(Code) of
		Val when (Val =:= 201) or (Val =:= 202) ->
			{ok, [{ssl_url, get_header("X-Cdn-Ssl-Uri", Header)},
			 {url, get_header("X-Cdn-Uri", Header)},
			 {streaming_url, get_header("X-Cdn-Streaming-Uri", Header)}]};
		_ -> {error, Content}
	end.

%%
%% Disable a Container on the CDN
%%
cdn_disable(State, Container) ->
	Headers = [{"X-CDN-Enabled", "False"}],
	{ok, Code, _Header, Content} = send_authed_cdn_query(State, "/" ++ Container, Headers, put),
	
	case list_to_integer(Code) of
		Val when (Val =:= 201) or (Val =:= 202) ->
			{ok, true};
		_ -> {error, Content}
	end.

%%
%% Return the Metadata for a container on the CDN
%%
cdn_retrieve_metadata(State, Container) ->
	{ok, Code, Header, Content} = send_authed_cdn_query(State, "/" ++ Container, head),
	
	case list_to_integer(Code) of
		204 ->
			{ok, [{ssl_url, get_header("X-Cdn-Ssl-Uri", Header)},
			 {url, get_header("X-Cdn-Uri", Header)},
			 {streaming_url, get_header("X-Cdn-Streaming-Uri", Header)},
			 {ttl, get_header("X-Ttl", Header)},
			 {enabled, get_header("X-Cdn-Enabled", Header)},
			 {log_retention, get_header("X-Log-Retention", Header)}]};
		404 -> {error, does_not_exist};
		_ -> {error, Content}
	end.

%%
%% Update the Metadata for an object on the CDN
%%  The Enabled flag, TTL, and the LogRetention flag must be specified
%%
cdn_update_metadata(State, Container, Enabled, TTL, LogRetention) ->
	Headers = [{"X-TTL", TTL}, {"X-Cdn-Enabled", Enabled}, {"X-Log-Retention", LogRetention}],
	{ok, Code, Header, Content} = send_authed_cdn_query(State, "/" ++ Container, Headers, post),
	
	case list_to_integer(Code) of
		202 ->
			{ok, [{ssl_url, get_header("X-Cdn-Ssl-Uri", Header)},
			 {url, get_header("X-Cdn-Uri", Header)},
			 {streaming_url, get_header("X-Cdn-Streaming-Uri", Header)}]};
		404 -> {error, does_not_exist};
		_ -> {error, Content}
	end.

%%
%% Purge an object from the CDN
%%
cdn_purge_object(State, Container, Object) ->
	cdn_purge_object(State, Container, Object, "").

%%
%% Purge an object from the CDN
%%  An email address can be specified in the form of "email1, email2"
%%
cdn_purge_object(State, Container, Object, PurgeEmail) ->
	Headers = [{"X-Purge-Email", PurgeEmail}],
	{ok, Code, _Header, Content} = send_authed_cdn_query(State, "/" ++ Container ++ "/" ++ Object, Headers, delete),
	
	case list_to_integer(Code) of
		204 -> ok;
		403 -> {error, not_authorised};
		404 -> {error, does_not_exist};
		498 -> {error, rate_limit};
		_ -> {error, Content}
	end.

cdn_get_object_url(State, URLType, Container, Object) ->
		case cdn_retrieve_metadata(State, Container) of
			{ok, Metadata} ->
				case lists:keyfind(URLType, 1, Metadata) of
					{URLType, URL} -> URL ++ "/" ++ Object;
					_ -> {error, invalid_url_type}
				end;
			_ -> {error, {Container, cdn_not_enabled}}
		end.

%%
%% Set a TempURL key, the key will be valid until it is changed there after it expires after 60 seconds
%%  The key can be any string 
%%
tempurl_set_key(State, Key) when is_list(Key) ->
		Headers = [{"X-Account-Meta-Temp-Url-Key", Key}],
		{ok, Code, _Header, Content} = send_authed_query(State, "", Headers, post),
		
		case list_to_integer(Code) of
				X when (X >= 200) and (X =< 299) -> ok;
				_ -> {error, Content}
		end.

%%
%% Create the TempURL for an object
%% Method should be an atom that is either 'get' or 'put'
%% Key is the key set with tempurl_setkey/2
%% Seconds is the number of seconds until the TempURL expires
%% A string will be return that contains the TempURL
%%
%% The tempurl will be of the form:
%% https://<storage_url>/<version>/<account>/<Container>/<Object>?temp_url_sig=<sha1hmac_sig>&temp_url_expires=<expiretimestamp>
%%
tempurl_create_url(State, Method, Container, Object, Seconds, Key) when is_atom(Method) and ((Method == get) or (Method == put)) ->
	NewMethod = string:to_upper(atom_to_list(Method)),
	EpochSeconds = epochSeconds(erlang:universaltime())+Seconds,
	ObjectPath = parse_path(State#state.storage_url) ++ Container ++ "/" ++ Object,
	Body = NewMethod ++ "\n" ++ integer_to_list(EpochSeconds) ++ "\n" ++ ObjectPath,
	
	Hmac = lists:flatten(lists:map(fun hex_char/1, binary_to_list(crypto:hmac(sha, Key, Body)))),
	State#state.storage_url ++ "/" ++ Container ++ "/" ++ Object ++ "?temp_url_sig=" ++ string:to_lower(Hmac) ++ "&temp_url_expires=" ++ integer_to_list(EpochSeconds);
tempurl_create_url(_State, _Method, _Container, _Object, _Seconds, _Key) ->
	{error, unknown_method}.

%% --------------------------------------------------------------------
%% General Functions
%% --------------------------------------------------------------------

%%
%% Sends a simple authed query to the server with a passed method with
%%  extra URL information and extra headers
%%
send_authed_query(#state{storage_url = _URL, token = _AuthToken} = State, Method) ->
	send_authed_query(State, [], [], Method, [], []).

send_authed_query(#state{storage_url = _URL, token = _AuthToken} = State, PathInfo, Method) ->
	send_authed_query(State, PathInfo, [], Method).

send_authed_query(#state{storage_url = _URL, token = _AuthToken} = State, PathInfo, Headers,  Method) ->
	send_authed_query(State, PathInfo, Headers, Method, []).

send_authed_query(#state{storage_url = _URL, token = _AuthToken} = State, PathInfo, Headers,  Method, Body) ->
	send_authed_query(State, PathInfo, Headers, Method, Body, []).

send_authed_query(#state{storage_url = undefined}, _PathInfo, _Headers, _Method, _Body, _Options) ->	  
	{ok, 403, [], "Not Authorised"};

send_authed_query(#state{storage_url = URL, token = AuthToken}, PathInfo, Headers,  Method, Body, Options) ->
	ibrowse:send_req(URL ++ PathInfo, [{"X-Auth-Token", AuthToken} | Headers], Method, Body, Options).

%%
%% Sends a simple authed query to the server with a passed method with
%%  extra URL information and extra headers - CDN methods
%%
send_authed_cdn_query(#state{cdn_url = _URL, token = _AuthToken} = State, PathInfo, Method) ->
	send_authed_cdn_query(State, PathInfo, [], Method).

send_authed_cdn_query(#state{cdn_url = _URL, token = _AuthToken} = State, PathInfo, Headers,  Method) ->
	send_authed_cdn_query(State, PathInfo, Headers, Method, []).

send_authed_cdn_query(#state{cdn_url = _URL, token = _AuthToken} = State, PathInfo, Headers,  Method, Body) ->
	send_authed_cdn_query(State, PathInfo, Headers, Method, Body, []).

send_authed_cdn_query(#state{cdn_url = undefined}, _PathInfo, _Headers, _Method, _Body, _Options) ->	  
	{ok, 403, [], "Not Authorised"};

send_authed_cdn_query(#state{cdn_url = URL, token = AuthToken}, PathInfo, Headers,  Method, Body, Options) ->	  
	ibrowse:send_req(URL ++ PathInfo, [{"X-Auth-Token", AuthToken} | Headers], Method, Body, Options).

%%
%% Compress incoming data with zlib:gzip if the compressed options was passed
%% the passed data is returned if no compression is required
%%
compress_object(Data, Options) ->
	case get_object_option(compressed, Options) of
		true ->
			zlib:gzip(Data);
		_ ->
			Data
	end.

%%
%% Extract a header from a list of items header items
%% Headers are in the form of [{Header, Value}|HeaderList]
%%
get_header(_Header, []) -> undefined;
get_header(Header, [{Header, Item}|_HeaderList]) -> Item;
get_header(Header, [_|HeaderList]) ->
	get_header(Header, HeaderList).

%%
%% Create a set of metadata headers by prepending "Prefix" to a list of the form
%%  [{string, string}, {string, string}]
%% Returns: [{Prefix++string, string}, {Prefix++string, string}]
%%
create_metadata_headers(Prefix, Headers) when is_list(Headers) ->
	[{Prefix ++ Key, Value} || {Key, Value} <- Headers];
create_metadata_headers(_Prefix, Headers) ->
	Headers.

%%
%% Extract the metadata headers from a set of HTTP headers, the prefix will be
%%  stripped out in the result
%%
extract_metadata_headers(_, []) -> [];
extract_metadata_headers(Prefix, [{Header, Item}|HeaderList]) ->
	case string:str(Header, Prefix) of
			1 -> [{string:substr(Header, string:len(Prefix)+1), Item} | extract_metadata_headers(Prefix, HeaderList)];
			_ -> extract_metadata_headers(Prefix, HeaderList)
	end.

%%
%% Extract the object headers from a passed set of options
%% Returns: a list of HTTP headers to be send with a query
%%
get_object_headers([]) -> [];
get_object_headers([{Option, Value}|Options]) ->
	case Option of
		delete_at -> [{"X-Delete-At", Value} | get_object_headers(Options)];
		delete_after -> [{"X-Delete-After", Value} | get_object_headers(Options)];
		compressed -> [{"Content-Encoding", "gzip"} | get_object_headers(Options)];
		content_type -> [{"Content-Type", Value} | get_object_headers(Options)];
		ac_allow_credentials -> [{"Access-Control-Allow-Credentials", Value} | get_object_headers(Options)];
		ac_allow_methods -> [{"Access-Control-Allow-Methods", Value} | get_object_headers(Options)];
		ac_allow_origin -> [{"Access-Control-Allow-Origin", Value} | get_object_headers(Options)];
		ac_expose_headers -> [{"Access-Control-Expose-Headers", Value} | get_object_headers(Options)];
		ac_max_age -> [{"Access-Control-Max-Age", Value} | get_object_headers(Options)];
		ac_request_headers -> [{"Access-Control-Request-Headers", Value} | get_object_headers(Options)];
		ac_request_method -> [{"Access-Control-Request-Method", Value} | get_object_headers(Options)];
		ac_origin -> [{"Origin", Value} | get_object_headers(Options)];
		_ -> get_object_headers(Options)
	end.

%%
%% Get the value of an option from a OptionsList
%%
get_object_option(Option, OptionsList) ->
	case lists:keyfind(Option, 1, OptionsList) of
		{compressed, true} -> true;
		{chunked, true} -> true;
		_ -> false
	end.

%%
%% Get the options with will be passed to ibrowse
%%
get_object_options([]) -> [];
get_object_options([{Option, _Value}|Options]) ->
	case Option of
		chunked -> [{transfer_encoding, {chunked, 1024}} | get_object_options(Options)];
		_ -> get_object_options(Options)
	end.

%%
%% Validate content with a parsed MD5 hash
%%  if the content is valid, {content, Content} is returned
%%  otherwise {error, hash_does_not_match} is returned
%%
check_return_content(Content, MD5String) ->
	case md5(Content) of
		MD5String -> {ok, Content};
		_ -> {error, hash_does_not_match}
	end.

%%
%% Parse a storage URL and return the version and account as the string
%%  /<Version>/<Account>
%% The URL will be of the form of: https://<host>/<version>/<account>/
%%
parse_path(URL) ->
		[_Protocol, _Host, Version, Account | _Rest] = string:tokens(URL, "/"),
		"/" ++ Version ++ "/" ++ Account ++ "/".

%%
%% MD5 Hex string
%%
md5(X) ->
	string:to_lower(lists:flatten(lists:map(fun hex_char/1, binary_to_list(crypto:md5(X))))).

%%
%% Generate a padded hex character for a integer input
%%
hex_char(X) ->
	lists:flatten(io_lib:format("~2.16.0B", [X])).

%%
%% Get the current time expressed as a UNIX timestamp
%%
epochSeconds(Time) ->
	UnixEpoch = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
	LocalDateTime = calendar:datetime_to_gregorian_seconds(Time),
	LocalDateTime - UnixEpoch.

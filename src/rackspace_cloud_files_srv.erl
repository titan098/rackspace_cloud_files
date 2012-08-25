%%% -------------------------------------------------------------------
%%% Author  : david
%%% Description :
%%%
%%% Created : Aug 21, 2012
%%% -------------------------------------------------------------------
-module(rackspace_cloud_files_srv).
-compile([export_all]).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
				username = undefined,
				apikey = undefined,
				token = undefined,
				tokenage = undefined,
				storage_url = undefined,
				cdn_url = undefined
			   }).

%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%get_auth_token(uk, #state{username = Username, apikey = APIKey} = State) ->
get_auth_token(uk, Username, APIKey) ->
	{ok, Code, Header, Content} = ibrowse:send_req("https://lon.auth.api.rackspacecloud.com/v1.0", [{"X-Auth-User", Username},{"X-Auth-Key", APIKey}], get),
	
	case list_to_integer(Code) of
		Status when (Status >= 200) and (Status < 300) -> {ok, #state{token = get_header("X-Auth-Token", Header),
																	  storage_url = get_header("X-Storage-Url", Header),
																	  cdn_url = get_header("X-CDN-Management-Url", Header)}};
		_ -> {error, {Code,Content}}
	end.

%%
%% Lists all of the containers.
%% Returns [string|string|string|...]
%%
list_containers(State) ->
	{ok, Code, _Header, Content} = send_authed_query(State, get),
	
	case list_to_integer(Code) of
		204 -> [];
		200 -> string:tokens(Content, "\n");
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
		204 -> [{"Object-Count", get_header("X-Container-Object-Count", Header)},
				{"Bytes-Used", get_header("X-Container-Bytes-Used", Header)} | extract_metadata_headers("X-Container-Meta-", Header)];
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
		204 -> [{"Container-Count", get_header("X-Account-Container-Count", Header)},
				{"Bytes-Used", get_header("X-Account-Bytes-Used", Header)}];
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
		204 -> [];
		200 -> string:tokens(Content, "\n");
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
		{content, Content} ->
			{ok, IODevice} = file:open(OutFile, [write, binary]),
			file:write(IODevice, Content),
			file:close(IODevice);
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
	io:format("~p~n", [Headers]),
	{ok, Code, _Header, _Content} = send_authed_query(State, "/" ++ Container ++ "/" ++ Object, Headers, put, NewData, ConnOptions),

	case list_to_integer(Code) of
		201 -> ok;
		401 -> {error, unauthorised};
		411 -> {error, length_required};
		422 -> {error, hash_mismatch};
		_ -> error
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
		200 -> [{"Content-Length", get_header("Content-Length", Header)},
				{"Content-Type", get_header("Content-Type", Header)},
				{"Last-Modified", get_header("Last-Modified", Header)},
				{"Hash", get_header("Etag", Header)} | extract_metadata_headers("X-Object-Meta-", Header)];
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
%% Sends a simple authed query to the server with a passed method
%%
send_authed_query(#state{storage_url = URL, token = AuthToken}, Method) ->
	ibrowse:send_req(URL, [{"X-Auth-Token", AuthToken}], Method).

%%
%% Sends a simple authed query to the server with a passed method specifying
%%  extra URL information
%%
send_authed_query(#state{storage_url = _URL, token = _AuthToken} = State, PathInfo, Method) ->
	send_authed_query(State, PathInfo, [], Method).

%%
%% Sends a simple authed query to the server with a passed method with
%%  extra URL information and extra headers
%%
send_authed_query(#state{storage_url = _URL, token = _AuthToken} = State, PathInfo, Headers,  Method) ->
	send_authed_query(State, PathInfo, Headers, Method, []).

send_authed_query(#state{storage_url = _URL, token = _AuthToken} = State, PathInfo, Headers,  Method, Body) ->
	send_authed_query(State, PathInfo, Headers, Method, Body, []).

send_authed_query(#state{storage_url = URL, token = AuthToken}, PathInfo, Headers,  Method, Body, Options) ->	  
	ibrowse:send_req(URL ++ PathInfo, [{"X-Auth-Token", AuthToken} | Headers], Method, Body, Options).

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
		MD5String -> {content, Content};
		_ -> {error, hash_does_not_match}
	end.

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

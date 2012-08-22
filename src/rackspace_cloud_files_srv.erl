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
	{ok, Code, _Header, Content} = send_authed_query(State, "/" ++ Container, create_metadata_headers("X-Container-Meta-", Metadata), post),
	
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

get_object(State, Container, Object) ->
	{ok, Code, Header, Content} = send_authed_query(State, "/" ++ Container ++ "/" ++ Object, get),

	case list_to_integer(Code) of
		200 -> check_return_content(list_to_binary(Content), get_header("Etag", Header));
		400 -> {error, does_not_exist};
		_ -> error
	end.

get_object(State, Container, Object, OutFile) ->
	case get_object(State, Container, Object) of
		{content, Content} ->
			{ok, IODevice} = file:open(OutFile, [write, binary]),
			file:write(IODevice, Content),
			file:close(IODevice);
		{error, Error} -> {error, Error};
		_ -> error
	end.

create_object(State, Container, Object, Data, Options) ->
	Headers = [{"Etag", md5(Data)} | parse_object_options(Options)],
	io:format("~p~n", [Headers]),
	{ok, Code, Header, Content} = send_authed_query(State, "/" ++ Container ++ "/" ++ Object, Headers, put, Data),

	case list_to_integer(Code) of
		201 -> ok;
		401 -> {error, unauthorised};
		411 -> {error, length_required};
		422 -> {error, hash_mismatch};
		_ -> error
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

send_authed_query(#state{storage_url = URL, token = AuthToken}, PathInfo, Headers,  Method, Body) ->	
	ibrowse:send_req(URL ++ PathInfo, [{"X-Auth-Token", AuthToken} | Headers], Method, Body).

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

parse_object_options([]) -> [];
parse_object_options([{Option, Value}|Options]) ->
	case Option of
		delete_at -> [{"X-Delete-At", Value} | parse_object_options(Options)];
		delete_after -> [{"X-Delete-After", Value} | parse_object_options(Options)];
		_ -> parse_object_options(Options)
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

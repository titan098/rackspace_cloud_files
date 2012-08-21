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

list_containers(State) ->
	{ok, Code, _Header, Content} = send_authed_query(State, get),
	
	case list_to_integer(Code) of
		204 -> [];
		200 -> string:tokens(Content, "\n");
		_ -> {error, Content}
	end.

send_authed_query(#state{storage_url = URL, token = AuthToken}, Method) ->
	ibrowse:send_req(URL, [{"X-Auth-Token", AuthToken}], Method).

%%
%% Extract a header from a list of items header items
%% Headers are in the form of [{Header, Value}|HeaderList]
%%
get_header(_Header, []) -> undefined;
get_header(Header, [{Header, Item}|HeaderList]) -> Item;
get_header(Header, [_|HeaderList]) ->
	get_header(Header, HeaderList).
		
%%% -------------------------------------------------------------------
%%% Author  : David Ellefsen
%%% Description : This module is a simple server to wrap the functionality
%%%  provided in the rackspace_cloud_files_api module - it just executes functions
%%%  and maintains state
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
-module(rackspace_cloud_files_srv).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("rackspace_cloud_files.hrl").

% the maximum age of the auth token, will have to reauthenticate ofter this time
-define(MAXTOKEN, 86400).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------

%% gen_server callbacks
-export([get_auth_token/3, close/0]).
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

%% --------------------------------------------------------------------
%% Auxillary Functions
%% --------------------------------------------------------------------

get_auth_token(Location, Username, APIKey) ->
	gen_server:call(?MODULE, {get_auth_token, Location, Username, APIKey}).

close() ->
	gen_server:call(?MODULE, destroy_state).

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
	ssl:start(),
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
handle_call({get_auth_token, Location, Username, APIKey}, _From, _State) ->
	{Reply, NewState} = case rackspace_cloud_files_api:get_auth_token(Location, Username, APIKey) of
							{ok, SState} -> {ok, SState};
							{error, ErrMessage} -> {error, ErrMessage}
						end,
	{reply, Reply, NewState};

handle_call({execute_function, FunctionName, FunctionParams}, _From, State) ->
	NewState = checkTokenAge(State),
	
	Reply = case erlang:apply(rackspace_cloud_files_api, FunctionName, [NewState | FunctionParams]) of
				ok -> ok;
				{ok, OKMessage} -> OKMessage;
				Other -> Other
			end,
	
	{reply, Reply, NewState};
	
handle_call(destroy_state, _From, _State) ->
	{reply, ok, #state{}};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

checkTokenAge(#state{tokenage = TokenAge} = State) ->
	Age = (TokenAge + ?MAXTOKEN) - epochSeconds(erlang:universaltime()),
	if
		Age > 0 -> State;
		true -> rackspace_cloud_files_api:get_auth_token(State)
	end.
			  
%%
%% Get the current time expressed as a UNIX timestamp
%%
epochSeconds(Time) ->
	UnixEpoch = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
	LocalDateTime = calendar:datetime_to_gregorian_seconds(Time),
	LocalDateTime - UnixEpoch.

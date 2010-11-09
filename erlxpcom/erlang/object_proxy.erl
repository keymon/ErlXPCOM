%% Implementation of the proxy process that represents an XPCOM object
-module(object_proxy).

-include("logger.hrl").
-include("erlxpcom_error.hrl").

-behaviour(xpcom_object).

-record(object_state, {
			oid,			   % OID of this object
			orb,			   % Orb where this object is registered
			request_transport  % Request transport to use to send calls 
			}).

% xpcom_object exports
-export([init/1, terminate/2, xpcom_object_info/3, 
		 query_interface/3, method/4, set_attribute/4, get_attribute/3, info/2]).

% Public exports
-export([create/3]).

%%
%% Create a new xpcom object proxy. 
%%
create(OID, OrbRef, RequestTransport) ->
	xpcom_object:create(?MODULE, [OID, OrbRef, RequestTransport]).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% xpcom_object callbacks |
%~~~~~~~~~~~~~~~~~~~~~~~~'
init([OID, OrbRef, RequestTransport]) ->
	?LOG_MSG(erlxpcom, ?LOGGER_DEBUG, 
			 "Created xpcom object proxy. pid=~p oid=~p", 
				[self(), OID]), 
	{ok, #object_state{oid=OID, orb=OrbRef, request_transport=RequestTransport}}.

xpcom_object_info(iids, _Params, _State) ->
	[].

query_interface(IID, _This, State) ->
	% Call remote queryInterface.  
	{reply, Reply, NewState} = method('QueryInterface', [IID], none, State),
	case Reply of
		{ok, [NewObject|_]} -> 
			% AddRef the incoming object
			xpcom_object:addref(NewObject),
			{{ok, NewObject}, NewState};
		{error, ?NS_ERROR_NO_INTERFACE} -> {no_interface, NewState};			
		Other -> {Other, NewState}
	end.

terminate(_Reason, State) ->
	% Deregister this object in ORB and mozilla side
	?LOG_MSG(erlxpcom, ?LOGGER_DEBUG, 
			 "Stoping xpcom object proxy. pid=~p oid=~p", 
				[self(), State#object_state.oid]), 
	erlxpcom_orb:unregister_remote_object(
		State#object_state.orb, 
		xpcom_object:pid_to_xpcom_object(self())).
							  
		
method(MethodName, Params, _, State) ->
	% Simply delegate on erlxpcom_call
	?LOG_MSG(erlxpcom, ?LOGGER_DEBUG, 
			 "Remote call to method ~s", [MethodName]), 
	Reply =
		erlxpcom_call:call_remote(State#object_state.request_transport, 
								  State#object_state.orb, 
								  State#object_state.oid, 
								  MethodName, Params,
								  call_method),
	{reply, Reply, State}.

get_attribute(Attribute, _, State) ->
	% Simply delegate on erlxpcom_call
	?LOG_MSG(erlxpcom, ?LOGGER_DEBUG, 
			 "Getting attribute ~s", [Attribute]), 
	Reply =
		erlxpcom_call:call_remote(State#object_state.request_transport, 
								  State#object_state.orb, 
								  State#object_state.oid, 
								  Attribute, [], 
								  get_attribute),
	{reply, Reply, State}.

set_attribute(Attribute, Value, _, State) ->
	% Simply delegate on erlxpcom_call
	?LOG_MSG(erlxpcom, ?LOGGER_DEBUG, 
			 "Setting attribute ~s", [Attribute]), 
	Reply =
		erlxpcom_call:call_remote(State#object_state.request_transport, 
								  State#object_state.orb, 
								  State#object_state.oid, 
								  Attribute, [Value], 
								  set_attribute),
	{reply, Reply, State}.
								
info(_Msg, State) -> {no_reply, State}.



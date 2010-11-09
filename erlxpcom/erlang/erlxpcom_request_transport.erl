%%
%% This module implements the server that mantain the comunication with
%% the mozilla XPCOM side. 
%%
-module(erlxpcom_request_transport).

-include("erlxpcom.hrl").

%% Maximum value for a callId
-define(MAX_CALL_ID, 1000000000).

-behaviour(gen_server).

%% State for the request transport
-record(rt_state, {
			orb, 
			callid_counter = 0, 
			reply_wait = dict:new(),
			mozilla_pid
		}).

% Public exports 
-export([start/1, call_remote/5, drop_object/2]).
% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, 
		 handle_info/2, terminate/2, code_change/3]).
% other exports
-export([dispatch_call/7]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public functions 

%% Start a new request transport, using the given pid to send remote calls
%%
start(MozillaPid) ->
	gen_server:start(?MODULE, [MozillaPid], []).

%% @spec call_remote(RequestTransport, OID, MethodName, Params, Type) ->
%%				{ok, ReplyList} | {error, Reason}
%%		Type = call_method | get_attribute | set_attribute
%%
%% Send call to an remote object in mozilla side. It will lock the calling
%% process until a response arrives.
call_remote(RequestTransport, OID, MethodName, Params, Type) when
	(Type == call_method) or (Type == get_attribute) or (Type == set_attribute) ->
	gen_server:call(RequestTransport, 
				    {call_remote, OID, MethodName, Params, Type}, infinity).


%% Drop an object in the mozilla side. This method will be called when a
%% XPCOM proxy is destroyed.
drop_object(RequestTransport, OID) ->
	gen_server:cast(RequestTransport,{drop_object, OID}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions 

%% Get the next callId increasing the counter in state
%% @spec new_callid(State) -> {NewState, CallId}
new_callid(State) ->
	CallId = State#rt_state.callid_counter,
	{State#rt_state{callid_counter = (CallId + 1) rem ?MAX_CALL_ID}, CallId}.

%%
%% Add an element to the list of pids waiting a reply with an callId associated
%%
add_reply_wait(State, FromPid) ->
	{NewState1, CallId} = new_callid(State),
	NewState2 = NewState1#rt_state{reply_wait =
						dict:append(CallId, FromPid, 
							NewState1#rt_state.reply_wait)},
	{NewState2, CallId}.

do_return_reply(RequestTransport, CallId, Reply) ->
	gen_server:cast(RequestTransport, {reply_remote, CallId, Reply}).

%
% Really execute a call. This method will be executed by a different 
% process and will execute the call an wait for a reply, sending it throw 
% the request transport. 
dispatch_call(Orb, RequestTransport, ObjectId, CallId, MethodName, Params, Type) ->
	case Reply = erlxpcom_call:call_local(Orb, ObjectId, MethodName, Params, Type) of
		{ok, _} -> 
			do_return_reply(RequestTransport, CallId, Reply);
		{error, ErrorCode} when is_integer(ErrorCode) -> 
			do_return_reply(RequestTransport, CallId, Reply)
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_server callbacks
init([MozillaPid]) -> 
	{ok, #rt_state{mozilla_pid = MozillaPid}}.

%% Set the orb
handle_call({setorb, Orb}, _, State) ->
	{reply, ok, State#rt_state{orb = Orb}};

%% Send a method_call to mozilla side
%% @spec send_call(State, From, OID, MethodName, Params) -> NewState1
%%		State = term()
%%		From = {pid(), ref()}
handle_call({call_remote, OID, MethodName, Params, Type}, From, State) ->
	{NewState, CallId} = add_reply_wait(State, From),
	_MsgSent = NewState#rt_state.mozilla_pid!
					{Type, OID, CallId, MethodName, Params}, 
	?LOG_MSG(erlxpcom_transport, ?LOGGER_DEBUG, 
		" ==> ~p", [_MsgSent]), 
	{noreply, NewState};

	
% default call handler
handle_call(Msg, _From, State) ->
	?LOG_MSG(erlxpcom, ?LOGGER_DEBUG, 
		"Unknown call message received: ~p", [Msg]),
	{reply, {error, {error, unknown_message}}, State}. 


%% 
%% Reply a call from mozilla side
%%
handle_cast({reply_remote, CallId, Reply}, State) ->
	_MsgSent = State#rt_state.mozilla_pid!{reply, CallId, Reply}, 			
	?LOG_MSG(erlxpcom_transport, ?LOGGER_DEBUG, 
		" ==> ~p", [_MsgSent]), 
	{noreply, State};

%---
% Drop an object
handle_cast({drop_object, OID}, State) ->
	_MsgSent = State#rt_state.mozilla_pid!{drop_object, OID}, 			
	?LOG_MSG(erlxpcom_transport, ?LOGGER_DEBUG, 
		" ==> ~p", [_MsgSent]), 
	{noreply, State};

% default cast handler
handle_cast(Msg, State) ->
	?LOG_MSG(erlxpcom, ?LOGGER_DEBUG, 
			"Unknown cast message received: ~p", [Msg]),
	{noreply, State}. 


%---
% XPCOM to Erlang call request
handle_info(Msg = {Type, OID, CallId, MethodName, Params}, State) when
	(Type == call_method) or (Type == get_attribute) or (Type == set_attribute) ->
	?LOG_MSG(erlxpcom_transport, ?LOGGER_DEBUG, 
			" <== ~p", [Msg]),
	% Get object id 
	case erlxpcom_orb:get_local_object_by_oid(State#rt_state.orb, OID) of 
		{ok, ObjectId} ->
			% spawn a new process that will execute the call and return the value
			% Spawning a process does not block the request transport.
			spawn(?MODULE, dispatch_call,  
					[State#rt_state.orb, self(), ObjectId, 
					 CallId, MethodName, Params, Type]);
		not_found ->
			?LOG_MSG(erlxpcom, ?LOGGER_ERROR, 
					"Request Transport: Call to an unknown object (OID=~p)", [OID]),
			handle_cast({reply_remote, CallId, {error, ?NS_ERROR_NULL_POINTER}}, State)
	end,
	{noreply, State};
	

%---
% XPCOM to Erlang call request
%%
%% Dispatch an reply from mozilla side. It will get the waiting pid in wait 
%% queue and send it the reply
%%
handle_info(Msg = {reply, CallId, Reply}, State) ->
	?LOG_MSG(erlxpcom_transport, ?LOGGER_DEBUG, 
			" <== ~p", [Msg]),
	case dict:find(CallId, State#rt_state.reply_wait) of 
		{ok, [Client|_]} ->
			NewState = State#rt_state{reply_wait = 
				dict:erase(CallId, State#rt_state.reply_wait)},
			gen_server:reply(Client, Reply);
		Error -> 
			?LOG_MSG(erlxpcom, ?LOGGER_ERROR, 
				"Request Transport: Unknown callId CallId=~p (Error=~p)", 
				[CallId, Error]),
			NewState = State
	end,
	{noreply, NewState};


%%
%% Drop an object. The mozilla side is not interesed in a local object
%%
handle_info(Msg = {drop_object, OID}, State) ->
	?LOG_MSG(erlxpcom_transport, ?LOGGER_DEBUG, 
			" <== ~p", [Msg]),
	case erlxpcom_orb:get_local_object_by_oid(State#rt_state.orb, OID) of 
		{ok, ObjectId} ->
			xpcom_object:release(ObjectId);
		not_found ->
			?LOG_MSG(erlxpcom, ?LOGGER_ERROR, 
					"Request Transport: Try to drop an unknown object (OID=~p)", [OID])
	end,
	{noreply, State};


% Ignore the rest
handle_info(_, State) -> {noreply, State}.

terminate(Reason, _) -> Reason.

code_change(_, State, _) -> {ok, State}.

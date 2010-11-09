%% Implementation of the proxy process that represents an XPCOM object
-module(erlxpcom_xpcom_object).

-behaviour(gen_server).

-record(object_state, {
			oid,			   % OID of this object
			orb,			   % Orb where this object is registered
			request_transport, % Request transport to use to send calls 
			ref_counter = 0    % Reference counter of this object
			}).

% genserver exports
-export([init/1, handle_call/3, handle_cast/2, 
		 handle_info/2, terminate/2, code_change/3]).

% Public exports
-export([create/3, addref/1, release/1, call/3]).

%%
%% Create a new xpcom object proxy. 
%%
create(OID, OrbRef, RequestTransport) ->
	case gen_server:start(?MODULE, [OID, OrbRef, RequestTransport], []) of
		{ok, Pid} -> {xpcom_object, Pid};
		Other -> 
			error_logger:error_msg(
				"erlxpcom_object: Error creating object (OID=~w)", [OID]),
			Other
	end.

addref({xpcom_object, ObjectPid}) ->
	gen_server:call(ObjectPid, addref).

release({xpcom_object, ObjectPid}) ->
	gen_server:call(ObjectPid, release).

%%
%% call to a method
%%			
call({xpcom_object, ObjectPid}, MethodName, Params) ->
	gen_server:call(ObjectPid, {call_method, MethodName, Params}).

%% 
%% This function checks if the object reference has a nil object value, 
%% which denotes no object. It is the reference that is tested and no object 
%% implementation is involved in the test. 
%% 
%% @spec is_nil(Object) -> boolean()
%% 
is_nil({nil}) -> true;
is_nil(_) -> false.

is_remote({xpcom_object, _}) -> true;
is_remote(_) -> false.

is_local({local_object, _}) -> true;
is_local(_) -> false.

      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private functions 

% Unmarshall a reply. It really simply translates object references and 
% registers new objects in orb
unmarshall_reply(Orb, [{xpcom_object, OID}|T]) -> 
	case erlxpcom_orb:register_xpcom_object(Orb, OID) of
		{ok, ObjectId} -> ObjectId;
		{error, {duplicated_xpcom_object, OID, ObjectId}} -> ObjectId
	end, 
	[ObjectId|unmarshall_reply(Orb, T)];
unmarshall_reply(Orb, [H|T]) -> [H|unmarshall_reply(Orb, T)];
unmarshall_reply(Orb, []) -> [].
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_server callbacks

init([OID, OrbRef, RequestTransport]) ->
	{ok, #object_state{oid=OID, orb=OrbRef, request_transport=RequestTransport}}.

handle_call(addref, From, State) ->
	ActualCounter = State#object_state.ref_counter + 1,
	NewState = State#object_state{ref_counter = ActualCounter}, 
	{reply, ActualCounter, NewState};

handle_call(release, From, State) ->
	ActualCounter = State#object_state.ref_counter - 1,
	NewState = State#object_state{ref_counter = ActualCounter}, 
	% Destroy the object if necesary counter raises 0...
	if 
		ActualCounter == 0 ->
			erlxpcom_orb:unregister_xpcom_object(State#object_state.orb, 
				State#object_state.oid),
			{stop,normal,ActualCounter,NewState};
		true -> 
			{reply, ActualCounter, NewState}
	end;
	
handle_call({call_method, MethodName, Params}, From, State) ->
	case MethodName of 
		"addRef" -> 
			handle_call(addRef, From, State);
		"release" -> 
			handle_call(release, From, State);
		_ -> 
			CallResponse = 
				erlxpcom_request_transport:call_remote(
					State#object_state.request_transport,
					State#object_state.oid, 
					MethodName, Params),
			case CallResponse of 
				{ok, Reply} -> 
					NewReply = unmarshall_reply(State#object_state.orb, Reply),
					{reply, {ok, NewReply}, State};
				_ ->
					{reply, CallResponse, State}
			end
	end;

% default call handler
handle_call(Msg, From, State) ->
	error_logger:error_msg(
		"erlxpcom_object: Unknown call message received: ~w", [Msg]),
	{reply, {error, {error, unknown_message}}, State}. 

% default cast handler
handle_cast(Msg, State) ->
	error_logger:error_msg(
		"erlxpcom_object: Unknown cast message received: ~w", [Msg]),
	{noreply, State}. 
		
terminate(Reason, _) -> Reason.

handle_info(_, State) -> {noreply, State}.

code_change(_, State, _) -> {ok, State}.


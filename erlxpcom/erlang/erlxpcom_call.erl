%% Perform and manage call to remote/local objects.
%% It will marshall/unmarshall parameters in the call.
-module(erlxpcom_call).

-include("erlxpcom.hrl").

-export([call_remote/6, call_local/5]).

%% @spec call_remote(RequestTransport, Orb, OID, MethodName, Params, Type) ->
%%				{ok, ReplyList} | {error, Reason}
%%		Type = call_method | get_attribute | set_attribute
%%
%% Send call to a remote object in mozilla side. This function will lock
%%	the calling process until a reply arrives.
call_remote(RequestTransport, Orb, OID, MethodName, Params, Type) ->
	case (Return = (catch 
			do_call_remote(RequestTransport, Orb, OID, MethodName, Params, Type))) of
		{ok, _} -> Return;
		{error, _} -> Return;
		Other -> {error, Other}
	end.
	
%% @spec call_local(Orb, ObjectId, MethodName, Params, type) ->
%%				{ok, ReplyList} | {error, Reason}
%%		Type = call_method | get_attribute | set_attribute
%%	    Reason = {no_object, ObjectId} and others
%% Send call to a local object. This function will lock
%%	the calling process until a reply arrives.
call_local(Orb, ObjectId, MethodName, Params, Type) ->
	case (Return = (catch 
			do_call_local(Orb, ObjectId, MethodName, Params, Type))) of
		{ok, _} -> Return;
		{error, _} -> Return;
		Other -> {error, Other}
	end.
	
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Private API |
%~~~~~~~~~~~~~'

% Unmarshall a list of parameters (In or Out). This can be the reply of a 
% remote call or the parameters of a incoming call. 
% It really simply translates the object references registering 
% new objects in orb if necesary. 
unmarshall_params(Orb, [{xpcom_oid, OID}|T]) -> 
	case erlxpcom_orb:register_remote_object(Orb, OID) of
		{ok, ObjectId} -> ObjectId;
		{error, {duplicated_object, OID, ObjectId}} -> ObjectId
	end, 
	[ObjectId|unmarshall_params(Orb, T)];
unmarshall_params(Orb, [{erlang_oid, OID}|T]) -> 
	ObjectId = 
		case erlxpcom_orb:get_local_object_by_oid(Orb, OID) of
			{ok, ObjectId2} -> ObjectId2;
			_ -> 
				?LOG_MSG(erlxpcom, ?LOGGER_ERROR, 
						 "Reference to unknown local OID ~w", [OID]), 
				throw({no_object, OID})
		end, 
	[ObjectId|unmarshall_params(Orb, T)];
unmarshall_params(Orb, [L|T]) when is_list(L) -> 
	[unmarshall_params(Orb, L)|unmarshall_params(Orb, T)];
unmarshall_params(Orb, [H|T]) -> [H|unmarshall_params(Orb, T)];
unmarshall_params(_, []) -> [].

% Marshall a list of parameters (In or Out). This can be the reply of a remote call or 
% the parameters of a incoming call. 
% It really simply translates the object references registering 
% new objects in orb if necesary. 
marshall_params(Orb, [ObjectRef = {xpcom_object, _}|T]) -> 
	% Check if it is a remote object
	ObjectId = 
	case erlxpcom_orb:get_remote_object_by_ref(Orb, ObjectRef) of
		{ok, OID} -> 
			{xpcom_oid, OID};
		_ -> 
			% Else check if is a local object
			case erlxpcom_orb:get_local_object_by_ref(Orb, ObjectRef) of
				{ok, OID} -> 
					{erlang_oid, OID};
				_ -> 
					% if not, register it
					case erlxpcom_orb:register_local_object(Orb, ObjectRef) of
						{ok, NewOID} -> {erlang_oid, NewOID};
						Other -> throw(Other)
					end
			end
	end, 
	[ObjectId|marshall_params(Orb, T)];
marshall_params(Orb, [L|T]) when is_list(L) -> 
	[marshall_params(Orb, L)|marshall_params(Orb, T)];
marshall_params(Orb, [H|T]) -> [H|marshall_params(Orb, T)];
marshall_params(_, []) -> [].

%% Do call to a remote object
do_call_remote(RequestTransport, Orb, OID, MethodName, Params, Type) ->
	% ...marshall parameters...
	?LOG_MSG(erlxpcom, ?LOGGER_DEBUG, 
		"Marshalling params ~p", [Params]), 
	NewParams = marshall_params(Orb, Params),
	% ...send call....
	CallResponse = 
		erlxpcom_request_transport:call_remote(RequestTransport, OID, 
											   MethodName, NewParams, Type),
	% ...and unmarshall reply
	case CallResponse of 
		{ok, Reply} -> 			
			?LOG_MSG(erlxpcom, ?LOGGER_DEBUG, 
				"Unmarshalling reply ~p", [Reply]), 
			case unmarshall_params(Orb, Reply) of 
				NewReply when Type == call_method ->
					{ok, NewReply};
				[Value]  when Type == get_attribute ->
					{ok, Value};
				[] when Type == set_attribute ->
					ok
			end;
		_ ->
			CallResponse
	end.

%% Do call to a local object
do_call_local(Orb, ObjectId, MethodName, Params, Type) ->
	% ...unmarshall parameters...
	?LOG_MSG(erlxpcom, ?LOGGER_DEBUG, 
		"Unmarshalling params ~p", [Params]), 
	NewParams = unmarshall_params(Orb, Params),
	% ...perform call....
	CallResponse = 
	case Type of 
		call_method ->
			(catch xpcom_object:call_method(ObjectId, MethodName, NewParams));
		get_attribute ->
			case (catch xpcom_object:get_attribute(
						ObjectId, MethodName)) of
				{ok, Value} -> {ok, [Value]};
				Other -> Other
			end; 
		set_attribute ->
			case (catch xpcom_object:set_attribute(
						ObjectId, MethodName, hd(NewParams))) of
				ok -> {ok, []};
				Other -> Other
			end
	end,
	% ...and unmarshall reply
	case CallResponse of 
		{ok, Reply} -> 			
			?LOG_MSG(erlxpcom, ?LOGGER_DEBUG, 
				"Marshalling reply ~p", [Reply]), 
			NewReply = marshall_params(Orb, Reply),
			% WARNING! HACK!!!
			% If the call is to QueryInterface that increases the reference 
			% counter, decrease it. The reference counter from Mozilla world 
			% must be 1 ever (only the reference from orb)
			% BUT this must be done after the marshalling, so the orb can
			% addref it X)
			if	MethodName == 'QueryInterface' -> 
					xpcom_object:release(ObjectId);
				true -> true
			end,
			{ok, NewReply};
		{error, Code} when is_integer(Code) ->
			CallResponse;
		{error, _Reason} ->
			?LOG_MSG(erlxpcom, ?LOGGER_ERROR, 
				"xpcom_object:~s(~s) returned error~n    Reason=~p", 
				[Type, MethodName, _Reason]), 
			{error, ?NS_ERROR_FAILURE};
		_Other ->
			?LOG_MSG(erlxpcom, ?LOGGER_ERROR, 
				"xpcom_object:~s(~s) failed~n    Reason=~p", 
				[Type, MethodName, _Other]), 
			{error, ?NS_ERROR_ABORT}
	end.


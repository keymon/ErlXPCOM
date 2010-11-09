%%
%% @author Héctor Rivas Gándara <keymon@gmail.com>
%% @copyright 2005 Héctor Rivas Gándara 

%% Main interface module for erlxpcom
-module(erlxpcom).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Public API |
%~~~~~~~~~~~~'

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Basic object methods:
%  - QueryInterface
%  - addRef
%  - release
%

%%
%% @spec query_interface(ObjectId, IID) -> Response.
%%		Response = {ok, NewObjectId} | {fail, ErrorCode}
%%		ErrorCode = integer()
%%
%% Get the object reference that implements the given IID for the given object.
%%
query_interface(ObjectId, IID) -> {ok, ObjectId}.

addRef(ObjectId).
release(ObjectId).

call_method(ObjectId, MethodName, Params)

get_property(ObjectId, PropertyName).
set_property(ObjectId, PropertyName).


% Process an XPCOM nsIEnumeration
process_enumeration(NsEnumeration, Function) ->
	case xpcom_object:call_method(NsEnumeration, hasMoreElements, []) of
		{ok, [true]} ->
			{ok, [Object]} = xpcom_object:call_method(NsEnumeration, getNext, []),
			Function(Object),
			process_enumeration(NsEnumeration, Function);
		_ -> 
			true
	end.

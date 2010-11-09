%%
%% nsISimpleEnumerator for lists 
%%
%% 
-module(lfListSimpleEnumerator).

-include("nsISimpleEnumerator.hrl").

% xpcom_object exports
-export([init/1, terminate/2, xpcom_object_info/3, query_interface/3,
		method/4, set_attribute/4, get_attribute/3, info/2]).

% public exports
-export([create/1]).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Public API |
%~~~~~~~~~~~~'
create(List) -> 
	xpcom_object:create(?MODULE, List).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% xpcom_object callbacks |
%~~~~~~~~~~~~~~~~~~~~~~~~'
init(TaskList) ->
	{ok, TaskList}.

xpcom_object_info(iids, _Params, _State) ->
	[?NSISIMPLEENUMERATOR_IID].   
	
query_interface(_IID, _This, State) ->
	{no_interface, State}.

terminate(_Reason, _State) -> 
	ok.
		
%~~~~~~~~~~
method(getNext, [], _, [Item|T]) ->
	{reply, {ok, [Item]}, T};

method(hasMoreElements, [], _, State = [_|_]) ->
	{reply, {ok, [true]}, State};

method(hasMoreElements, [], _, State = []) ->
	{reply, {ok, [false]}, State};

method(Method, _, _, State) ->
	{reply, {error, {unknown_method, Method}}, State}.

get_attribute(Attribute,  _, State) ->
	{reply, {error, {unknown_method, Attribute}}, State}.

set_attribute(Attribute, _Value, _, State) ->
	{reply, {error, {unknown_method, Attribute}}, State}.

info(_Msg, State) -> {no_reply, State}.

								





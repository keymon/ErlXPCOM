%%
%% Implementation of a generic factory, ala nsIGenericFactory:
%% See:
%% http://lxr.mozilla.org/seamonkey/source/xpcom/glue/nsGenericFactory.h
%% http://lxr.mozilla.org/seamonkey/source/xpcom/glue/nsIGenericFactory.h
%% http://lxr.mozilla.org/seamonkey/source/xpcom/glue/nsGenericFactory.cpp
%% an others
%%
-module(ns_generic_factory).

-behaviour(xpcom_object).

-include("erlxpcom.hrl").

% xpcom_object exports
-export([init/1, terminate/2, xpcom_object_info/3, query_interface/3,
		method/4, set_attribute/4, get_attribute/3, info/2]).

% Public exports
-export([create/1]).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Public API |
%~~~~~~~~~~~~'
create(ComponentInfo) ->
	xpcom_object:create(?MODULE, ComponentInfo).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% xpcom_object callbacks |
%~~~~~~~~~~~~~~~~~~~~~~~~'
%~~~	
init(ComponentInfo) -> {ok, ComponentInfo}.

%~~~	
xpcom_object_info(iids, _Params, _State) -> [?NSIFACTORY_IID].

%~~~	
query_interface(_IID, _This, State) -> {no_interface, State}.

%~~~	
terminate(_Reason, _State) -> ok. % que se lle vai facer!? ;-)

%~~~
method(lockFactory, [_Lock], _, State) -> {reply, {ok, []}, State};

method(createInstance, [Outer, IID], _, ComponentInfo) -> 
	Constructor = ComponentInfo#ns_module_component_info.constructor,
	{reply, Reply2, NewState} =
	if is_function(Constructor) ->
		case Reply = (catch Constructor(Outer, IID)) of 
			{ok, Object} -> 
				{reply, {ok, [Object]}, ComponentInfo};
			{error, _Reason} ->
				{reply, Reply, ComponentInfo};
			Other ->
				?LOG_MSG(erlxpcom, ?LOGGER_ERROR, 
					"Error in constructor call: ~p", [Other]), 
				{reply, {error, Other}, ComponentInfo}
		end;
	true ->	
		{reply, {error, ?NS_ERROR_FACTORY_NOT_REGISTERED}, ComponentInfo}
	end,
	?LOG_MSG(erlxpcom, ?LOGGER_ERROR, 
		"ns_factory:createInstance(~s) -> ~p", [IID, Reply2]), 
	{reply, Reply2, NewState}.
	
set_attribute(Attribute, _Value, _, State) ->
	{reply, {error, {unknown_method, Attribute}}, State}.

get_attribute(Attribute,  _, State) ->
	{reply, {error, {unknown_method, Attribute}}, State}.

info(_Msg, State) -> {no_reply, State}.


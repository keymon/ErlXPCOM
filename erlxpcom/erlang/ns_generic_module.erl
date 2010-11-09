%%
%% Implementation of a generic module, ala nsIGenericModule:
%% See:
%% http://lxr.mozilla.org/seamonkey/source/xpcom/glue/nsGenericFactory.h
%% http://lxr.mozilla.org/seamonkey/source/xpcom/glue/nsIGenericFactory.h
%% http://lxr.mozilla.org/seamonkey/source/xpcom/glue/nsGenericFactory.cpp
%% an others
%%
-module(ns_generic_module).

-behaviour(xpcom_object).

-include("erlxpcom.hrl").

% xpcom_object exports
-export([init/1, terminate/2, xpcom_object_info/3, query_interface/3, 
		 method/4, set_attribute/4, get_attribute/3, info/2]).

% Public exports
-export([create/1, create_and_query/1]).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% module state
-record(?MODULE, {
			module_info,    % info of the module
			factories = []      % list with instantiated factories
		}).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Public API |
%~~~~~~~~~~~~'
create(ModuleInfo) ->
	xpcom_object:create(?MODULE, ModuleInfo).

%%
%% will create a new module and queryinterface it with nsIModule
create_and_query(ModuleInfo) ->
	case Reply1 = create(ModuleInfo) of 
		{ok, Module} -> 
			case Reply2 = xpcom_object:query_interface(Module, ?NSIMODULE_IID) of
				{ok, _} -> Reply2;
				_ -> 
					% release the created module
					xpcom_object:release(Module),
					Reply2
			end;
		_ -> Reply1
	end.
	
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% xpcom_object callbacks |
%~~~~~~~~~~~~~~~~~~~~~~~~'
%~~~	
init(ModuleInfo) ->
	Constructor = ModuleInfo#ns_module_info.module_constructor,
	if  
		% if there is an constructor, call it
		is_function(Constructor) ->
			Constructor(ModuleInfo);
		true ->
			true
	end, 
	{ok, #?MODULE{module_info = ModuleInfo}}.

%~~~	
xpcom_object_info(iids, _Params, _State) -> 
	[?NSIMODULE_IID].

%~~~	
query_interface(_IID, _This, State) -> {no_interface, State}.

%~~~	
terminate(_Reason, State) ->
	ModuleInfo = State#?MODULE.module_info,
	Destructor = ModuleInfo#ns_module_info.module_destructor,
	if  
		% if there is an desconstructor, call it
		is_function(Destructor) ->
			% Call destructor
			Destructor(ModuleInfo);
		true -> true
	end, 
	% release all factories
	lists:map(fun({_, Factory}) -> xpcom_object:release(Factory) end,
			 State#?MODULE.factories),
	ok.

%~~~
%% Check if this module can be unloaded. By default not.
method(canUnload, [_ComponentManager], _, State) -> 
	{reply, {ok, [false]}, State};

%% Get the class object implemented by CID and with interface IID
method(getClassObject, [_ComponentManager, CID, IID], _, State) -> 
	% Search the factory in the instantiated list
	{reply, Reply2, NewState} = 
	case lists:keysearch(CID, 1, State#?MODULE.factories) of
		{value, {CID, Factory}} ->
			% QueryInterface it. FIXME: What happens with reference counter??
			Reply = xpcom_object:query_interface(Factory, IID), 
			{reply, Reply, State};
		_ -> 
			% Get the component info
			ComponentInfoList = 
				(State#?MODULE.module_info)#ns_module_info.components,
			case lists:keysearch(CID, 3, ComponentInfoList) of 
				{value, ComponentInfo} ->
					% Instantiate a generic factory
					{ok, Factory} = ns_generic_factory:create(ComponentInfo),
					% Add it to factory list 
					NewFactories = [{CID, Factory}|State#?MODULE.factories],
					xpcom_object:addref(Factory),
					% Do QueryInterface to the given IID 
					% FIXME: What happens with reference counter??
					case xpcom_object:query_interface(Factory, IID) of
						{ok, Obj} ->     
							{reply, {ok, [Obj]}, State#?MODULE{factories = NewFactories}};
						_Other ->
							xpcom_object:release(Factory),
							{reply, 
								{error, ?NS_ERROR_FACTORY_NOT_REGISTERED}, State}
					end;
				_ -> 
					% return error
					{reply, {error, ?NS_ERROR_FACTORY_NOT_REGISTERED}, State}
			end
	end,
	?LOG_MSG(erlxpcom, ?LOGGER_INFO, 
			"ns_generic_module:getClassObject(~s) -> ~p", 
			[CID, Reply2]), 
	{reply, Reply2, NewState};

method(registerSelf, [ComponentManager, LocationPath, LoaderStr, Type], _, State) -> 
	%% Log ala mozilla :)
	?LOG_MSG(erlxpcom, ?LOGGER_DEBUG, 
		"*** Registering ~s components (all right -- an erlang generic module!)\n", 
		[(State#?MODULE.module_info)#ns_module_info.moduleName]),
	%% Get the registrar from ComponentManager
	case xpcom_object:query_interface(ComponentManager, 
									  ?NSICOMPONENTREGISTRAR_IID) of 
		{ok, ComponentRegistrar} ->
			% Register each component of list of components
			RegisterFunction = fun(ComponentInfo) ->
				case xpcom_object:call_method(ComponentRegistrar,
						registerFactoryLocation,
						[ComponentInfo#ns_module_component_info.cid, 
						 ComponentInfo#ns_module_component_info.description,
						 ComponentInfo#ns_module_component_info.contractID,
						 LocationPath, LoaderStr,Type]) of
					{error, Reason} ->
						?LOG_MSG(erlxpcom, ?LOGGER_ERROR, 
							"Can't register component ~p\n  Reason ~p", 
							[ComponentInfo#ns_module_component_info.description,
							 Reason]),
						false;
					_ ->
						% Execute the registration proc if necesary
						RegisterSelfProc = 
							ComponentInfo#ns_module_component_info.registerSelfProc,
						if	is_function(RegisterSelfProc) -> 
							RegisterSelfProc(ComponentManager, LocationPath, 
											 LoaderStr, Type, ComponentInfo);
						true ->	true
						end
				end %case
			end, %function
			lists:foreach(RegisterFunction, 
				(State#?MODULE.module_info)#ns_module_info.components),
			% release the registrar
			xpcom_object:release(ComponentRegistrar),
			% return ok
			{reply, {ok, []}, State};
		
		Error = {error, Reason} -> 
			?LOG_MSG(erlxpcom, ?LOGGER_ERROR, 
					 "Can't get component registrar\n  Reason=~p", [Reason]),
			{reply, Error, State}
	end;

method(unregisterSelf, [ComponentManager, LocationPath, LoaderStr], _, State) -> 
	?LOG_MSG(erlxpcom, ?LOGGER_DEBUG, 
		"*** Unregistering ~s components (all right -- an erlang generic module!)\n", 
		[(State#?MODULE.module_info)#ns_module_info.moduleName]),
	%% Get the registrar from ComponentManager
	case xpcom_object:query_interface(ComponentManager, 
									  ?NSICOMPONENTREGISTRAR_IID) of 
		{ok, ComponentRegistrar} ->
			% Register each component of list of components
			UnRegisterFunction = 
			fun(ComponentInfo) ->
				case xpcom_object:call_method(ComponentRegistrar,
						unregisterFactoryLocation, 
						[ComponentInfo#ns_module_component_info.cid, 
						LocationPath]) of
					{error, Reason} ->
						?LOG_MSG(erlxpcom, ?LOGGER_ERROR, 
							"Can't unregister component ~p\n  Reason ~p", 
							[ComponentInfo#ns_module_component_info.description,
							 Reason]),
						false;
					_ -> 
						% Execute the unregistration proc if necesary
						UnRegisterSelfProc = 
							ComponentInfo#ns_module_component_info.unregisterSelfProc,
						if is_function(UnRegisterSelfProc) -> 
							UnRegisterSelfProc(ComponentManager, LocationPath, 
											   LoaderStr, ComponentInfo);
						true ->	true
						end
				end %case
			end, %function
			lists:foreach(UnRegisterFunction, 
				(State#?MODULE.module_info)#ns_module_info.components),
			% return ok
			{reply, {ok, []}, State};
		Error = {error, Reason} -> 
			?LOG_MSG(erlxpcom, ?LOGGER_ERROR, 
					 "Can't get component registrar\n  Reason=~p", [Reason]),
			{reply, Error, State}
	end.

set_attribute(Attribute, _Value, _, State) ->
	{reply, {error, {unknown_method, Attribute}}, State}.

get_attribute(Attribute,  _, State) ->
	{reply, {error, {unknown_method, Attribute}}, State}.
	
info(_Msg, State) -> {no_reply, State}.

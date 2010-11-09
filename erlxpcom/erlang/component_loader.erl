%%
%% Component loader for erlXPCOM
%%
%% This component will be registered in the CategoryManager as loader
%% of erlang components. 
%% It will be registered locally in orb with OID = 0 
%% 
%% The component loader is implemented using the generic_factory and 
%% generic_module schema
%%
%% CID: 9fab2034-002c-4e8f-bf2c-f81e77072e42
%% contractId: @lfcia.org/erlang/componentloader;1
%%
%% TODO: Do not register already registered modules. Use a md5 or something.
%%
-module(component_loader).

-include("erlxpcom.hrl").

-behaviour(xpcom_object).

-compile(export_all).

% xpcom_object exports
-export([init/1, terminate/2, xpcom_object_info/3, query_interface/3,
		method/4, set_attribute/4, get_attribute/3, info/2]).

% XPCOM Component exports
-export([ns_get_module/0]).

% Public exports
-export([create/0]).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-define(ERLANG_COMPONENT_LOADER_CID, "9fab2034-002c-4e8f-bf2c-f81e77072e42").
-define(ERLANG_COMPONENT_LOADER_CONTRACTID, "@lfcia.org/erlang/componentloader;1").

% FIXME put it somewhere
-define(ERLANG_COMPONENT_MIME_TYPE, "application/erlang-beam").
-define(ERLANG_COMPONENT_SUFFIX, ".erlang").

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-record(?MODULE, {
		component_manager, 
		component_registry,
		initialized = false
		}).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% FIXME: put this somewhere. Create an xpidl compiler!!
% When is AutoRegistration occuring?
-define(Startup, 0).
-define(Component, 1).
-define(Timer, 2).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Generic module  |
%~~~~~~~~~~~~~~~~~'
nsmodule_info() ->
	#ns_module_info{
		version = "0.0.1",
		moduleName = "ErlXPCOM: The Erlang XPCOM binding", 
		% If this module is destroyed, exit the erlang runtime
		module_destructor = 
			fun(_) -> 
				?LOG_MSG(erlxpcom, ?LOGGER_INFO, 
					"Destroing component-loader module", []),
				halt() 
			end, 
		components = [
			#ns_module_component_info{
				description = "Erlang component loader",
				cid = ?ERLANG_COMPONENT_LOADER_CID,
				contractID = ?ERLANG_COMPONENT_LOADER_CONTRACTID, 
				constructor = 
					?NS_GENERIC_FACTORY_CONSTRUCTOR_FUNCTION(?MODULE, create),
				% The register self proc
				registerSelfProc = 
					fun (ComponentManager, LocationPath, 
						 LoaderStr, Type, ComponentInfo) ->
						 register_self_proc(ComponentManager, LocationPath, 
											LoaderStr, Type, ComponentInfo)
					end,
				% The unregister self proc
				unregisterSelfProc = 
					fun (ComponentManager, LocationPath, 
						 LoaderStr, ComponentInfo) ->
						 unregister_self_proc(ComponentManager, LocationPath, 
											  LoaderStr, ComponentInfo)
					end
			}]
	}.

ns_get_module() ->
	ns_generic_module:create_and_query(nsmodule_info()).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Public API |
%~~~~~~~~~~~~'
create() -> 
	xpcom_object:create(?MODULE, []).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Private API |
%~~~~~~~~~~~~~'

% Implementation of the callback of the generic module. 
% Will register this module in the category of component loaders
register_self_proc(_ComponentManager, _LocationPath, 
				   _LoaderStr, _Type, _ComponentInfo) -> 
	% Get the category manager from the service manager
	{ok, ServiceManager} = erlxpcom_orb:get_service_manager(),
	
	case xpcom_object:call_method(ServiceManager, getServiceByContractID, 
		["@mozilla.org/categorymanager;1", ?NSICATEGORYMANAGER_IID]) of
		{ok, [CategoryManager]} ->
			xpcom_object:addref(CategoryManager),
			Reply = 
			case xpcom_object:call_method(CategoryManager, addCategoryEntry, 
						["component-loader",
						?ERLANG_COMPONENT_MIME_TYPE, 
						?ERLANG_COMPONENT_LOADER_CONTRACTID,
						true, true]) of
				{ok, _} -> 
					?LOG_MSG(erlxpcom, ?LOGGER_INFO, 
						"Erlang component loader registered in caterory manager!", []),
					ok;
				{error, Reason} ->
					?LOG_MSG(erlxpcom, ?LOGGER_INFO, 
						"Erlang component loader NOT registered in caterory manager~n   Error=~p", 
						[Reason]), 
					{error, Reason}
			end,
			xpcom_object:release(CategoryManager),
			Reply;
		{error, Reason} ->
			?LOG_MSG(erlxpcom, ?LOGGER_INFO, 
				"Can't register loader, failed getting category manager~n   Error=~p", 
				[Reason]),
			{error, Reason}
	end.

% Implementation of the callback of the generic module. 
% Will unregister this module in the category of component loaders
unregister_self_proc(_ComponentManager, _LocationPath, 
		 		     _LoaderStr, _ComponentInfo) -> 
	% Get the category manager from the service manager
	{ok, ServiceManager} = erlxpcom_orb:get_service_manager(),
	
	case xpcom_object:call_method(ServiceManager, getServiceByContractID, 
		["@mozilla.org/categorymanager;1", ?NSICATEGORYMANAGER_IID]) of
		{ok, [CategoryManager]} ->
			xpcom_object:addref(CategoryManager),
			Reply = 
			case xpcom_object:call_method(CategoryManager, removeCategoryEntry, 
							["component-loader",
							?ERLANG_COMPONENT_MIME_TYPE, true]) of
				{ok, _} -> 
					?LOG_MSG(erlxpcom, ?LOGGER_INFO, 
						"Erlang component loader unregistered in caterory manager", []),
					ok; 
				{error, Reason} ->
					?LOG_MSG(erlxpcom, ?LOGGER_INFO, 
						"Erlang component loader NOT unregistered in caterory manager~n    Error=~p", 
						[Reason]),
					{error, Reason}
			end,
			xpcom_object:release(CategoryManager),
			Reply;
		{error, Reason} ->
			?LOG_MSG(erlxpcom, ?LOGGER_INFO, 
				"Can't unregister loader, failed getting category manager~n    Error=~p", 
				[Reason]),
			{error, Reason}
	end.

%~~~~~~~~~~~~~~~~~~~~~~~
% Registration functions

% Check if a component file needs be update
component_needs_update(_ComponentPath) ->
	% FIXME: implement
	true.


%
% Check if a path is a component. The erlang components will be directories
% with extension .erlang in the component directory. Such directories will
% have a least a module with the same name than component directory without
% suffix and with an ns_get_module() function exported:
% 
% components/testComponent.erlang/testComponent.beam
% @spec is_erlang_component(ComponentPath) -> true | false 
%   ComponentPath = path to component dir
%	ModuleName = name of module contained in the component dir (without suffix)
is_erlang_component(ComponentPath) ->
	case filelib:is_dir(ComponentPath) of
	true -> 
		case lists:suffix(?ERLANG_COMPONENT_SUFFIX, ComponentPath) of
		true ->
			ModuleName = list_to_atom(
				filename:rootname(filename:basename(ComponentPath))),
			{ok, ModuleName};
		false -> false
		end;
	false -> false
	end.

%
% This function will try to load the component erlang module and get the
% nsIModule calling to ns_get_module function.
% Returns a instance to the new NsModule. component dir is added to path.
% Returns false if component path is not a erlang component.
% returns error if there is a error
% @spec load_component_module(ComponentPath) -> 
%	{ok, NsModule} | false | {error, Reason}
% NsModule = 
load_component_module(ComponentPath) ->
	case is_erlang_component(ComponentPath) of
		{ok, ModuleName} ->
			% add it to path
			code:add_path(ComponentPath),
			% try to get the module
			Reply = 
			case (catch ModuleName:ns_get_module()) of
				{ok, NsModule} ->
					{ok, NsModule};
				{error, Reason} ->	
					{error, Reason};
				{'EXIT', {undef, _}} -> 
					{error, no_ns_get_module};
				Other -> 
					{error, Other}
			end,
			?LOG_MSG(erlxpcom, ?LOGGER_ERROR, 
				"component_loader:load_component_module(~s) ->~n    ~p", 
				[ComponentPath, Reply]),
			Reply;
		false -> false
	end.

%
% Register the components in a directory.
%
register_components(When, NsDirectory, State) -> 
	% Get the list of files and process it
	{ok, DirectoryPath} = 
		xpcom_object:get_attribute(NsDirectory, path),
	case file:list_dir(DirectoryPath) of
		{ok, Files} ->
			lists:foreach(fun(File) -> 
							register_component(When, 
											   filename:join(DirectoryPath, File), 
											   State) 
						  end,
						  Files);
		Error ->
			?LOG_MSG(erlxpcom, ?LOGGER_DEBUG, 
					"erlxpcom_loader:register_components() -> Failed: ~p", 
					[Error])
	end,
	{ok, []}.
	
% Register a component
register_component(_When,FilePath, State) -> 
	case load_component_module(FilePath) of
		% is not an erlang module
		false -> 
			{ok, [false]};
		% we got the module, register it
		{ok, NsModule} ->
			?LOG_MSG(erlxpcom, ?LOGGER_DEBUG, 
				"erlxpcom_loader:register_component(~s)", 
				[FilePath]),
			xpcom_object:addref(NsModule),
			% Create the nsIFile representation of this path
			{ok, [NsFile]} = 
				xpcom_object:call_method(State#?MODULE.component_manager,
					createInstanceByContractID,
					["@mozilla.org/file/local;1", null, ?NSIFILE_IID]),
			xpcom_object:addref(NsFile),
			% call the NsModule:registerSelf()
			% FIXME: What should we put in LoaderStr :?... actually put the filepath
			Reply = 
			case xpcom_object:call_method(NsModule, registerSelf, 
						[State#?MODULE.component_manager, NsFile, 
						FilePath, ?ERLANG_COMPONENT_MIME_TYPE]) of
				{ok, _} -> {ok, [true]};
				Other -> Other
			end,
			xpcom_object:release(NsModule),
			xpcom_object:release(NsFile),
			Reply;
		{error, Error} -> {error, Error}
	end.

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% xpcom_object callbacks |
%~~~~~~~~~~~~~~~~~~~~~~~~'
init([]) ->
	{ok, #?MODULE{}}.

xpcom_object_info(iids, _Params, _State) ->
	[?NSICOMPONENTLOADER_IID].   
	
query_interface(_IID, _This, State) ->
	{no_interface, State}.

terminate(_Reason, _State) -> ok.
		
%~~~~~~~~~~
method(init, [ComponentManager, _Registry], _, State) ->
	case Reply = xpcom_object:query_interface(
					ComponentManager, ?NSICOMPONENTREGISTRAR_IID) of
		{ok, ComponentRegistry} -> 
			NewState = State#?MODULE{component_manager = ComponentManager, 
									 component_registry = ComponentRegistry,
									 initialized = true},
			{reply, {ok, []}, NewState};
		{error, _} -> Reply
	end;

method(autoRegisterComponents, [When, Directory], Client, State) ->
	% This is an slow operation and with reentrant calls. 
	% Do it in other process. 
	spawn(fun() ->
			Reply = register_components(When, Directory, State),
			xpcom_object:reply(Client, Reply)
		  end),
	{noreply, State};

method(autoRegisterComponent, [When, NsFile], _, State) ->
	{ok, [FilePath]} = xpcom_object:call_method(NsFile, path, []),
	Reply = register_component(When, FilePath, State), 
	{reply, Reply, State};

method(autoUnregisterComponent, [_When, _Component], _, State) ->
	% fixme unimplemented
	{reply, {ok, [false]}, State};

method(getFactory, [CID, Location, ?ERLANG_COMPONENT_MIME_TYPE], _, State) ->
	Reply = 
	case load_component_module(Location) of	
		false -> 
			{error, ?NS_ERROR_FACTORY_NOT_REGISTERED};
		{ok, NsModule} ->
			% Get the factory
			Reply2 = 
				xpcom_object:call_method(NsModule, getClassObject, 
						[State#?MODULE.component_manager, CID, ?NSIFACTORY_IID]),
			% Release the module
			xpcom_object:release(NsModule),
			Reply2;
		{error, Error} -> {error, Error}
	end,
	{reply, Reply, State};
% only instance erlang type
method(getFactory, [_CID, _Location, _Type], _, State) ->
	{reply, {error, ?NS_ERROR_INVALID_ARG}, State};


method(onRegister, [_CID, _Type, _ClassName, _ContractId, 
					_Location, _Replace, _Persist], _, State) ->
	{reply, {ok, []}, State};

method(registerDeferredComponents, [_When], _, State) ->
	% FIXME: unimplemented
	{reply, {ok, [false]}, State};

method(unloadAll, [_When], _, State) ->
	% FIXME: unimplemented
	{reply, {ok, [false]}, State}.

set_attribute(Attribute, _Value, _, State) ->
	{reply, {error, {unknown_method, Attribute}}, State}.

get_attribute(Attribute,  _, State) ->
	{reply, {error, {unknown_method, Attribute}}, State}.

info(_Msg, State) -> {no_reply, State}.
								

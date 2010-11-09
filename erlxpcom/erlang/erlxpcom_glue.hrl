%%
%% This file defines some usefull macros and record for use with erlxpcom
%% 


%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% Generic module/factory code.
%%
%% Based on nsModuleComponentInfo from nsIGenericFactory.h
%% For full documentation see:
%%	http://lxr.mozilla.org/seamonkey/source/xpcom/glue/nsIGenericFactory.h
%%
%% The function callbacks defined here follow the same rules than 
%% XPIDL/C++/Erlang mapping  

%% Create a generic constructor for the object implemented in Module
-define(NS_GENERIC_FACTORY_CONSTRUCTOR(Module),
	fun
		(null, _IID) -> 
			case Result = xpcom_object:create(Module, []) of 
				{ok, _} -> Result;
				_ -> Result
			end;
		(_, _) -> {error, no_agregation} % agregation is not allowed
	end 
	).

%% Create a generic constructor that will call the function Module:Function()
-define(NS_GENERIC_FACTORY_CONSTRUCTOR_FUNCTION(Module, Function),
	fun
		(null, _IID) -> 
			case Result = Module:Function() of 
				{ok, _} -> Result;
				_ -> Result
			end;
		(_, _) -> {error, no_agregation} % agregation is not allowed
	end 
	).

%% Create a generic constructor that will call the function Module:Function(Args)
-define(NS_GENERIC_FACTORY_CONSTRUCTOR_FUNCTION_ARGS(Module, Function, Args),
	fun
		(null, _IID) -> 
			case Result = Module:Function(Args) of 
				{ok, _} -> Result;
				_ -> Result
			end;
		(_, _) -> {error, no_agregation} % agregation is not allowed
	end 
	).


%% Use this type to define a list of module component info to pass to 
%% ns_generic_module::create()
%%
%% @param description           : Class Name of given object
%% @param cid                   : CID of given object
%% @param contractID            : Contract ID of given object
%% @param constructor           : Constructor of given object of type 
%%                                fun(Outer, IID) -> {ok, Object} | {error, X}
%%                                (see NSConstructorProcPtr in nsIGenericFactory.h)
%% @param registerSelfProc      : (optional) Registration Callback of type
%%                                (see NSRegisterSelfProcPtr in nsIGenericFactory.h)
%% @param unregisterSelfProc    : (optional) Unregistration Callback
%%                                (see NSUnregisterSelfProcPtr in nsIGenericFactory.h)
%% NOT IMPLEMENTED @param canUnloadProc			: (optional) CanUnload callback, called when the
%%                                module can be unloaded.
%% NOT IMPLEMENTED @param factoryDestructor     : (optional) Destruction Callback
%% NOT IMPLEMENTED @param getInterfacesProc     : (optional) Interfaces Callback
%% NOT IMPLEMENTED @param getLanguageHelperProc : (optional) Language Helper Callback
%% NOT IMPLEMENTED @param classInfoGlobal       : (optional) Global Class Info of given object 
%% NOT IMPLEMENTED @param flags                 : (optional) Class Info Flags @see nsIClassInfo 
%%		
-record(ns_module_component_info,
		{
		description, 
		cid, 
		contractID, 
		constructor,
		registerSelfProc = none,
		unregisterSelfProc = none
		}).

%% 
%% ns_module_info
%% 
%% Use this structure to define meta-information about the module
%% itself, including the name, its components, and an optional
%% module-level initialization or shutdown routine.
%% 
%% @param mVersion     : Module Info Version
%% @param mModuleName  : Module Name
%% @param mComponents  : List of Components 
%% @param mCtor        : (optional) Module user defined initializer
%% @param mDtor        : (optional) Module user defined destructor
-record(ns_module_info, 
		{
			version, 
			moduleName, 
			components, 
			module_constructor = none, 
			module_destructor = none
		}).
		

%% Implementation of ITestErlXPCOMComponent component
-module(testErlXPCOMComponent).

-include("erlxpcom.hrl").

-behaviour(xpcom_object).

% xpcom_object exports
-export([init/1, terminate/2, xpcom_object_info/3, query_interface/3, 
		method/4, set_attribute/4, get_attribute/3, info/2]).

% component exports
-export([ns_get_module/0]).

% Public exports
-export([create/0]).

-define(ITESTERLXPCOMCOMPONENT_IID, "37a524b7-e2ed-4b35-8560-0636148f5282").
-define(ERLANG_ITESTERLXPCOMCOMPONENT_CID, "733d51c3-c125-411c-8ea0-d500c7ad905c").

create() ->
	xpcom_object:create(?MODULE, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generic module definitions
nsmodule_info() ->
	#ns_module_info{
		version = "0.0.1",
		moduleName = "Erlang test module", 
		components = [
			#ns_module_component_info{
				description = "Erlang ITestErlXPCOMComponent",
				cid = ?ERLANG_ITESTERLXPCOMCOMPONENT_CID,
				contractID = "@lfcia.org/erlang/TestErlXPCOMComponent", 
				constructor = 
				  ?NS_GENERIC_FACTORY_CONSTRUCTOR_FUNCTION(?MODULE, create)
			}],
		module_constructor = 
			fun (_) -> 
				?LOG_MSG(erlxpcom, ?LOGGER_DEBUG, 
					"Building module.", [])
			end, 
		module_destructor = 
			fun (_) -> 
				?LOG_MSG(erlxpcom, ?LOGGER_DEBUG, 
					"Destroying module.", [])
			end
	}.

ns_get_module() ->
	ns_generic_module:create_and_query(nsmodule_info()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_server callbacks

init([]) ->
	{ok, none}.

xpcom_object_info(iids, _Params, _State) ->
	[?ITESTERLXPCOMCOMPONENT_IID].   % ITestErlXPCOMComponent

query_interface(_IID, _This, State) ->
	{no_interface, State}.

terminate(_Reason, _State) -> ok.
		
method(aMethod, [], _, State) ->
	io:format("Hello from aMethod()~n"),
	{reply, {ok, []}, State};

method('GetStrings', [], _, State) ->
	io:format("Hello from GetString()~n"),
	{reply, {ok, [["hola", "mundo", "cruel"]]}, State};

method(MethodName, Params, _, State) ->
	io:format("Hello from ~p(Params):~n   Params: ~p", [MethodName, Params]),
	{reply, {error, not_implemented}, State}.

set_attribute(Attribute, Value, _, State) ->
	io:format("Attribute ~s=~p", [Attribute, Value]),
	{reply, {error, not_implemented}, State}.

get_attribute(Attribute,  _, State) ->
	io:format("Get Attribute ~s", [Attribute]),
	{reply, {error, not_implemented}, State}.
			
info(_Msg, State) -> {no_reply, State}.
						

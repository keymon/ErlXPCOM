%%
%% Stores and manages all the remote and local XPCOM object references
%%
-module(erlxpcom_orb).

-include("logger.hrl").

-behaviour(gen_server).

-define(ORB_REGISTER_NAME, erlxpcom).
-define(COMPONENT_MANAGER_OID, 0).
-define(SERVICE_MANAGER_OID, 1).

% Public exports
-export([start/1, register_remote_object/2, register_local_object/2, 
		get_remote_object_by_oid/2, get_remote_object_by_ref/2, 
		get_local_object_by_oid/2, get_local_object_by_ref/2,
		unregister_remote_object/2, unregister_local_object/2, 
		get_component_manager/0, get_service_manager/0]).

% genserver exports
-export([init/1, handle_call/3, handle_cast/2, 
		 handle_info/2, terminate/2, code_change/3]).

% ORB State 
-record(orb_state, 
		{ 
			% Map of registered remote xpcom objects by OID (remote representation)
			remote_objects_by_oid = dict:new(),       
			% Map of registered remote xpcom objects by ObjectRef (local representation)
			remote_objects_by_ref = dict:new(),       
			% Map of registered local xpcom objects by OID (remote representation)
			local_objects_by_oid = dict:new(),       
			% Map of registered local xpcom objects by ObjectRef (remote representation)
			local_objects_by_ref = dict:new(),       
			% Reference to request transport that will 
			%    comunicate with mozilla side
			request_transport,
			% Counter of local generated OIDs
			oids_counter = 0			
		}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Start a new ORB, using the given pid to send remote calls
%% It will return the pid of the RequestTransport that will comunicate with 
%% mozilla side
%% This call will create the component_loader module and will register it
%% as the first local object (OID=0)
start(MozillaPid) ->
	% Init logger.
	?LOG_START_OPTIONS([{level, ?LOGGER_DEBUG}]),
	?LOG_DEFINE_TAG(erlxpcom, [{string, "ErlXPCOM"},
		{destination, {file, "erlxpcom.log"}}]), 
	?LOG_DEFINE_TAG(erlxpcom_object, [{string, "XPCOM Object"},
		{destination, {file, "erlxpcom.log"}}]), 
	?LOG_DEFINE_TAG(erlxpcom_transport, [{string, "XPCOM Transport"},
		{destination, {file, "erlxpcom.log"}}]), 

	?LOG_MSG(erlxpcom, ?LOGGER_DEBUG, 
		"Starting ErlXPCOM ORB", []),

	% Start the request transport.
	{ok, RequestTransport} = erlxpcom_request_transport:start(MozillaPid),
	% try to start the erlang ORB
	case gen_server:start({local, ?ORB_REGISTER_NAME}, ?MODULE, [RequestTransport], []) of
		{ok, Orb} -> 
			gen_server:call(RequestTransport, {setorb, Orb}), 
			% register the component manager.
			{ok, ComponentManager} = 
				register_remote_object(Orb, ?COMPONENT_MANAGER_OID), 
			{ok, ServiceManager} = 
				register_remote_object(Orb, ?SERVICE_MANAGER_OID), 
			% increase the reference, they will always be there
			xpcom_object:addref(ComponentManager),
			xpcom_object:addref(ServiceManager),

			% Register the component_loader
			?LOG_MSG(erlxpcom, ?LOGGER_DEBUG, "Registering component loader", []),

			{ok, ComponentLoader} = component_loader:ns_get_module(), 
			case erlxpcom_orb:register_local_object(Orb, ComponentLoader) of 
				{error, Reason} -> 
					?LOG_MSG(erlxpcom, ?LOGGER_DEBUG, 
							"Error registering component-loader: ~p", [Reason]);
				_ -> ok
			end,
			% release the addref'd by ns_get_module() object.
			xpcom_object:release(ComponentLoader), 

			?LOG_MSG(erlxpcom, ?LOGGER_DEBUG, "Orb started", []),
			% return the request transport
			RequestTransport;
		{error, {already_started,_}} -> 
			% if its already started, return the request transport of running orb
			get_request_transport();
		Other -> Other
	end.		
	
%% 
%% Registers a Mozilla side XPCOM object (by OID)
%% It will create a local process with reference counter = 0.
%%
register_remote_object(OrbRef, OID) ->
	gen_server:call(OrbRef, {register_remote_object, OID}). 

%% 
%% Registers a local XPCOM object (by ObjectRef).
%% This function will increase the reference counter
%%
register_local_object(OrbRef, ObjectRef) ->
	gen_server:call(OrbRef, {register_local_object, ObjectRef}). 
	
%%
%% @spec get_remote_object_by_oid(OID) -> {ok, ObjectRef} | not_found
%% Searchs for a remote XPCOM object by OID
%%
get_remote_object_by_oid(OrbRef, OID) -> 
	gen_server:call(OrbRef, {get_remote_object_by_oid, OID}).

%%
%% @spec get_remote_object_by_ref(ObjectRef) -> {ok, OID} | not_found
%% Searchs for a remote XPCOM object by ObjectRef
%%
get_remote_object_by_ref(OrbRef, ObjectRef) -> 
	gen_server:call(OrbRef, {get_remote_object_by_ref, ObjectRef}).

%%
%% @spec get_local_object_by_oid(OID) -> {ok, ObjectRef} | not_found
%% Searchs for a local XPCOM object by OID
%%
get_local_object_by_oid(OrbRef, OID) -> 
	gen_server:call(OrbRef, {get_local_object_by_oid, OID}).

%%
%% @spec get_local_object_by_ref(ObjectRef) -> {ok, OID} | not_found
%% Searchs for a local XPCOM object by ObjectRef
%%
get_local_object_by_ref(OrbRef, ObjectRef) -> 
	gen_server:call(OrbRef, {get_local_object_by_ref, ObjectRef}).

%%
%% Removes a remote XPCOM object of the list of remote registered objects. 
%% Will send a message to mozilla side in order to drop this object
%%
unregister_remote_object(OrbRef, ObjectRef) ->
	gen_server:call(OrbRef, {unregister_remote_object, ObjectRef}).

%%
%% Removes a local XPCOM object of the list of local registered objects. 
%% Will decrease the reference coutner of the object
%%
unregister_local_object(OrbRef, OID) ->
	gen_server:call(OrbRef, {unregister_local_object, OID}).

%%
%% Get the component manager instante reference
%%
get_component_manager() ->
	get_remote_object_by_oid(?ORB_REGISTER_NAME, ?COMPONENT_MANAGER_OID).

%%
%% Get the service manager instante reference
%%
get_service_manager() ->
	get_remote_object_by_oid(?ORB_REGISTER_NAME, ?SERVICE_MANAGER_OID).

%%
%% Get the request tranport pid
%%
get_request_transport() ->
	gen_server:call(?ORB_REGISTER_NAME, 'get_request_transport').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_server callbacks

init([RequestTransportPid]) ->
	% create the state
	InitialState = #orb_state{request_transport=RequestTransportPid},
	{ok, InitialState}.

handle_call({register_remote_object, OID}, _From, State) ->
	% Check if its a member already
	case dict:find(OID, State#orb_state.remote_objects_by_oid) of
		{ok, [ObjectId|_]} ->
			Reply = {error, {duplicated_object, OID, ObjectId}},
			NewState = State;
		_ ->
			% ... if not create the object
			{ok, NewObjectId} = 
				object_proxy:create(OID, self(), State#orb_state.request_transport),
			% ... register it
			NewState = State#orb_state{
						remote_objects_by_oid = 
							dict:append(OID, NewObjectId, 
										State#orb_state.remote_objects_by_oid), 
						remote_objects_by_ref = 
							dict:append(NewObjectId, OID, 
										State#orb_state.remote_objects_by_ref)}, 
			% ... and return it
			Reply = {ok, NewObjectId}
	end, 
	{reply, Reply, NewState};

handle_call({register_local_object, ObjectRef}, _From, State) ->
	% Check if its a member already
	case dict:find(ObjectRef, State#orb_state.local_objects_by_ref) of
		{ok, OID} ->
			Reply = {error, {duplicated_object, ObjectRef, OID}},
			NewState = State;
		_ ->
			% ... if not get new OID
			NewOID = State#orb_state.oids_counter,
			% ... register it
			NewState = State#orb_state{
						local_objects_by_oid = 
							dict:append(NewOID, ObjectRef, 
										State#orb_state.local_objects_by_oid), 
						local_objects_by_ref = 
							dict:append(ObjectRef, NewOID, 
										State#orb_state.local_objects_by_ref),
						oids_counter = NewOID+1}, 
			% ... increase reference counter
			xpcom_object:addref(ObjectRef),
			% ... and return it
			Reply = {ok, NewOID}
	end, 
	{reply, Reply, NewState};
	
handle_call({get_remote_object_by_oid, OID}, _From, State) ->
	case dict:find(OID, State#orb_state.remote_objects_by_oid) of
		{ok, [Result|_]} ->
			Reply = {ok, Result};
		_ ->
			Reply = error
	end,
	{reply, Reply, State};

handle_call({get_remote_object_by_ref, ObjectRef}, _From, State) ->
	case dict:find(ObjectRef, State#orb_state.remote_objects_by_ref) of
		{ok, [Result|_]} ->
			Reply = {ok, Result};
		_ ->
			Reply = error
	end,
	{reply, Reply, State};

handle_call({get_local_object_by_oid, OID}, _From, State) ->
	case dict:find(OID, State#orb_state.local_objects_by_oid) of
		{ok, [Result|_]} ->
			Reply = {ok, Result};
		_ ->
			Reply = error
	end,
	{reply, Reply, State};

handle_call({get_local_object_by_ref, ObjectRef}, _From, State) ->
	case dict:find(ObjectRef, State#orb_state.local_objects_by_ref) of
		{ok, [Result|_]} ->
			Reply = {ok, Result};
		_ ->
			Reply = error
	end,
	{reply, Reply, State};


handle_call({unregister_remote_object, ObjectRef}, _, State) ->
	case dict:find(ObjectRef, State#orb_state.remote_objects_by_ref) of
		{ok, [OID|_]} ->
			NewState = 
				State#orb_state{
					remote_objects_by_ref = 
						dict:erase(ObjectRef, State#orb_state.remote_objects_by_ref), 
					remote_objects_by_oid = 
						dict:erase(OID, State#orb_state.remote_objects_by_oid)
					},
			% Drop the object in mozilla side
			erlxpcom_request_transport:drop_object(
				NewState#orb_state.request_transport, OID),
			Reply = ok;
		_ ->
			NewState = State, 
			Reply = {error, not_found}
	end,
	{reply, Reply, NewState};

handle_call({unregister_local_object, OID}, _, State) ->
	case dict:find(OID, State#orb_state.local_objects_by_oid) of
		{ok, [ObjectRef|_]} ->
			NewState = 
				State#orb_state{
					local_objects_by_ref = 
						dict:erase(ObjectRef, State#orb_state.local_objects_by_ref), 
					local_objects_by_oid = 
						dict:erase(OID, State#orb_state.local_objects_by_oid)
					},
			% Decrease reference counter of object 
			xpcom_object:release(ObjectRef),
			Reply = ok;
		_ ->
			NewState = State, 
			Reply = {error, not_found}
	end,
	{reply, Reply, NewState};

	
handle_call('get_request_transport', _From, State) ->	
	{reply, State#orb_state.request_transport, State};

% default call handler
handle_call(_Msg, _From, State) ->
	?LOG_MSG(erlxpcom, ?LOGGER_ERROR, "Unknown call received:~n    ~p", [_Msg]), 
	{reply, {error, {error, unknown_message}}, State}. 

% default cast handler
handle_cast(_Msg, State) ->
	?LOG_MSG(erlxpcom, ?LOGGER_ERROR, "Unknown cast received:~n    ~p", [_Msg]), 
	{noreply, State}. 

terminate(Reason, _) -> Reason.

handle_info(_Msg, State) ->
	?LOG_MSG(erlxpcom, ?LOGGER_INFO, "Info message: ~p", [_Msg]), 
	{noreply, State}.

code_change(_, State, _) -> {ok, State}.

%% @doc 
%% Behaviour modules for implementation Erlang XPCOM Objects.
%% 
%% The behaviour implements the reference counter, destroying the object 
%% (stoping the process) if counter raises 0.~p
%%
%% The XPCOM Object module must implement a set of predefined functions:
%%  - Module:init(Args)
%%		Args = term()
%%		Result = {ok,State} | {stop,Reason}
%%		 State = term()
%%		 Reason = term()
%%
%%    this callback is called just before object creation. 
%%    The object constructs his state in this callback
%%
%%  - Module:xpcom_object_info(What, Args, State) -> [IID]
%%      What = atom()
%%		IID = String 
%%    
%%	  Optional callback. Called just after Module:init(). 
%%    Only a info is implemented: 
%%       What = iids: The object returns the list of supported IIDs. 
%%                    If the object is query_interfaced by any of the IIDs
%%                    in this list, will automacly return himself without
%%                    call the Module:query_interface() callback.
%%                    If the object needs to return a different instante or
%%                    execute something, do not put here the iid, implement the
%%                    query_interface callback.
%%
%%  - Module:terminate(Reason, State): 
%% 
%%    Optional callback. Called just before object destruction.
%%
%%  - Module:query_interface(IID, State): 
%%              {{ok, NewObject}, NewState} | {{error, Reason},NewState} | 
%%					{no_interface, NewState}
%%	
%%    Optional callback, default no_interface. Called if the object is queried
%%    with an IID that is not in the list returned in object_info(iids,...).
%%	  This class must return an addRef'd object!!!
%%
%%  - Module:method(MethodName, Params, From, State) -> 
%%		{reply, Reply, NewObjectState} | {noreply, NewObjectState}
%%			Reply = {ok, Reply} | {error, Reason}
%%			From = like in gen_server
%%
%%    Execute the XPCOM object method MethodName, using Params as In parameters.
%%    Return the list of Out parameters or error. Reason is usually an integer
%%	  with an nsresult value.
%%
%%  - Module:set_attribute(Attribute, Value, From, State) -> 
%%		{reply, Reply, NewObjectState} | {noreply, NewObjectState}
%%			Reply = ok | {error, Reason}
%%
%%    Set the attribute Attribute with Value
%%
%%  - Module:get_attribute(Attribute, From, State) -> 
%%		{reply, Reply, NewObjectState} | {noreply, NewObjectState}
%%			Reply = {ok, Value} | {error, Reason}
%%
%%    Get the attribute Attribute 
%%
%%  - Module:info(Info, State) -> Result
%%  
%%    Receive other message or timeout. This callback is like the
%%    gen_server:handle_info() callback.
%% @end
-module(xpcom_object).

-include("erlxpcom.hrl").

%-behaviour(behaviour).
-behaviour(gen_server).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Behaviour exports
-export([behaviour_info/1]).

% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, 
		 handle_info/2, terminate/2, code_change/3]).

% Public interface exports
-export([is_xpcom_object/1, xpcom_object_to_pid/1, pid_to_xpcom_object/1,
		 self_object/0, create/2, addref/1, release/1, 
		 query_interface/2, call_method/3,
		 set_attribute/3, get_attribute/2,
		 reply/2]).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-record(behaviour_state, {
			object_module,     % module that implements the object
			object_state,      % state of the object
			ref_counter = 0,   % reference counter of this object
			supported_iids     % list of supported IIDs
			}).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Public API |
%~~~~~~~~~~~~'

%% guard that checks if a term is an XPCOM object reference
is_xpcom_object({xpcom_object, _}) -> true;
is_xpcom_object(_) -> false.

%% Extract the pid from an object referece
xpcom_object_to_pid({xpcom_object, Pid}) -> Pid.

%% Create an XPCOM object reference from a Pid
pid_to_xpcom_object(Pid) -> {xpcom_object, Pid}.

%% Get a reference to self object (simply returns an object 
%% reference using self() pid).
self_object() -> pid_to_xpcom_object(self()).

%% @spec create(Module, Args) -> #XPCOMObject
%%
%% Create a new XPCOM object, implemented by module Module with arguments Args
create(Module, Args) ->
	Reply = 
	case gen_server:start(?MODULE, [Module, Args], [{timeout, infinity}]) of 
		{ok, Pid} -> 
			{ok, pid_to_xpcom_object(Pid)};
		Other ->
			Other
	end, 
	?LOG_MSG(erlxpcom, ?LOGGER_WARNING, 
			 "xpcom:create(~s, ~p) -> ~p", 
				[Module, Args, Reply]), 
	Reply.

%% @spec addref(#XPCOMObject) -> ok
%%
%% Increase the reference counter for given object.
addref(XPCOMObject) ->
	ObjectPid = xpcom_object_to_pid(XPCOMObject), 
	gen_server:cast(ObjectPid, addref).

%% @spec addref(#XPCOMObject) -> ok
%%
%% Decrease the reference counter for given object. Will destroy the object
%% if counter raises 0.
release(XPCOMObject) ->
	ObjectPid = xpcom_object_to_pid(XPCOMObject), 
	gen_server:cast(ObjectPid, release).

%% @spec query_interface(#XPCOMObject) -> {ok, NewObject} | no_interface
%% Query this object with the given IID. Returns the new object reference
%% or no_interface if IID is not supported.
%% The returned object is addRef'd
query_interface(XPCOMObject, IID) ->
	ObjectPid = xpcom_object_to_pid(XPCOMObject), 
	do_server_call(ObjectPid, {query_interface, IID}).
	

%% @spec call_method(#XPCOMObject, MethodName, Params) -> 
%%		{ok, Reply} | {error, Reason} | unknown_object
%% 
%% Call a method of a XPCOM object. It returns:
%%	{ok, Reply} with the Reply (Out params) if success
%%  {error, Reason} if the method returns an error or method call fails
%%  unknown_object if the object does not exists (pid not found)
%%
call_method(XPCOMObject, MethodName, Params) ->
	ObjectPid = xpcom_object_to_pid(XPCOMObject), 
	% Capture exceptions if object does not exists
 	do_server_call(ObjectPid, {call_method, MethodName, Params}).

%% @spec set_attribute(#XPCOMObject, Attribute, Value) -> 
%%		ok | {error, Reason} | unknown_object
%% 
%% Set an attribute in a XPCOM object. It returns:
%%	ok  if success
%%  {error, Reason} if returns an error or call fails
%%  unknown_object if the object does not exists (pid not found)
%%
set_attribute(XPCOMObject, Attribute, Value) ->
	ObjectPid = xpcom_object_to_pid(XPCOMObject), 
	% Capture exceptions if object does not exists
 	do_server_call(ObjectPid, {set_attribute, Attribute, Value}).

%% @spec get_attribute(#XPCOMObject, Attribute) -> 
%%		{ok, Value} | {error, Reason} | unknown_object
%% 
%% Get an attribute in a XPCOM object. It returns:
%%	{ok, Value}  if success with attribute value in Vallue
%%  {error, Reason} if returns an error or call fails
%%  unknown_object if the object does not exists (pid not found)
%%
get_attribute(XPCOMObject, Attribute) ->
	ObjectPid = xpcom_object_to_pid(XPCOMObject), 
	% Capture exceptions if object does not exists
 	do_server_call(ObjectPid, {get_attribute, Attribute}).
	
	
%% Explicity, send the return of a function to a client. Used when a call 
%% does not return nothing (noreply). Simply call gen_server:reply/2
reply(Client, Reply) ->
	gen_server:reply(Client, Reply).
	
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Private API |
%~~~~~~~~~~~~~'

% do a gen_server call enmascaring exception of object stoped
do_server_call(ObjectPid, Msg) ->
 	case (catch gen_server:call(ObjectPid, Msg, infinity)) of 
		% No such process
		{'EXIT', {noproc,_}} -> throw({unknown_object, ObjectPid});
		{'EXIT', Reason} -> throw(Reason);
		Other -> Other
	end.

% handle a method/attribute call request Used in hand_call({call_method...)
do_call_method(MethodName, Param, From, State, Type) ->
	Mod = State#behaviour_state.object_module, 
	ObjectState = State#behaviour_state.object_state,

	?LOG_MSG(erlxpcom_object, ?LOGGER_DEBUG, 
			 "~p-> object_xpcom:~s(~s:~s, Params)~n     `-> Params=~p", 
			 [self(), Type, Mod, MethodName, Param]),

	Response = 
	case Type of 
		call_method ->
			catch Mod:method(MethodName, Param, From, ObjectState);
		set_attribute ->
			catch Mod:set_attribute(MethodName, Param, From, ObjectState);
		get_attribute ->
			catch Mod:get_attribute(MethodName, From, ObjectState)
	end, 
	
	case Response of
		% the callback is not defined 
		{'EXIT', {function_clause, [{Mod, Type, _} |_]}} ->
			?LOG_MSG(erlxpcom_object, ?LOGGER_ERROR, 
					 "~p<# xpcom_object:~s()~n    "++
					 "Reason = callback undefined in module ~s", 
					 [self(), Type, Mod]), 
			{reply, {error, undefined_callback}, State};
		% other failure	
		{'EXIT', Reason} ->
			?LOG_MSG(erlxpcom_object, ?LOGGER_WARNING, 
					 "~p<# xpcom_object:~s(~s:~s)~n    "++
					 "Reason = ~p", 
					 [self(), Type, Mod, MethodName, Reason]), 
			{reply, {error, Reason}, ObjectState};
		% execution ok (with or without errors)
		{reply, Reply, NewObjectState} -> 
			% Check that reply have the correct format
			Reply2 = 
			case {Reply, Type} of 
				{{ok, L}, call_method} when is_list(L) -> Reply;
				{{ok, _}, get_attribute} -> Reply;
				{ok, set_attribute} -> Reply;
				{{error, _}, _} -> Reply;
				{Other, _} -> 
					{error, {bad_return_format, Other}}
			end,
			?LOG_MSG(erlxpcom_object, ?LOGGER_DEBUG, 
					 "~p<- object_xpcom:~s(~s:~s) -> ~p", 
					 [self(), Type, Mod, MethodName, Reply2]),
			{reply, Reply2, State#behaviour_state{object_state=NewObjectState}};

		{noreply, NewObjectState} ->
			?LOG_MSG(erlxpcom_object, ?LOGGER_DEBUG, 
					 "~p<~~ object_xpcom:~s(~s:~s), no reply", 
					 [self(), Type, Mod, MethodName]),
			{noreply, State#behaviour_state{object_state=NewObjectState}};

		{stop, Reason, NewObjectState} -> 
			?LOG_MSG(erlxpcom_object, ?LOGGER_DEBUG, 
					 "~p<-/- object_xpcom:~s(~s:~s), stop. Reason=~p", 
					 [self(), Type, Mod, MethodName, Reason]),
			{stop, Reason, State#behaviour_state{object_state=NewObjectState}}
		
	end.

% handle a query interface. Used in hand_call({query_interface...)
do_query_interface(IID, State) ->
	Mod = State#behaviour_state.object_module, 
	ObjectState = State#behaviour_state.object_state,
	case (catch Mod:query_interface(IID, pid_to_xpcom_object(self()), ObjectState)) of
		{'EXIT', {undef,_}} -> 
			?LOG_MSG(erlxpcom_object, ?LOGGER_WARNING, 
					 "~p<# xpcom_object:query_interface(~p)~n    "++
					 "Undefined ~s:query_interface/2",
					 [self(), IID, Mod]), 
			{reply, no_interface, State};
		{'EXIT', Reason} -> 
			?LOG_MSG(erlxpcom_object, ?LOGGER_DEBUG, 
					 "~p<# xpcom_object:query_interface(~p)~n    "
					 "~pCalled ~s:query_interface/2~n    "++
					 "Reason = ~p",
					 [self(), IID, Mod, Reason]), 
			{reply, {error, Reason}, State};
		{Other, NewObjectState} -> 
			?LOG_MSG(erlxpcom_object, ?LOGGER_DEBUG, 
					 "~p. xpcom_object:query_interface(~p) -> ~p",
					 [self(), IID, Other]), 
			% Continue updating the object state
			{reply, Other, State#behaviour_state{object_state = NewObjectState}}
	end.

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% behaviour callbacks |
%~~~~~~~~~~~~~~~~~~~~~'
behaviour_info(callbacks) ->
	[{init,1}, {xpcom_object_info, 3}, {terminate,2}, 
	 {query_interface,3}, {method,4}, 
	 {set_attribute, 4}, {get_attribute, 3}, 
	 {info, 2}].
      
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% gen_server callbacks |
%~~~~~~~~~~~~~~~~~~~~~~'

%
% Init the server for this object
% Will call the init callback
%
init([Module, Args]) ->
	% Call object initializer
	case Module:init(Args) of 
		{ok, ObjectState} ->
			% try to get the list of supported iids
			SupportedIIDs =   
			case Module:xpcom_object_info(iids, Args, ObjectState) of
				List when is_list(List) -> 
					List;
				_Other -> 
					?LOG_MSG(erlxpcom_object, ?LOGGER_WARNING, 
							 "~p. ~p:xpcom_object_info(iids)~n    "++
							 "Incorrect Reply = ~p", 
							 [self(), Module, _Other]), 
					[]				
			end, 
			% Crate the state
			NewState = #behaviour_state{object_state = ObjectState, 
									    object_module = Module,
									    supported_iids = SupportedIIDs}, 

			{ok, NewState};
		{stop, Reason} ->
			{stop, Reason};
		X ->
			?LOG_MSG(erlxpcom_object, ?LOGGER_ERROR, 
					 "Badformed response in ~s:init(): ~p",
					 [Module, X]),
			{stop, {badformed_response, X}}
	end.
			
handle_call({call_method, MethodName, Params}, From, State) ->
	case MethodName of 
		addRef -> 
			{noreply, NewState} = handle_cast(addref, State),
			{reply, {ok, [NewState#behaviour_state.ref_counter]}, NewState};
		release -> 
			{noreply, NewState} = handle_cast(release, State),
			{reply, {ok, [NewState#behaviour_state.ref_counter]}, NewState};
		'QueryInterface' ->
			case Params of
				[IID] -> 
					{reply, Reply, NewState} = 
						handle_call({query_interface, IID}, From, State),
					Reply2 = 
					case Reply of
						{ok, ReplyParams} ->
							{ok, [ReplyParams]};
						{error, _} ->
							Reply;
						X -> 
							{error, X}
					end,
					{reply, Reply2, NewState};
				_ -> 
					{error, {badarg, query_interface}}
			end;
		_ -> 
			do_call_method(MethodName, Params, From, State, call_method)
	end;
	
handle_call({set_attribute, Attribute, Value}, From, State) ->
	do_call_method(Attribute, Value, From, State, set_attribute);
	
handle_call({get_attribute, Attribute}, From, State) ->
	do_call_method(Attribute, none, From, State, get_attribute);

handle_call({query_interface, IID}, _From, State) ->
	This = pid_to_xpcom_object(self()),
	% Check if this IID corresponds to directly supported interfaces.
	case lists:member(IID, 
		[?NSISUPPORTS_IID|State#behaviour_state.supported_iids]) of 
	true ->
		% add ref
		{noreply, NewState} = handle_cast(addref, State),
		{reply, {ok, This}, NewState};
	false ->
		do_query_interface(IID, State)
	end;

% default call handler
handle_call(_Msg, _, State) ->
	?LOG_MSG(erlxpcom, ?LOGGER_ERROR, "Unknown call received:~n    ~p", [_Msg]), 
	{reply, {error, {error, unknown_message}}, State}. 

%~~~~~~~
handle_cast(addref, State) ->
	ActualCounter = State#behaviour_state.ref_counter + 1,
	?LOG_MSG(erlxpcom_object, ?LOGGER_DEBUG, 
			 "~p. xpcom_object:addref() -> ~w",
			 [self(), ActualCounter]), 
	NewState = State#behaviour_state{ref_counter = ActualCounter}, 
	{noreply, NewState};

handle_cast(release, State) ->
	ActualCounter = State#behaviour_state.ref_counter - 1,
	NewState = State#behaviour_state{ref_counter = ActualCounter}, 

	?LOG_MSG(erlxpcom_object, ?LOGGER_DEBUG, 
			 "~p. xpcom_object:release() -> ~w",
			 [self(), ActualCounter]), 
	% Destroy the object if necesary counter raises 0...
	if 
		ActualCounter == 0 ->
			{stop, normal, NewState};
		true -> 
			{noreply, NewState}
	end;

% default cast handler
handle_cast(_Msg, State) ->
	?LOG_MSG(erlxpcom, ?LOGGER_ERROR, "Unknown cast received:~n    ~p", [_Msg]), 
	{noreply, State}. 

% Terminate the execution of this object		
terminate(Reason, State) -> 
	Mod = State#behaviour_state.object_module, 
	ObjectState = State#behaviour_state.object_state,
	?LOG_MSG(erlxpcom_object, ?LOGGER_DEBUG, 
			 "~p. xpcom_object:terminate(Module=~s, Reason=~p).",
			 [self(), Mod, Reason]), 
	catch Mod:terminate(Reason, ObjectState).

handle_info(Msg, State) -> 
	?LOG_MSG(erlxpcom, ?LOGGER_INFO, "Info message: ~p", [Msg]), 
	Mod = State#behaviour_state.object_module, 
	ObjectState = State#behaviour_state.object_state,
	case (catch Mod:info(Msg, ObjectState)) of 
		{'EXIT', Reason} ->
			?LOG_MSG(erlxpcom_object, ?LOGGER_DEBUG, 
				"~p. xpcom_object:terminate(Module=~s, Reason=~p).",
				[self(), Mod, Reason]), 
			throw(Reason);
		{noreply, NewObjectState} ->
			{noreply, State#behaviour_state{object_state=NewObjectState}};
		{stop, Reason, NewObjectState} -> 
			?LOG_MSG(erlxpcom_object, ?LOGGER_DEBUG, 
					 "~p<-/- object_xpcom:method_info(), stop. Reason=~p", 
					 [self(), Reason]),
			{stop, Reason, State#behaviour_state{object_state=NewObjectState}}
	end.

code_change(_, State, _) -> {ok, State}.


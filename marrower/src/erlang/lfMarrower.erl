%%
%% Fachada XPCOM de Marrower  
%%
%%
-module(lfMarrower).

-behaviour(xpcom_object).

% xpcom_object exports
-export([init/1, terminate/2, xpcom_object_info/3, query_interface/3,
		method/4, set_attribute/4, get_attribute/3, info/2]).

% public exports
-export([create/0]).

-include("lfIMarrower.hrl").
-include("lfIUserVO.hrl").
-include("lfITaskVO.hrl").

-record(?MODULE, { 
			login,
			caster
		}).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Public API |
%~~~~~~~~~~~~'
create() -> 
	xpcom_object:create(?MODULE, []).
	
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% xpcom_object callbacks |
%~~~~~~~~~~~~~~~~~~~~~~~~'
init([]) ->
	% Conecta co servidor de Marrower
	case marrower_server:connect() of 
		ok -> 
			% Obtemos o PbserverService
			{ok, ServiceManager} = erlxpcom_orb:get_service_manager(),
			{ok, [ObserverService]} = 
				xpcom_object:call_method(ServiceManager, 
										 getServiceByContractID,
										["@mozilla.org/observer-service;1", 
										 "d07f5192-e3d1-11d2-8acd-00105a1b8860"]),
			{ok, #?MODULE{login = offline, caster = ObserverService}};
		_ -> {stop, {error, can_not_connect}}
	end.
	

xpcom_object_info(iids, _Params, _State) ->
	[?LFIMARROWER_IID].   
	
query_interface(_IID, _This, State) ->
	{no_interface, State}.

terminate(_Reason, _State) -> 
	ok.
		
%~~~~~~~~~~

method(listAllUsers, [], _, State) ->
	Fun = fun ({UserState, Login, Alias}, {StateAcum, LoginAcum, AliasAcum}) ->
		DecodedUserState = case UserState of online -> ?ONLINE; _ -> ?OFFLINE end,
		{[DecodedUserState|StateAcum], [Login|LoginAcum], [Alias|AliasAcum]}
	end, 
	{States, Logins, Aliases} = lists:foldr(Fun, {[],[],[]}, marrower_server:list_all_users()),
	{reply, {ok, [States, Logins, Aliases]}, State};
	
method(getUserInfo, [Login], _, State) ->
	Result = 
	case marrower_server:get_user_info(Login) of
		unknown_user -> null;
		User -> 
			{ok, Object} = lfUserVO:create(User), 
			Object
	end,
	{reply, {ok, [Result]}, State};
	
method(sendMessage, [ToLogin, Message], _, State) ->
	Reply = 
	case State#?MODULE.login of
		offline -> 
			{error, offline};
		FromLogin -> 
			marrower_server:send_message(FromLogin, ToLogin, Message), 
			{ok, []}
	end,
	{reply, Reply, State};
	
method(login, [Login, Alias], _, State) ->
	case marrower_server:login(Login, Alias, self()) of
		unknown_user -> 
			{reply, {error, unknown_user}, State};
		_ -> 
			{reply, {ok, []}, State#?MODULE{login = Login}}
	end;

method(logout, [], _, State) ->
	case State#?MODULE.login of
		offline -> true;
		FromLogin -> marrower_server:logout(FromLogin)
	end,
	{reply, {ok, []}, State#?MODULE{login = offline}};

%~~~~~~~~~~
method(findAllTasks, [], _, State) ->
	% Consultamos ó servidor central
	Items = marrower_server:find_all_tasks(), 
	% Construimos un enumerador para estes elementos	
	Fun = fun(Task) -> {ok, TaskVO} = lfTaskVO:create(Task), TaskVO end,
	{ok, Enumerator} = lfListSimpleEnumerator:create(lists:map(Fun, Items)), 
	% devolvemos o enumerador 
	{reply, {ok, [Enumerator]}, State};

method(findTaskById, [Id], _, State) ->
	Reply = 
	case marrower_server:find_task_by_id(Id) of 
		not_found -> null;
		Task -> 
			{ok, TaskVO} = lfTaskVO:create(Task),
			TaskVO
	end, 
	{reply, {ok, [Reply]}, State};

method(findTaskByAssigned, [Login], _, State) ->
	Items = marrower_server:find_task_by_assigned(Login), 
	Fun = fun(Task) -> {ok, TaskVO} = lfTaskVO:create(Task), TaskVO end,
	{reply, {ok, [lists:map(Fun, Items)]}, State};

method(createTask, [TaskVO], _, State) ->
	Task = lfTaskVO:get_task(TaskVO), 
	Reply = 
	case marrower_server:create_task(Task) of 
		duplicated -> {error, duplicated};
		NewTask -> 
			{ok, NewTaskVO} = lfTaskVO:create(NewTask),
			{ok, [NewTaskVO]}
	end,
	{reply, Reply, State};

method(updateTask, [TaskVO], _, State) ->
	Task = lfTaskVO:get_task(TaskVO), 
	Reply = 
	case marrower_server:update_task(Task) of 
		not_found -> {error, not_found};
		_ -> {ok, []}
	end,
	{reply, Reply, State};

method(removeTask, [Id], _, State) ->
	marrower_server:remove_task(Id),
	{reply, {ok, []}, State};

method(findPredepends, [Id], _, State) ->
	{reply, {ok, [marrower_server:find_predepends(Id)]}, State};

method(findPostdepends, [Id], _, State) ->
	{reply, {ok, [marrower_server:find_postdepends(Id)]}, State};

method(addRelation, [Id1, Id2], _, State) ->
	Reply = 
	case marrower_server:add_relation(Id1, Id2) of 
		not_found -> {error, not_found}; 
		ok -> {ok, []}
	end, 
	{reply, Reply, State};

method(removeRelation, [Id1, Id2], _, State) ->
	marrower_server:remove_relation(Id1, Id2), 
	{reply, {ok, []}, State};

method(Method, _, _, State) -> % FIN
	{reply, {error, {unknown_method, Method}}, State}.

set_attribute(Attribute, _Value, _, State) ->
	{reply, {error, {unknown_method, Attribute}}, State}.

get_attribute(Attribute,  _, State) ->
	{reply, {error, {unknown_method, Attribute}}, State}.

% Aviso de conexión dun usuario
info({user_online, Login}, State) -> 
	UserVO = 
	case marrower_server:get_user_info(Login) of
		unknown_user -> null;
		User -> 
			{ok, Object} = lfUserVO:create(User), 
			Object
	end,
	xpcom_object:call_method(State#?MODULE.caster, notifyObservers, 
							[UserVO, "marrower-user-online", ""]), 
	{noreply, State};

% Aviso de desconexión dun usuario
info({user_offline, Login}, State) -> 
	UserVO = 
	case marrower_server:get_user_info(Login) of
		unknown_user -> null;
		User -> 
			{ok, Object} = lfUserVO:create(User), 
			Object
	end,
	xpcom_object:call_method(State#?MODULE.caster, notifyObservers, 
							[UserVO, "marrower-user-offline", ""]),
	{noreply, State};

info(_, State) -> 
	{noreply, State}.

								



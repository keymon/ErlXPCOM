%
% Mini servidor de chat
%
-module(marrower_server).

-behaviour(gen_server).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, 
		 handle_info/2, terminate/2, code_change/3]).


% Public interface exports
-export([start/0, connect/0, list_all_users/0, get_user_info/1, 
		 send_message/3, login/3, logout/1,
		 find_all_tasks/0, find_task_by_id/1, find_task_by_assigned/1, 
		 create_task/1, update_task/1,  remove_task/1, 
		 find_all_relations/0, find_predepends/1, find_postdepends/1, 
		 add_relation/2, remove_relation/2
		 ]).

		
-record(?MODULE, { 
		users,       % Usuarios
		tasks        % Tareas
		}).


%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Public API |
%~~~~~~~~~~~~'

%% Inicia o servidor de chat
start() ->
	case gen_server:start({global, ?MODULE}, ?MODULE, [], []) of 
		{ok, _} -> ok;
		{error, {already_started, _}} -> ok;
		error -> error
	end.
	
%% Connecta este nodo co nodo do servidor centrall
connect() -> start().

%% Listar todos os usuarios . Devolve unha lista de tuplas: 
%%		{online|offline, Login, Alias}
list_all_users() ->
	gen_server:call({global, ?MODULE}, list_all_users).
	
%% Obter toda a información dun usuario
%%
%% Returns: #user_info | unknown_user
get_user_info(Login) ->
	gen_server:call({global, ?MODULE}, {get_user_info, Login}).
	
%% Enviar unha mesaxe a un usuario
send_message(FromLogin, ToLogin, Message) ->
	gen_server:call({global, ?MODULE}, {send_message, FromLogin, ToLogin, Message}).

%% Facer login no servidor. O proceso actual pasará a ser
%% o proceso asociado a este usuario, e recibirá eventos e 
%% mesaxes 
login(Login, Alias, Pid) ->
	gen_server:call({global, ?MODULE}, {login, Login, Alias, Pid}).

%% Facer logout no servidor
%% Returns: ok
logout(Login) ->
	gen_server:call({global, ?MODULE}, {logout, Login}).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Buscar todas as tarefas
% Returns: [#task_info]	
find_all_tasks() ->
	gen_server:call({global, ?MODULE}, find_all_tasks).

% Buscar tarefa por id
% Returns: not_found | #task_info	
find_task_by_id(Id) ->
	gen_server:call({global, ?MODULE}, {find_task_by_id, Id}).

% Buscar tarefas asignadas para determinado login
% Returns: [#task_info]
find_task_by_assigned(Login) ->
	gen_server:call({global, ?MODULE}, {find_task_by_assigned, Login}).

% Crear tarefa
% Returns: ok | duplicated
create_task(Task) ->
gen_server:call({global, ?MODULE}, {create_task, Task}).

% Actualizar tarefa
% Returns: ok | not_found
update_task(Task) ->
gen_server:call({global, ?MODULE}, {update_task, Task}).

% Borrar tarefa
% Returns: ok
remove_task(Id) ->
gen_server:call({global, ?MODULE}, {remove_task, Id}).

% buscar todas as relacións entre tarefas
% Returns: [{Id1, Id2}]
find_all_relations() ->
gen_server:call({global, ?MODULE}, find_all_relations).

% Busca todas as tarefas predecesoras a unha dada
% Returns: [Id]
find_predepends(Id) ->
gen_server:call({global, ?MODULE}, {find_predepends, Id}).

% Busca todas as tarefas sucesoras a unha dada
% Returns: [Id]
find_postdepends(Id) ->
gen_server:call({global, ?MODULE}, {find_postdepends, Id}).

% Agrega unha relacion
% Returns: ok | not_found 
add_relation(Id1, Id2) ->
gen_server:call({global, ?MODULE}, {add_relation, Id1, Id2}).

% Borra unha relacion
% Returns: ok 
remove_relation(Id1, Id2) ->
gen_server:call({global, ?MODULE}, {remove_relation, Id1, Id2}).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% private API |
%~~~~~~~~~~~~~'
notifyUsers(Message, Users) ->
	Pids = user_db:get_all_online_pids(Users),
	Fun = fun (Pid) -> Pid!Message end,
	lists:map(Fun, Pids).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% gen_server callbacks |
%~~~~~~~~~~~~~~~~~~~~~~'
init([]) ->
	{ok, #?MODULE{
			users = user_db:init_db(), 
			tasks =  task_db:init_db()
	}}.

%-----------------------------------------------------------------------------
% Control de usuarios
handle_call(list_all_users, _, State) ->
	{reply, user_db:find_all_users(State#?MODULE.users), State};
	
handle_call({get_user_info, Login},  _, State) ->
	{reply, user_db:get_user_info(Login, State#?MODULE.users), State}; 
	
handle_call({send_message, FromLogin, ToLogin, Message}, _, State) ->
	Reply = 
	case user_db:get_user_session(ToLogin, State#?MODULE.users) of 
		{_, Pid} ->
			Pid!{message, FromLogin, Message},
			ok;
		X -> X
	end,
	{reply, Reply, State};
		
handle_call({login, Login, Alias, ClientPid}, _, State) ->
	case user_db:set_online(Login, Alias, ClientPid, State#?MODULE.users) of
		{ok, NewUsers} -> 
			notifyUsers({user_online, Login}, NewUsers),
			{reply, ok, State#?MODULE{users = NewUsers}};
		Error -> 
			{reply, Error, State}
	end;
	
handle_call({logout, Login}, _, State) ->
	case user_db:set_offline(Login, State#?MODULE.users) of
		{ok, NewUsers} ->
			notifyUsers({user_offline, Login}, NewUsers),
			{reply, ok, State#?MODULE{users = NewUsers}};
		Error -> {reply, Error, State}
	end; 

%-----------------------------------------------------------------------------
% Control de tarefas
handle_call(find_all_tasks, _, State) ->
	{reply, task_db:find_all(State#?MODULE.tasks), State};

handle_call({find_task_by_id, Id}, _, State) ->
	{reply, task_db:find_by_id(Id, State#?MODULE.tasks), State};

handle_call({find_task_by_assigned, Login}, _, State) ->
	{reply, task_db:find_by_assigned(Login, State#?MODULE.tasks), State};

handle_call({create_task, Task}, _, State) ->
	case task_db:create(Task, State#?MODULE.tasks) of 
		{ok, NewTask, TaskDb} ->
			notifyUsers({task_created, NewTask}, State#?MODULE.users),
			{reply, NewTask, State#?MODULE{tasks = TaskDb}};
		Error -> 
			{reply, Error, State}
	end;

handle_call({update_task, Task}, _, State) ->
	case task_db:update(Task, State#?MODULE.tasks) of 
		{ok, NewTask, TaskDb} ->
			notifyUsers({task_updated, Task}, State#?MODULE.users),
			{reply, NewTask, State#?MODULE{tasks = TaskDb}};
		Error -> 
			{reply, Error, State}
	end;

handle_call({remove_task, Id}, _, State) ->
	TaskDb = task_db:remove(Id, State#?MODULE.tasks),
	notifyUsers({task_removed, TaskId}, State#?MODULE.users),
	{reply, ok, State#?MODULE{tasks = TaskDb}};

handle_call(find_all_relations, _, State) ->
	{reply, task_db:find_all_relations(State#?MODULE.tasks), State};

handle_call({find_predepends, Id}, _, State) ->
	{reply, task_db:find_predepends(Id, State#?MODULE.tasks), State}; 

handle_call({find_postdepends, Id}, _, State) ->
	{reply, task_db:find_postdepends(Id, State#?MODULE.tasks), State}; 

handle_call({add_relation, Id1, Id2}, _, State) ->
	case task_db:add_relation(Id1, Id2, State#?MODULE.tasks) of 
		{ok, TaskDb} ->
			{reply, ok, State#?MODULE{tasks = TaskDb}};
		Error -> 
			{reply, Error, State}
	end;

handle_call({remove_relation, Id1, Id2}, _, State) ->
	case task_db:remove_relation(Id1, Id2, State#?MODULE.tasks) of 
		{ok, TaskDb} ->
			{reply, ok, State#?MODULE{tasks = TaskDb}};
		Error -> 
			{reply, Error, State}
	end;

% default call handler
handle_call(_Msg, _, State) ->
	{reply, {error, {error, unknown_message}}, State}. 

handle_cast(_Msg, State) ->
	{noreply, State}. 

terminate(_Reason, _State) -> 
	% TODO: notificar clientes
	ok.

handle_info(_Msg, State) -> 
	{noreply, State}.

code_change(_, State, _) -> {ok, State}.




	

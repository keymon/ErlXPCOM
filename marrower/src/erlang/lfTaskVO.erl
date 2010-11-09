%%
%% Implementación do interfaz lfITaskVO
%% VO da información de tarea
%% 
-module(lfTaskVO).

% xpcom_object exports
-export([init/1, terminate/2, xpcom_object_info/3, query_interface/3,
		method/4, set_attribute/4, get_attribute/3, info/2]).

% public exports
-export([create/1, create/0, get_task/1]).

-behaviour(xpcom_object).

-include("task_db.hrl").
-include("lfITaskVO.hrl").

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Public API |
%~~~~~~~~~~~~'
create(Task) -> 
	xpcom_object:create(?MODULE, [Task]).

create() -> 
	xpcom_object:create(?MODULE, []).

get_task(TaskVO) ->
	{ok, [Task]} = xpcom_object:call_method(TaskVO, get_task, []),
	Task.
	
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% xpcom_object callbacks |
%~~~~~~~~~~~~~~~~~~~~~~~~'
init([]) ->
	{ok, #task_info{}};

init([Task]) ->
	{ok, Task}.

xpcom_object_info(iids, _Params, _State) ->
	[?LFITASKVO_IID].   
	
query_interface(_IID, _This, State) ->
	{no_interface, State}.

terminate(_Reason, _State) -> 
	ok.
		
int_of_state(State) ->
	case State of 
		stoped -> ?STOPED;
		in_progress -> ?INPROGRESS;
		completed -> ?COMPLETED;
		canceled -> ?CANCELED
	end.
state_of_int(State) ->
	case State of 
		?STOPED -> stoped;
		?INPROGRESS -> in_progress;
		?COMPLETED -> completed;
		?CANCELED -> canceled
	end.
int_of_priority(State) ->
	case State of 
		high -> ?HIGH;
		medium -> ?MEDIUM;
		low -> ?LOW
	end.
priority_of_int(State) ->
	case State of 
		?HIGH -> high;
		?MEDIUM -> medium;
		?LOW -> low 
	end.
			
%~~~~~~~~~~
method(init, [Id, Name, Description, Duration, 
			  State, Completion, Priority], _, Task) ->
	NewTask = 
		Task#task_info{id = Id, name = Name, description = Description,
			           duration = Duration, 
					   state = state_of_int(State), completion = Completion, 
					   priority = priority_of_int(Priority), assigned_to = []},
	{reply, {ok, []}, NewTask};

method(get_task, [], _, Task) ->
	{reply, {ok, [Task]}, Task};

method(getAssignedTo, [], _, Task) ->
	{reply, {ok, [Task#task_info.assigned_to]}, Task};

method(Method, _, _, State) ->
	{reply, {error, {unknown_method, Method}}, State}.

get_attribute(id,  _, Task) ->
	{reply, {ok, Task#task_info.id}, Task};

get_attribute(name,  _, Task) ->
	{reply, {ok, Task#task_info.name}, Task};

get_attribute(description,  _, Task) ->
	{reply, {ok, Task#task_info.description}, Task};

get_attribute(duration,  _, Task) ->
	{reply, {ok, Task#task_info.duration}, Task};

get_attribute(state,  _, Task) ->
	Value = int_of_state(Task#task_info.state),
	{reply, {ok, Value}, Task};

get_attribute(completion,  _, Task) ->
	{reply, {ok, Task#task_info.completion}, Task};

get_attribute(priority,  _, Task) ->
	Value = int_of_priority(Task#task_info.priority),
	{reply, {ok, Value}, Task};

get_attribute(Attribute,  _, State) ->
	{reply, {error, {unknown_method, Attribute}}, State}.

set_attribute(id, Value, _, Task) ->
	{reply, ok, Task#task_info{id = Value}};

set_attribute(name, Value, _, Task) ->
	{reply, ok, Task#task_info{name = Value}};

set_attribute(description, Value, _, Task) ->
	{reply, ok, Task#task_info{description = Value}};

set_attribute(duration, Value, _, Task) ->
	{reply, ok, Task#task_info{duration = Value}};

set_attribute(state, ?STOPED, _, Task) ->
	{reply, ok, Task#task_info{state = stoped}};
set_attribute(state, ?INPROGRESS, _, Task) ->
	{reply, ok, Task#task_info{state = in_progress}};
set_attribute(state, ?COMPLETED, _, Task) ->
	{reply, ok, Task#task_info{state = completed}};
set_attribute(state, ?CANCELED, _, Task) ->
	{reply, ok, Task#task_info{state = canceled}};

set_attribute(completion, Value, _, Task) ->
	{reply, ok, Task#task_info{completion = Value}};

set_attribute(priority, ?STOPED, _, Task) ->
	{reply, ok, Task#task_info{priority = high}};
set_attribute(priority, ?INPROGRESS, _, Task) ->
	{reply, ok, Task#task_info{priority = medium}};
set_attribute(priority, ?COMPLETED, _, Task) ->
	{reply, ok, Task#task_info{priority = low}};

set_attribute(Attribute, _Value, _, State) ->
	{reply, {error, {unknown_method, Attribute}}, State}.

info(_Msg, State) -> {no_reply, State}.

								



% 
% Xestión de tarefas
%
-module(task_db).

-compile(export_all).

-include("task_db.hrl").
-include("default_task_db.hrl").

init_db() ->
	get_default_tasks().

% o propio diccionario terá un contador de identificadores
new_id(Db) ->
	{ok, Counter} = dict:find(counter_id, Db),
	{Counter, dict:update_counter(counter_id, 1, Db)}.

find_all(Db) ->
	Fun = fun 
		(Key, X, Acum) when is_integer(Key)  -> [X|Acum];
		(_, _, Acum)  -> Acum
	end,
	dict:fold(Fun, [], Db).

exists(Id, Db) ->
	dict:is_key(Id, Db).

find_by_id(Id, Db) ->
	case dict:find(Id, Db) of
		{ok, Task} -> Task;
		_ -> not_found
	end.

find_by_assigned(Login, Db) ->
	Fun = fun (Key, Task, Acum) when is_integer(Key) ->
			case lists:member(Login, Task#task_info.assigned_to) of 
				true -> [Task|Acum];
				false -> Acum
			end;
			(_, _, Acum) -> Acum
		end,
	dict:fold(Fun, [], Db).
	
create(Task, Db) ->
	% creamos un novo Id
	{Id, Db2} = new_id(Db), 
	NewTask = Task#task_info{id=Id}, 
	Db3 = dict:store(Id, NewTask, Db2),
	{ok, NewTask, Db3}.

update(Task, Db) ->
	case exists(Task#task_info.id, Db) of
		true -> 
			Db2 = dict:store(Task#task_info.id, Task, Db),
			{ok, Task, Db2};
		false -> 
			not_found
	end.

remove(Id, Db) ->
	dict:erase(Id, Db).

get_relations(Db) ->
	case dict:find(relations, Db) of
		{ok, Value} -> Value;
		_ -> sets:new()
	end.

find_all_relations(Db) ->
	sets:to_list(get_relations(Db)).

find_predepends(Id, Db) ->
	Relations = find_all_relations(Db), 
	Fun = fun({Id1, Id2}, Acum) -> 
		case Id2 of Id -> [Id1|Acum]; _ -> Acum end
	end, 
	lists:foldr(Fun, [], Relations).

find_postdepends(Id, Db) ->
	Relations = find_all_relations(Db), 
	Fun = fun({Id1, Id2}, Acum) -> 
		case Id1 of Id -> [Id2|Acum]; _ -> Acum end
	end, 
	lists:foldr(Fun, [], Relations).
	
add_relation(Id1, Id2, Db) ->
	Relations = get_relations(Db), 
	case {exists(Id1, Db),exists(Id2, Db)} of 
		{true, true} ->
			Db2 = dict:store(relations, sets:add_element({Id1, Id2},Relations), Db),
			{ok, Db2};
		_ -> 
			not_found
	end.

remove_relation(Id1, Id2, Db) ->
	Relations = get_relations(Db),
	Db2 = dict:store(relations, sets:del_element({Id1, Id2}, Relations), Db),
	{ok, Db2}.
	

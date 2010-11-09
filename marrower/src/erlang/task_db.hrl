%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% Rexistro coa información dunha tarefa
-record(task_info, {
		id = 0,           % identificador da tarefa
		name = "",         % nome da tarea
		description = "",  % descripción da tarea
		duration = [],     % duración estimada (string)
		state = 0,        % Estado: stoped | in_progress | completed | canceled
		completion = 0,   % % de compleción
		priority = 0,     % Prioridade: high | medium | low
		assigned_to =[]  % login do usuario ó que está asignado
		}).


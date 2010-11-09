%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% Rexistro coa información dunha tarefa
%-record(task_info, {
%		id,           % identificador da tarefa
%		name,         % nome da tarea
%		description,  % descripción da tarea
%		duration,     % duración estimada (string)
%		state,        % Estado: stoped | in_progress | completed | canceled
%		completion,   % % de compleción
%		priority,     % Prioridade: high | medium | low
%		assigned_to   % login do usuario ó que está asignado
%		}).


get_default_tasks() ->
	PopulateFun = fun(Task, D) ->
		dict:store(Task#task_info.id, Task, D)
	end,
	Db = lists:foldl(PopulateFun, dict:new(), task_list()), 
	Db2 = dict:store(counter_id, 26, Db), 
	Db3 = dict:store(relations, sets:from_list(depends()), Db2),
	Db3. 
	
depends() -> 
	[{1, 3}, {1, 4}, {4, 5}, {3,10}, 
	{6, 7}, {8, 9}, {8, 10}, {9, 11}, {10, 11}, {8, 12},     
	{12, 13}, {13, 14}, {13, 15},
	{15, 16}].

task_list() -> 
	[

	#task_info{
		id = 0, 
		name = "Formas de pago �rea 1.",
		description = "Revisi�n dos resultados das ventas por cotas mensuais do 2004.",
		duration = "10h",
		state = in_progress, 
		completion = 20, 
		priority = medium,
		assigned_to = ["sofia"]
		}, 
	
	#task_info{
		id = 2, 
		name = "Contrataci�n staff avogados",
		description = "Realizar�se a elecci�n entre os bufetes seleccionados na reuni�n previa.",
		duration = "2h",
		state = completed, 
		completion = 100, 
		priority = high,
		assigned_to = ["brais"]
		}, 
	#task_info{
		id = 3, 
		name = "Entrevista posto director/a de mantemento de hardware.",
		description = "Entrevista personalizada a cargo de Luisa Ramos (Dir. t�cnica) . ",
		duration = "1:30 h",
		state = completed, 
		completion = 100, 
		priority = high,
		assigned_to = ["luisa"]
		}, 
	#task_info{
		id = 4, 
		name = "Modificaci�n convenio traballadores",
		description = "Necesario acordo sobre contratos labarais.  ",
		duration = "10h",
		state = in_progress, 
		completion = 34, 
		priority = high,
		assigned_to = ["carlos"]
		}, 
	#task_info{
		id = 5, 
		name = "Visita Central A Coru�a - Juan Fl�rez 24",
		description = "Control de eficiencia do persoal.",
		duration = "8h",
		state = canceled, 
		completion = 89, 
		priority = high,
		assigned_to = ["chema"]
		}, 
	#task_info{
		id = 6, 
		name = "Visita Central Vigo - Calvo Sotelo 198 ",
		description = "Control de calidade do servicio t�cnico.",
		duration = "8h",
		state = stoped, 
		completion = 24, 
		priority = low,
		assigned_to = ["carlos", "brais", "chema"]
		}, 
	#task_info{
		id = 7, 
		name = "Reuni�n co xefe de sincicatos da Comunidade de Madrid",
		description = "Necesario mantelo calado os pr�ximos 2 meses e medio.",
		duration = "4h",
		state = completed, 
		completion = 100, 
		priority = high,
		assigned_to = ["jose"]
		}, 
	#task_info{
		id = 8, 
		name = "Estratexia comercial ver�n.",
		description = "Definir estratexia, presuposto campa�a.",
		duration = "20h",
		state = completed, 
		completion = 100, 
		priority = high,
		assigned_to = ["maria", "jose"]
		}, 
	#task_info{
		id = 9, 
		name = "Revisi�n �rea 2",
		description = "Mantemento e coidado hardware en toda a �rea.",
		duration = "10h",
		state = in_progress, 
		completion = 75, 
		priority = medium,
		assigned_to = ["chema", "jose"]
		}, 

	#task_info{
		id = 10, 
		name = "Revisar informe de vendas 2004",
		description = "D�bese revisar o informe de vendas do 2004 e comprobar que corresponde cos gananciales deste ano",
		duration = "20h",
		state = stoped, 
		completion = 0, 
		priority = low,
		assigned_to = []
		}, 

		
	#task_info{
		id = 11, 
		name = "Campa�a primavera 2005 en t�dalas �reas.",
		description = "Mostra dos resultados.",
		duration = "5h",
		state = completed, 
		completion = 100, 
		priority = high,
		assigned_to = ["pedro"]
		}, 
	#task_info{
		id = 12, 
		name = "Fin contratos Central Burgos - Angel Gracia, 87.",
		description = " Definir remates en funci�n das reuni�ns previas. ",
		duration = "10h",
		state = stoped, 
		completion = 0, 
		priority = high,
		assigned_to = []
		}, 
	#task_info{
		id = 13, 
		name = "Empresa contrataci�n persoal - Central Lugo. ",
		description = "Exposici�n dos controis de eficiencia do ano 2004. ",
		duration = "2h",
		state = completed, 
		completion = 100, 
		priority = medium,
		assigned_to = ["victor", "patricia", "brais"]
		}, 
	#task_info{
		id = 14, 
		name = "Reuni�n Presidente Codisa.",
		description = "Acordo de promoci�ns para campa�a inverno.",
		duration = "6h",
		state = completed, 
		completion = 100, 
		priority = high,
		assigned_to = ["patricia"]
		}, 
	#task_info{
		id = 15, 
		name = "Compra equipos Central A Coru�a - Juan Fl�rez 24 ",
		description = "Renovaci�n de 40 PC's. ",
		duration = "3h",
		state = in_progress, 
		completion = 23, 
		priority = high,
		assigned_to = ["brais"]
		}, 
	#task_info{
		id = 16, 
		name = "Reuni�n Director ETT �rea 2.",
		description = "Acordo sobre o perfil dos candidatos. ",
		duration = "2h",
		state = in_progress, 
		completion = 22, 
		priority = medium,
		assigned_to = ["chema"]
		}
	].


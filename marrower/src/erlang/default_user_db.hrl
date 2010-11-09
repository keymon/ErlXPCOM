%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% Rexistro coa informaciÃ³n de usuario
%-record (user_info, {
%		login,      % identificador de usuario 
%		alias,      % nick ou alias de usuario
%		state,      % estado do usuario: online | offline 
%		pid,        % Proceso do cliente se online
%		firstname,   
%		lastname,    
%		email,
%		address, 
%		birthdate 
%		}).


% 
% Hardcoded users
%
registered_users_list() -> [ 
	#user_info{
		login = "carlos",
		alias = "Carlitos",
		state = offline,
		firstname = "Carlos", 
		lastname = "Iglesias", 
		email = "carlos@lfcia.org",
		address = "Portela 15 3ºD",
		birthdate = "8/7/1980" }, 
	#user_info{
		login = "pedro",
		alias = "PeterMan",
		state = offline,
		firstname = "Pedro", 
		lastname = "Suarez", 
		email = "pedro@lfcia.org",
		address = "Av. Alfonso X 22 1ºD",
		birthdate = "2/2/1977" }, 
	#user_info{
		login = "maria",
		alias = "Amatista",
		state = offline,
		firstname = "Maria", 
		lastname = "Lopez", 
		email = "maria@lfcia.org",
		address = "Arteixo 123 9ºC",
		birthdate = "2/12/1978" }, 
	#user_info{
		login = "victor",
		alias = "DarkVader",
		state = offline,
		firstname = "Vitor", 
		lastname = "Gutierrez", 
		email = "victor@lfcia.org",
		address = "Alcorcon 44 5ºA",
		birthdate = "1/6/1974" }, 
	#user_info{
		login = "jose",
		alias = "J-Max",
		state = offline,
		firstname = "Jose", 
		lastname = "Martinez", 
		email = "jose@lfcia.org",
		address = "Angel Senra 12 Baixo",
		birthdate = "12/2/1982" }, 
	#user_info{
		login = "chema",
		alias = "Chema",
		state = offline,
		firstname = "Jose Maria", 
		lastname = "Albano", 
		email = "jma@lfcia.org",
		address = "Catro caminos 4, 5ºIzq",
		birthdate = "4/12/1983" }, 
	#user_info{
		login = "sofia",
		alias = "Small-a",
		state = offline,
		firstname = "Sofia", 
		lastname = "Penas", 
		email = "sofia@lfcia.org",
		address = "Teruel 112 3ºD",
		birthdate = "22/8/1970" },
	#user_info{
		login = "brais",
		alias = "chimpo",
		state = offline,
		firstname = "Brais", 
		lastname = "Argivay", 
		email = "brachi@teleline.es",
		address = "Ronda de Outeiro,13,5ºD",
		birthdate = "12/2/1975" },
    #user_info{
		login = "luisa",
		alias = "tini",
		state = offline,
		firstname = "Luísa", 
		lastname = "Díaz", 
		email = "luchi@lfcia.org",
		address = "Calvo Sotelo, 35, 1ºA",
		birthdate = "20/9/1973" },
	#user_info{
		login = "patricia",
		alias = "nena",
		state = offline,
		firstname = "Patricia", 
		lastname = "Someso", 
		email = "nena@metalicana.org",
		address = "Manuel Azaña, 56, 5ºB",
		birthdate = "15/7/1980" },
	#user_info{
		login = "iago",
		alias = "hiper",
		state = offline,
		firstname = "Iago", 
		lastname = "Orosa", 
		email = "iiper@hotmail.com",
		address = "Rio Sil, 3,3ºG",
		birthdate = "18/9/1968" },
	#user_info{
		login = "isa",
		alias = "peque",
		state = offline,
		firstname = "María Isabel", 
		lastname = "Pregigueiro", 
		email = "isinha@metalicana.org",
		address = "Brues s/n",
		birthdate = "22/7/1973" }	
].
		
% Crea o diccionario inicial
get_registered_users() ->
	PopulateFun = fun(User, D) ->
		dict:store(User#user_info.login, User, D)
	end,
	lists:foldl(PopulateFun, dict:new(), registered_users_list()).
	
	
		

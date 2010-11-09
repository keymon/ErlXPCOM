%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% Rexistro coa información de usuario
-record (user_info, {
		login,      % identificador de usuario 
		alias,      % nick ou alias de usuario
		state,      % estado do usuario: online | offline 
		pid,        % Proceso do cliente se online
		firstname,   
		lastname,    
		email,
		address, 
		birthdate 
		}).


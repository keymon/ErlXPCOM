% 
% Compoñente XPCOM marrower. 
% Este é o punto de entrada do compoñente XPCOM Erlang
%
% CID: 5c4e36cc-898f-4912-b85c-e78e94993ca7
% contractId: @lfcia.org/erlang/marrower;1
%
%
% 
-module(marrower_component).

% XPCOM Component exports
-export([ns_get_module/0]).

-include("erlxpcom.hrl").

-include ("lfIMarrower.hrl").
-include ("lfITaskVO.hrl").

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-define(LFIMARROWER_CID, ?LFIMARROWER_IID).
-define(LFITASKVO_CID, ?LFITASKVO_IID).
-define(LFIMARROWER_CONTRACTID, "@lfcia.org/marrower;1").
-define(LFITASKVO_CONTRACTID, "@lfcia.org/marrower/taskVO;1").

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Generic module  |
%~~~~~~~~~~~~~~~~~'
nsmodule_info() ->
	#ns_module_info{
		version = "0.0.1",
		moduleName = "Marrower: Xestion de tareas", 
		components = [
			#ns_module_component_info{
				description = "Marrower facade",
				cid = ?LFIMARROWER_CID,
				contractID = ?LFIMARROWER_CONTRACTID, 
				constructor = 
					?NS_GENERIC_FACTORY_CONSTRUCTOR_FUNCTION(lfMarrower, create)
			}, 
			#ns_module_component_info{
				description = "Task VO",
				cid = ?LFITASKVO_CID,
				contractID = ?LFITASKVO_CONTRACTID, 
				constructor = 
					?NS_GENERIC_FACTORY_CONSTRUCTOR_FUNCTION(lfTaskVO, create)
			}
			]
	}.

ns_get_module() ->
	ns_generic_module:create_and_query(nsmodule_info()).


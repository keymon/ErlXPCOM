%%
%% Implementación do interfaz lfIUserVO
%% 
-module(lfUserVO).

% xpcom_object exports
-export([init/1, terminate/2, xpcom_object_info/3, query_interface/3,
		method/4, set_attribute/4, get_attribute/3, info/2]).

% public exports
-export([create/1]).

-behaviour(xpcom_object).

-include("user_db.hrl").
-include("lfIUserVO.hrl").


%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Public API |
%~~~~~~~~~~~~'
create(User) -> 
	xpcom_object:create(?MODULE, [User]).


%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% xpcom_object callbacks |
%~~~~~~~~~~~~~~~~~~~~~~~~'
init([User]) ->
	{ok, User}.

xpcom_object_info(iids, _Params, _State) ->
	[?LFIUSERVO_IID].   
	
query_interface(_IID, _This, State) ->
	{no_interface, State}.

terminate(_Reason, _State) -> 
	ok.
		
%~~~~~~~~~~
method(Method, _, _, State) ->
	{reply, {error, {unknown_method, Method}}, State}.

get_attribute(state,  _, User) ->
	Value = 
	case User#user_info.state of 
		online -> ?ONLINE;
		offline -> ?OFFLINE
	end, 
	{reply, {ok, Value}, User};
	
get_attribute(login,  _, User) ->
	{reply, {ok, User#user_info.login}, User};

get_attribute(alias,  _, User) ->
	{reply, {ok, User#user_info.alias}, User};

get_attribute(firstname,  _, User) ->
	{reply, {ok, User#user_info.firstname}, User};

get_attribute(lastname,  _, User) ->
	{reply, {ok, User#user_info.lastname}, User};

get_attribute(birthdate,  _, Task) ->
	{reply, {ok, Task#user_info.birthdate}, Task};

get_attribute(email,  _, Task) ->
	{reply, {ok, Task#user_info.email}, Task};

get_attribute(Attribute,  _, State) ->
	{reply, {error, {unknown_method, Attribute}}, State}.

set_attribute(Attribute, _Value, _, State) ->
	{reply, {error, {unknown_method, Attribute}}, State}.

info(_Msg, State) -> {no_reply, State}.

								



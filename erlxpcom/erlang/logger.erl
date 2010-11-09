%% 
%% Logger server an some primitives
%%
-module(logger).

-behaviour(gen_server).

-include("logger.hrl").

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% genserver exports
-export([init/1, handle_call/3, handle_cast/2, 
		 handle_info/2, terminate/2, code_change/3]).

% Public exports
-export([start/1, stop/0,
		 change_options/1, define_tag/2,  
		 log_msg/4, time_str/0]).

% Server name to use for registration
-define(LOGGER_SERVER_NAME, ?MODULE).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% Log tag entry 
-record(log_tag, {
			tag,                   % tag name
			string,                % string that will be printed in log entries
			level = ?LOGGER_NONE,  % level for this tag
			destination = default  % destination for this tag 
			}).

%% State of logger server			
-record(logger_state, {
			all_level = ?LOGGER_NONE,          % Default level
			defined_tags = dict:new(),         % List of defined tags
			default_destination = standard_io  % default destination
			}).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Public API |
%~~~~~~~~~~~~'

%% Start the logger server with given options
start(Options) ->
	case gen_server:start({local, ?LOGGER_SERVER_NAME}, ?MODULE, 
						  [Options], []) of
		{ok, _} -> ok;
		{error, {already_started, _}} -> ok;
		Other -> Other
	end.

%% Stop the logger server with given level. This call never fails
stop() ->
	gen_server:cast(?LOGGER_SERVER_NAME, stop).

%% Change the options of the server
change_options(Options) ->
	gen_server:cast(?LOGGER_SERVER_NAME, {change_options, Options}).

%%	
%% Define the given tag with the given options. 
%% If tag exists, update it with options.
%% This call never fails.
%% @spec define_tag(Tag, String, Destination) -> ok
%%		TagName = atom()      - tag which identifies the message
%%      Options = [Option]
%%       Option = {string, String} | {level, Level} | {destination, Destination}
%%        Destination = default | {file, FilePath} | {ioDevice, IoDevice}
%%
define_tag(TagName, Options) ->
	gen_server:cast(?LOGGER_SERVER_NAME, {define_tag, TagName, Options}).

%%
%% Log the given message (ala io:format) for the given tag an level 
%%
log_msg(Tag, Level, Msg, Args) ->
	gen_server:cast(?LOGGER_SERVER_NAME, {log_msg, Tag, Level, Msg, Args}).

% Get string representation of current time
time_str() ->
	{Hour, Minute, Second} = time(), 
	{_,_, Microsecs} = now(),
	MiliSecs = Microsecs div 1000,
	io_lib:format("~2.2.0w:~2.2.0w:~2.2.0w.~3.3.0w", [Hour,Minute,Second,MiliSecs]).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% Private API |
%~~~~~~~~~~~~~'

% Check destination and opening/closing files if necesary.
open_destination({file, Path}, OldDestination, Default) -> 
	close_destination(OldDestination),
	case file:open(Path, [write, append]) of
		{ok, IoDevice} -> {ioDevice, IoDevice};
		{error, Reason} -> 
			error_logger:error_msg(
			"logger: Can't open file ~w: ~w", [Path, Reason]),
			Default
	end;
open_destination(default, OldDestination, Default) -> 
	close_destination(OldDestination),
	Default;
open_destination(Destination, OldDestination, _) -> 
	close_destination(OldDestination),
	Destination.

% Close destination if necesary
close_destination({ioDevice, IoDevice}) ->
	catch file:close(IoDevice);
close_destination(_) -> true.
	
% Create the server state from the option list
create_server_state(Options) -> update_state(#logger_state{}, Options).

update_state(State, [{level, Level} | T]) ->
	update_state(State#logger_state{all_level = Level}, T);
update_state(State, [{destination, Destination} | T]) ->
	Destination2 = open_destination(Destination, 
						State#logger_state.default_destination, 
						standard_io),
	update_state(State#logger_state{default_destination = Destination2}, T);
update_state(State, [Bad|T]) -> 
	error_logger:error_msg("logger: unknown option: ~w", [Bad]),
	update_state(State, T);
update_state(State, []) -> State;
update_state(State, Bad) -> 
	error_logger:error_msg("logger: Bad logger options format: ~w", [Bad]),
	State.
	

% Create a new log_tag 
create_tag(TagName, Options) ->
	update_tag(#log_tag{tag=TagName, string=atom_to_list(TagName)}, Options).
	
% update a tag  setting the apropiate parameters
update_tag(Tag, [{string, String}|T]) ->
	update_tag(Tag#log_tag{string = String}, T);
update_tag(Tag, [{level, Level}|T]) -> 
	update_tag(Tag#log_tag{level = Level}, T);
update_tag(Tag, [{destination, Destination}|T]) -> 
	Destination2 = open_destination(Destination, Tag#log_tag.destination, default),
	update_tag(Tag#log_tag{destination = Destination2}, T);
update_tag(Tag, [Bad|T]) ->
	error_logger:error_msg("logger: unknown tag option: ~w", [Bad]),
	update_tag(Tag, T);
update_tag(Tag, []) -> Tag;
update_tag(Tag, Bad) -> 
	error_logger:error_msg("logger: Bad tag options for tag ~s: ~w", [Tag, Bad]),
	Tag.

do_print_entry(Destination, DefaultDestination, HeadString, Msg, Args) ->
	RealDestination = case Destination of 
		default -> case DefaultDestination of 
					{ioDevice, IoDevice} -> IoDevice;
					Y -> Y
				   end;
		{ioDevice, IoDevice} -> IoDevice;
		X -> X
	end, 
	case catch io:format(RealDestination, "["++HeadString++"] "++Msg++"~n", Args) of
		ok -> true;
		{'EXIT', {badarg, [{io, format, [_,Msg2, [_Time, _Module, File, Line|_]]}|_]}} ->
			ErrMsg = "logger: Bad log format in ~s:~w~n  Msg='~s' ~n  Args=~w~n",
			error_logger:error_msg(ErrMsg, [File, Line, Msg2, Args]);
		Other -> 
			error_logger:error_msg("logger: Error printing log: ~w", [Other]),
			error_logger:error_msg("logger: "++Msg, Args),
			true
	end.

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% gen_server callbacks |
%~~~~~~~~~~~~~~~~~~~~~~'

init([Options]) ->
	{ok, create_server_state(Options)}.

% default call handler
handle_call(Msg, _, State) ->
	?LOG_MSG(erlxpcom, ?LOGGER_ERROR, 
		"erlxpcom_object: Unknown call message received: ~w", [Msg]),
	{reply, {error, {error, unknown_message}}, State}. 

handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast({change_options, Options}, State) ->
	NewState = update_state(State, Options),
	{noreply, NewState};

handle_cast({define_tag, TagName, Options}, State) ->
	Dict = State#logger_state.defined_tags,
	F = fun ([Tag|_]) -> [update_tag(Tag, Options)] end,
	case (catch dict:update(TagName, F, Dict)) of
		{'EXIT', _} ->
			case (catch dict:append(TagName, create_tag(TagName, Options), Dict)) of
				{'EXIT', Reason} ->
					error_logger:error_msg(
						"logger: Error setting tag: ~w", [Reason]),
					{noreply, State};
				NewDict ->
					{noreply, State#logger_state{defined_tags = NewDict}}
			end;
		NewDict ->
			{noreply, State#logger_state{defined_tags = NewDict}}
	end;

handle_cast({log_msg, TagName, Level, Msg, Args}, State) ->
	case dict:find(TagName, State#logger_state.defined_tags) of
		{ok, [LogTag|_]} ->
			case (Level =< State#logger_state.all_level) 
					orelse (Level =< LogTag#log_tag.level) of
				true -> 
					do_print_entry(LogTag#log_tag.destination, 
								   State#logger_state.default_destination,
								   LogTag#log_tag.string,
								   Msg, Args);
				_ -> true
			end;
		_ ->
			if Level =< State#logger_state.all_level ->
					do_print_entry(State#logger_state.default_destination,
								   State#logger_state.default_destination,
								   "UNKNOWN(~s)",
								   Msg, [TagName|Args]);
				true -> true
			end
	end, 
	{noreply, State};
	
% default cast handler
handle_cast(Msg, State) ->
	?LOG_MSG(erlxpcom, ?LOGGER_ERROR, 
		"logger: Unknown cast: ~w", [Msg]),
	{noreply, State}. 
		
terminate(Reason, _) -> Reason.

handle_info(_, State) -> {noreply, State}.

code_change(_, State, _) -> {ok, State}.


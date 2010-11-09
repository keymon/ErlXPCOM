
-define(LOGGER_NONE, 0). 
-define(LOGGER_ERROR, 1). 
-define(LOGGER_WARNING, 2). 
-define(LOGGER_INFO, 3). 
-define(LOGGER_DEBUG, 4).

-ifdef(LOGGER).
%% Evaluate expresion if in logger is activated
-define(IFLOG(Expr), Expr).
%% Start the logger
-define(LOG_START(), 
	logger:start()).
%% Start the logger with options:
%%	Options = [Option]
%%	 Option = {level, Level} | {destination, Destination}
-define(LOG_START_OPTIONS(Options), 
	logger:start(Options)). 
%% Change logger options
-define(LOG_CHANGE_OPTIONS(Options), 
	logger:change_options(Options)). 
%% Stop the logger
-define(LOG_STOP(), 
	logger:stop()).
%% Define a new tag with given options. If tag exists, update options
%%	Options = [Option]
%%	 Option = {string, String} | {level, Level} | {destination, Destination}
-define(LOG_DEFINE_TAG(Tag, Options),
	logger:define_tag(Tag, Options)).
%% Log a term
-define(LOG(Tag, Level, Term), 
	logger:log_msg(Tag, Level, "~s - ~s(~s:~w):~n  ", 
		[logger:time_str(),?MODULE_STRING,?FILE,?LINE|Term])).
%% Log a formated message ala io:format()
-define(LOG_MSG(Tag, Level, Msg, Args), 
	logger:log_msg(Tag, Level , "~s - ~s(~s:~w):~n  "++Msg, 
		[logger:time_str(),?MODULE_STRING,?FILE,?LINE|Args])).

-else.
-define(IFLOG(Expr), true).
-define(LOG_START(Level), true). 
-define(LOG_START_OPTIONS(Options), true). 
-define(LOG_START_DEFAULTDEST(Level), true).
-define(LOG_START_LEVEL_DEFAULTDEST(Level, Destination), true). 
-define(LOG_CHANGE_OPTIONS(Options), true). 
-define(LOG_STOP(), true).
-define(LOG_DEFINE_TAG(Tag, String), true).
-define(LOG_DEFINE_TAG_IO(Tag, String, Destination), true).
-define(LOG_SET_LEVEL(Tag, Level), true).
-define(LOG(Tag, Level, Term), true).
-define(LOG_MSG(Tag, Level, Msg, Args), true).
-endif.





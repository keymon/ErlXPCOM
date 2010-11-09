-module(logger_test).

-export([test/0]).

-include("logger.hrl").

test() ->
	?LOG_START_OPTIONS([{level, ?LOGGER_ERROR}]),
	?LOG_DEFINE_TAG(test, [{string, "Testing Logger"},{level, ?LOGGER_INFO}]), 
	?LOG_DEFINE_TAG(test2, 
		[{string, "Testing Logger 2"}, {destination, {file, "/tmp/test.log"}}]), 
	?LOG_DEFINE_TAG(test2, [{level, ?LOGGER_DEBUG}]), 
	?LOG_MSG(test, ?LOGGER_INFO, "A info message ~w", [something]), 
	?LOG_MSG(test, ?LOGGER_WARNING, "A warning message ~w", [something]), 
	?LOG_MSG(test, ?LOGGER_ERROR, "A error message ~w", [something]), 
	?LOG_MSG(test, ?LOGGER_DEBUG, "A debug message ~w", [something]), 
	?LOG_MSG(test2, ?LOGGER_INFO, "A info message ~w", [something]), 
	?LOG_MSG(test2, ?LOGGER_WARNING, "A warning message ~w", [something,jeje]), 
	?LOG_MSG(test2, ?LOGGER_ERROR, "A error message ~w", [something]), 
	?LOG_MSG(test2, ?LOGGER_DEBUG, "A debug message ~w", [something]), 
	?LOG_DEFINE_TAG(test2, [{destination, {file, "/tmp/test2.log"}}]), 
	?LOG_MSG(test2, ?LOGGER_INFO, "A info message ~w", [something]), 
	?LOG_MSG(test2, ?LOGGER_WARNING, "A warning message ~w", [something]), 
	?LOG_MSG(test2, ?LOGGER_ERROR, "A error message ~w", [something]), 
	?LOG_MSG(test2, ?LOGGER_DEBUG, "A debug message ~w", [something]), 
	?LOG_CHANGE_OPTIONS([{destination, {file, "/tmp/test3.log"}}]), 
	?LOG_MSG(test, ?LOGGER_INFO, "A info message ~w", [something]), 
	?LOG_MSG(test, ?LOGGER_WARNING, "A warning message ~w", [something]), 
	?LOG_MSG(test, ?LOGGER_ERROR, "A error message ~w", [something]), 
	?LOG_MSG(test, ?LOGGER_DEBUG, "A debug message ~w", [something]), 
	true.
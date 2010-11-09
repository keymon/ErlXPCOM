% ***** BEGIN LICENSE BLOCK *****
% Version: MPL 1.1/GPL 2.0/LGPL 2.1
%
% The contents of this file are subject to the Mozilla Public License Version
% 1.1 (the "License"); you may not use this file except in compliance with
% the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Software distributed under the License is distributed on an "AS IS" basis,
% WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
% for the specific language governing rights and limitations under the
% License.
%
% The Original Code is mozilla.org code.
%
% The Initial Developer of the Original Code is
% Netscape Communications Corporation.
% Portions created by the Initial Developer are Copyright (C) 1998
% the Initial Developer. All Rights Reserved.
%
% Contributor(s):
%
% Alternatively, the contents of this file may be used under the terms of
% either of the GNU General Public License Version 2 or later (the "GPL"),
% or the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
% in which case the provisions of the GPL or the LGPL are applicable instead
% of those above. If you wish to allow use of your version of this file only
% under the terms of either the GPL or the LGPL, and not to allow others to
% use your version of this file under the terms of the MPL, indicate your
% decision by deleting the provisions above and replace them with the notice
% and other provisions required by the GPL or the LGPL. If you do not delete
% the provisions above, a recipient may use your version of this file under
% the terms of any one of the MPL, the GPL or the LGPL.
%
% ***** END LICENSE BLOCK ***** */
%% Mozilla error codes.
%%
%% THIS FILE GENERATED FROM mozilla/xpcom/base/nsError.h.
%% PLEASE SEE THAT FILE FOR FULL DOCUMENTATION.
%%/

-define(NS_ERROR_MODULE_XPCOM, 16#1).
-define(NS_ERROR_MODULE_BASE, 16#2).
-define(NS_ERROR_MODULE_GFX, 16#3).
-define(NS_ERROR_MODULE_WIDGET, 16#4).
-define(NS_ERROR_MODULE_CALENDAR, 16#5).
-define(NS_ERROR_MODULE_NETWORK, 16#6).
-define(NS_ERROR_MODULE_PLUGINS, 16#7).
-define(NS_ERROR_MODULE_LAYOUT, 16#8).
-define(NS_ERROR_MODULE_HTMLPARSER, 16#9).
-define(NS_ERROR_MODULE_RDF, 16#10).
-define(NS_ERROR_MODULE_UCONV, 16#11).
-define(NS_ERROR_MODULE_REG, 16#12).
-define(NS_ERROR_MODULE_FILES, 16#13).
-define(NS_ERROR_MODULE_DOM, 16#14).
-define(NS_ERROR_MODULE_IMGLIB, 16#15).
-define(NS_ERROR_MODULE_MAILNEWS, 16#16).
-define(NS_ERROR_MODULE_EDITOR, 16#17).
-define(NS_ERROR_MODULE_XPCONNECT, 16#18).
-define(NS_ERROR_MODULE_PROFILE, 16#19).
-define(NS_ERROR_MODULE_LDAP, 16#20).
-define(NS_ERROR_MODULE_SECURITY, 16#21).
-define(NS_ERROR_MODULE_DOM_XPATH, 16#22).
-define(NS_ERROR_MODULE_DOM_RANGE, 16#23).
-define(NS_ERROR_MODULE_URILOADER, 16#24).
-define(NS_ERROR_MODULE_CONTENT, 16#25).
-define(NS_ERROR_MODULE_PYXPCOM, 16#26).
-define(NS_ERROR_MODULE_XSLT, 16#27).

-define(NS_ERROR_MODULE_GENERAL, 16#51).

-define(NS_ERROR_SEVERITY_SUCCESS, 16#0).
-define(NS_ERROR_SEVERITY_ERROR, 16#1).

-define(NS_ERROR_MODULE_BASE_OFFSET, 16#45).

%  Standard "it worked" return value 
-define(NS_OK, 16#0).

-define(NS_ERROR_BASE, 16#C1F30000).

%  Returned when an instance is not initialized 
-define(NS_ERROR_NOT_INITIALIZED, ?NS_ERROR_BASE+16#1).

%  Returned when an instance is already initialized 
-define(NS_ERROR_ALREADY_INITIALIZED, ?NS_ERROR_BASE+16#2).

%  Returned by a not implemented function 
-define(NS_ERROR_NOT_IMPLEMENTED, 16#80004001).

%  Returned when a given interface is not supported. 
-define(NS_NOINTERFACE, 16#80004002).
-define(NS_ERROR_NO_INTERFACE, ?NS_NOINTERFACE).

-define(NS_ERROR_INVALID_POINTER, 16#80004003).
-define(NS_ERROR_NULL_POINTER, ?NS_ERROR_INVALID_POINTER).

%  Returned when a function aborts 
-define(NS_ERROR_ABORT, 16#80004004).

%  Returned when a function fails 
-define(NS_ERROR_FAILURE, 16#80004005).

%  Returned when an unexpected error occurs 
-define(NS_ERROR_UNEXPECTED, 16#8000ffff).

%  Returned when a memory allocation fails 
-define(NS_ERROR_OUT_OF_MEMORY, 16#8007000e).

%  Returned when an illegal value is passed 
-define(NS_ERROR_ILLEGAL_VALUE, 16#80070057).
-define(NS_ERROR_INVALID_ARG, ?NS_ERROR_ILLEGAL_VALUE).

%  Returned when a class doesn't allow aggregation 
-define(NS_ERROR_NO_AGGREGATION, 16#80040110).

%  Returned when an operation can't complete due to an unavailable resource 
-define(NS_ERROR_NOT_AVAILABLE, 16#80040111).

%  Returned when a class is not registered 
-define(NS_ERROR_FACTORY_NOT_REGISTERED, 16#80040154).

%  Returned when a class cannot be registered, but may be tried again later 
-define(NS_ERROR_FACTORY_REGISTER_AGAIN, 16#80040155).

%  Returned when a dynamically loaded factory couldn't be found 
-define(NS_ERROR_FACTORY_NOT_LOADED, 16#800401f8).

%  Returned when a factory doesn't support signatures 
-define(NS_ERROR_FACTORY_NO_SIGNATURE_SUPPORT, ?NS_ERROR_BASE+16#101).

%  Returned when a factory already is registered 
-define(NS_ERROR_FACTORY_EXISTS, ?NS_ERROR_BASE+16#100).

-define(NS_ERROR_PROXY_INVALID_IN_PARAMETER, 16#80010010).

-define(NS_ERROR_PROXY_INVALID_OUT_PARAMETER, 16#80010011).

%  I/O Errors 

%   Stream closed 
-define(NS_BASE_STREAM_CLOSED, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_BASE+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 2)).
%   Error from the operating system 
-define(NS_BASE_STREAM_OSERROR, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_BASE+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 3)).
%   Illegal arguments 
-define(NS_BASE_STREAM_ILLEGAL_ARGS, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_BASE+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 4)).
%   For unichar streams 
-define(NS_BASE_STREAM_NO_CONVERTER, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_BASE+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 5)).
%   For unichar streams 
-define(NS_BASE_STREAM_BAD_CONVERSION, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_BASE+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 6)).

-define(NS_BASE_STREAM_WOULD_BLOCK, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_BASE+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 7)).

-define(NS_ERROR_FILE_UNRECOGNIZED_PATH, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_FILES+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 1)).
-define(NS_ERROR_FILE_UNRESOLVABLE_SYMLINK, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_FILES+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 2)).
-define(NS_ERROR_FILE_EXECUTION_FAILED, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_FILES+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 3)).
-define(NS_ERROR_FILE_UNKNOWN_TYPE, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_FILES+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 4)).
-define(NS_ERROR_FILE_DESTINATION_NOT_DIR, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_FILES+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 5)).
-define(NS_ERROR_FILE_TARGET_DOES_NOT_EXIST, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_FILES+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 6)).
-define(NS_ERROR_FILE_COPY_OR_MOVE_FAILED, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_FILES+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 7)).
-define(NS_ERROR_FILE_ALREADY_EXISTS, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_FILES+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 8)).
-define(NS_ERROR_FILE_INVALID_PATH, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_FILES+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 9)).
-define(NS_ERROR_FILE_DISK_FULL, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_FILES+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 10)).
-define(NS_ERROR_FILE_CORRUPTED, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_FILES+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 11)).
-define(NS_ERROR_FILE_NOT_DIRECTORY, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_FILES+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 12)).
-define(NS_ERROR_FILE_IS_DIRECTORY, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_FILES+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 13)).
-define(NS_ERROR_FILE_IS_LOCKED, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_FILES+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 14)).
-define(NS_ERROR_FILE_TOO_BIG, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_FILES+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 15)).
-define(NS_ERROR_FILE_NO_DEVICE_SPACE, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_FILES+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 16)).
-define(NS_ERROR_FILE_NAME_TOO_LONG, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_FILES+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 17)).
-define(NS_ERROR_FILE_NOT_FOUND, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_FILES+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 18)).
-define(NS_ERROR_FILE_READ_ONLY, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_FILES+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 19)).
-define(NS_ERROR_FILE_DIR_NOT_EMPTY, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_FILES+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 20)).
-define(NS_ERROR_FILE_ACCESS_DENIED, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_FILES+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 21)).

-define(NS_SUCCESS_FILE_DIRECTORY_EMPTY, ((?NS_ERROR_SEVERITY_SUCCESS bsl 31) | ((?NS_ERROR_MODULE_FILES+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 1)).

%  Result codes used by nsIVariant 

-define(NS_ERROR_CANNOT_CONVERT_DATA, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_XPCOM+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 1)).
-define(NS_ERROR_OBJECT_IS_IMMUTABLE, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_XPCOM+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 2)).
-define(NS_ERROR_LOSS_OF_SIGNIFICANT_DATA, ((?NS_ERROR_SEVERITY_ERROR bsl 31) | ((?NS_ERROR_MODULE_XPCOM+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 3)).

-define(NS_SUCCESS_LOSS_OF_INSIGNIFICANT_DATA, ((?NS_ERROR_SEVERITY_SUCCESS bsl 31) | ((?NS_ERROR_MODULE_XPCOM+?NS_ERROR_MODULE_BASE_OFFSET) bsl 16) | 1)).


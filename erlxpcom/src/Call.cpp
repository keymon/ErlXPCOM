/*
***** BEGIN LICENSE BLOCK *****

This file is part of the erlXPCOM (Erlang XPCOM binding) project.

Copyright (C) 2005 Hector Rivas Gandara <keymon@gmail.com>

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published replyby the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

***** END LICENSE BLOCK *****
*/
#include "ErlXPCOMLog.h"

#include "Call.h"

using namespace erlxpcom;

Call::Call(lfOID _oid, lfCallID _callID, 
		   std::string _methodName):
	oid(_oid), callID(_callID), methodName(_methodName),
	completed(false), launched(false)
{
}


void Call::returnSuccess(ErlList* outParams) 
	throw(AlreadyExecutedException, InternalErrorException)
{
	try {
		if (this->isCompleted()) {
			throw AlreadyExecutedException("Return method called more than one time");
		}
		if (outParams == 0 || !outParams->isValid()) {
			throw InternalErrorException("out params list is not a valid element");
		}
		this->outParams.reset(outParams);
		PR_LOG(ErlXPCOMLog::getLog(), PR_LOG_ERROR, 
			("Call::returnSuccess(): Returning success with out params: %s", 
			 outParams->toString().c_str()));
		result = NS_OK;
		this->doReturn();
	} catch (InternalErrorException &e) {
		PR_LOG(ErlXPCOMLog::getLog(), PR_LOG_ERROR, 
			("Call::returnSuccess(): Internal error: %s, trying return failure", 
			 e.getMessage().c_str()));
		returnFailure(NS_ERROR_FAILURE);
	}
}

void Call::returnFailure(nsresult result) 
	throw(AlreadyExecutedException, InternalErrorException)
{
	if (this->isCompleted()) {
		throw AlreadyExecutedException("Return method called more than one time");
	}

	this->result = result;
	completed = true;
	doReturn();
}

void Call::call() 
	throw (AlreadyExecutedException, 
		   InterfaceMismatchException, 
		   InternalErrorException)
{
	if (this->isLaunched()) {
		throw AlreadyExecutedException("Call launched two or more times");
	}
	this->doCall();
	launched = true;
}

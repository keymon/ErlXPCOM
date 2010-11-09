/*
***** BEGIN LICENSE BLOCK *****

This file is part of the erlXPCOM (Erlang XPCOM binding) project.

Copyright (C) 2005 Hector Rivas Gandara <keymon@gmail.com>

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
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
#include <nscore.h>

#include "ErlangCall.h"
#include "ErlXPCOMHelper.h"
#include "ErlXPCOMHelper.h"

#include "NSPRUtils.h"

using namespace erlxpcom;

static NS_DEFINE_CID(kEventQueueServiceCID, NS_EVENTQUEUESERVICE_CID);

ErlangCall::ErlangCall(ErlXPCOMProxy *_xpcomProxy, lfCallID _callID, 
					   PRUint16 _methodIndex, const nsXPTMethodInfo *_methodInfo, 
					   nsXPTCMiniVariant* params)
	throw (InternalErrorException, InterfaceMismatchException):
	Call(_xpcomProxy->getOID(), _callID, _methodInfo->GetName()), 
	log(ErlXPCOMLog::getLog()), 
	xpcomProxy(_xpcomProxy), orb(_xpcomProxy->getORB()),
	callLock(PR_NewLock()), callCond(PR_NewCondVar(callLock))
{
	methodInfo.reset(new ErlXPCOMMethodInfo(xpcomProxy->getInterfaceInfo(), 
										    _methodInfo, _methodIndex));
	method.reset(new ErlXPCOMRemoteMethod(methodInfo.get(), params));
	
	if (methodInfo->isGetter()) {
		type = GET_ATTRIBUTE;
	} else if (methodInfo->isSetter()) {
		type = SET_ATTRIBUTE;
	} else {
		type = CALL_METHOD;
	}

	
}

ErlangCall::~ErlangCall() {
	PR_DestroyCondVar(callCond);
	PR_DestroyLock(callLock);
}

void ErlangCall::waitForReturn() throw (InternalErrorException) {
	// Wait for return entering in a loop and processing incoming events.
    PR_LOG(log, PR_LOG_DEBUG, 
		("ErlangCall::waitForReturn(method=\"%s\", CallId=%i): waiting\n", 
		methodName.c_str(), callID));
	// If there is not event Queue, use conditions to lock the thread
	if (noEventLoop) {
		NSPRScopedLock _scopedLock(callLock);
		while(!isCompleted()) {
			PR_WaitCondVar(callCond, PR_INTERVAL_NO_TIMEOUT);
		}
	} else {
		nsresult rv;    
		while (!isCompleted()) {
			PLEvent *nextEvent;
			rv = currentEventQ->WaitForEvent(&nextEvent);

			PR_LOG(log, PR_LOG_DEBUG, 
				("ErlangCall::waitForReturn(): Dispatch event"));

			currentEventQ->HandleEvent(nextEvent);
			if (NS_FAILED(rv)) {
				PR_LOG(log, PR_LOG_DEBUG, 
					("--ErlangCall::waitForReturn(): failed dispaching event"));
				break;
			}
		}
	}
    PR_LOG(log, PR_LOG_DEBUG, 
		("ErlangCall::waitForReturn(method=\"%s\", CallId=%i): Exitting", 
		methodName.c_str(), callID));
	
}

void ErlangCall::doCall()
	throw (InterfaceMismatchException, 
		   InternalErrorException)
{

	// Get the transport 
	RequestTransport* transport = orb->getRequestTransport();

	// Marshall in params
	ErlXPCOMMethodMarshaller marshaller(orb, true, 
									    methodInfo->getParamInCount()-
										methodInfo->getParamInSizeCount());
	method->processInParams(marshaller);
	
	// -------
	// Get the event queue for this thread. Do this here because is not 
	// sure that we will enter in waitReturn before doReturn is called
	noEventLoop = false;
	eventLoopCreated = false;
	nsresult rv = NS_OK;

	eventQService = do_GetService(kEventQueueServiceCID);
	
	// If there is no eventQService, run this call without eventQueues
	if (eventQService.get() == NULL) {
		PR_LOG(log, PR_LOG_WARNING, 
			("ErlangCall::doCall(), CallId=%i. can't get/create event queue, running without it", 
			 callID));
		noEventLoop = true;
	} else {
		rv = eventQService->GetThreadEventQueue(NS_CURRENT_THREAD, 
												getter_AddRefs(currentEventQ));
		// If there is no event queue, create one 
		if (NS_FAILED(rv)) {
			PR_LOG(log, PR_LOG_DEBUG, 
				("ErlangCall::doCall(method=\"%s\", CallId=%i). "
				 "No event queue, creating one", 
				 methodName.c_str(), callID));
			rv = eventQService->CreateMonitoredThreadEventQueue();
			if (NS_FAILED(rv)) {
				PR_LOG(log, PR_LOG_WARNING, 
					("ErlangCall::doCall(), CallId=%i. can't get/create event queue, running without it",
					callID));
				noEventLoop = true;
			} else {
				eventLoopCreated = true;
				rv = eventQService->GetThreadEventQueue(NS_CURRENT_THREAD, 
														getter_AddRefs(currentEventQ));
			}
		}
	}
	// send the call, setting this class as ReplyObserver
	transport->sendCall(oid, callID, methodName, marshaller.getParams(), type, this);

}

void ErlangCall::doReturn() 
	throw (InternalErrorException)
{
	if (isSuccess()) {
		// unmarshall out params
		try {
			ErlXPCOMMethodUnmarshaller unmarshaller(orb, false, outParams.get());
			method->processOutParams(unmarshaller);
		} catch (InterfaceMismatchException &e) {
			PR_LOG(log, PR_LOG_ERROR, 
				("ErlangCall::doReturn(): error unmarshalling out parameters: %s\n", 
				 e.getMessage().c_str()));
			throw InternalErrorException(e.getMessage());
		}
	}

	completed = true;
	 
	// Notify waiting process 
	if (noEventLoop) {
		// If there is not eventQ, use the conditions
		NSPRScopedLock _scopedLock(callLock);
		PR_NotifyAllCondVar(callCond);
	} else {
		// Notify sending null event to eventQ
		PLEvent *event = PR_NEW(PLEvent);
		PL_InitEvent(event, 
					 this,
					 ErlangCall::EventHandler,
					 ErlangCall::DestroyHandler);
		currentEventQ->PostEvent(event);

// FIXME!! This crashes if you execute a lot of calls fast
//		if (eventLoopCreated) {
//			 eventQService->DestroyThreadEventQueue();
//			 currentEventQ = NULL;
//		}
	}
}

void ErlangCall::reply(lfCallID callID, ErlTuple* reply) {
	ErlTermPtr<ErlTuple> successReply(new ErlTuple(
										new ErlAtom("ok"), 
										new ErlVariable("OutParams")));
	ErlTermPtr<ErlTuple> errorReply(new ErlTuple(
										new ErlAtom("error"), 
										new ErlVariable("Result")));
									  
	VariableBinding binding;
	try {
		// {ok, OutParams} ->
		if (successReply->match(reply, &binding)) {
			ErlList *outParams =		
				ErlList::cast(binding.search("OutParams"));
			returnSuccess(outParams);
		// {error, Error code} ->
		} else if (errorReply->match(reply, &binding)) {
			ErlLong *resultLong =		
				ErlLong::cast(binding.search("Result"));
			returnFailure(resultLong->longValue());
		// _ ->
		} else {
			PR_LOG(log, PR_LOG_ERROR, 
				("ErlangCall::reply(): bad reply format %s\n", 
				 reply->toString().c_str()));
			// return failure
			returnFailure(NS_ERROR_FAILURE);
		}
	} catch (EpiBadArgument &e) {
		PR_LOG(log, PR_LOG_ERROR, 
			("ErlangCall::reply(): bad reply format %s\n", 
			 reply->toString().c_str()));
		// return failure
		returnFailure(NS_ERROR_FAILURE);
	} catch (std::exception &e) {
		PR_LOG(log, PR_LOG_ERROR, 
			("ErlangCall::reply(): exception %s\n", 
			 e.what()));
		// return failure
		returnFailure(NS_ERROR_FAILURE);
	}
}

void* ErlangCall::EventHandler(PLEvent *event) {
	// Do nothing, just notify the return 
	PR_LOG(ErlXPCOMLog::getLog(), PR_LOG_DEBUG, 
		("ErlangCall::EventHandler(): call reply notified"));
	return 0;
}

void ErlangCall::DestroyHandler(PLEvent *event) {
	// Delete the event after execution. 
	PR_DELETE(event);
}


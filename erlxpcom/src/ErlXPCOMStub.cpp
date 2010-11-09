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

#include "ErlXPCOMStub.h"
#include "XPCOMCall.h"

#include <sstream>

using namespace erlxpcom;

static NS_DEFINE_CID(kEventQueueServiceCID, NS_EVENTQUEUESERVICE_CID);

ErlXPCOMStub::ErlXPCOMStub(nsISupports *_xpcomObject, nsIID _iid, void* keyThread)
	throw (XPCOMException):
	xpcomObject(_xpcomObject), 
	iid(_iid), orb(NULL)
{
    nsresult rv;
    log = ErlXPCOMLog::getLog();

	// Get the thread. It can be current or UI thread
	if (keyThread == NS_CURRENT_THREAD)	{
		owningThread = PR_GetCurrentThread();
	} else if (keyThread == NS_UI_THREAD) {
		nsCOMPtr<nsIThread>  mainIThread;

		// Get the primordial thread
		nsresult rv = nsIThread::GetMainThread(getter_AddRefs(mainIThread));
		if (NS_FAILED(rv)) throw XPCOMException("Fail getting main (UI) thread", rv);

		rv = mainIThread->GetPRThread(&owningThread);
		if (NS_FAILED(rv)) throw XPCOMException("Fail getting main (UI) thread", rv);
	}

	// Get the EventQueueService and the EventQueue for the given thread
    eventQService = do_GetService(kEventQueueServiceCID);
	
    rv = eventQService->GetThreadEventQueue((PRThread *) keyThread, 
											getter_AddRefs(owningEventQ));
											
    if (NS_FAILED(rv)) { 
        PR_LOG(log, PR_LOG_DEBUG, ("ErlXPCOMStub::ErlXPCOMStub() no eventQ in the given thread\n"));

        /* there is no eventQ in the given thread. Let's try using UI thread eventQ
           what else can we do? supposedly calling from inside UI thread is safe
        */
        rv = eventQService->GetThreadEventQueue(NS_UI_THREAD, 
							                    getter_AddRefs(owningEventQ));
        if (NS_FAILED(rv)) {
            PR_LOG(log, PR_LOG_DEBUG, ("ErlXPCOMStub::ErlXPCOMStub() no eventQ in the UI thread\n"));
            owningEventQ = NULL;
        }
	}
	
	// Get the interface info for the given IID.
    nsCOMPtr<nsIInterfaceInfoManager> iimgr = XPTI_GetInterfaceInfoManager();
	if (iimgr == 0) throw XPCOMException("Can't get info for object", 0);
	
	rv = iimgr->GetInfoForIID(&iid, getter_AddRefs(interfaceInfo));
    if (NS_FAILED(rv)) throw XPCOMException("Can't get info for object", rv);
		
}

bool ErlXPCOMStub::isOwnedByCurrentThread() {
	return (owningThread == PR_CurrentThread() || (void*)owningEventQ == NULL);
}

Call* ErlXPCOMStub::createCall(lfCallID callID, 
							   std::string methodName, 
							   ErlList* inParams,
							   call_type type)
	throw(InternalErrorException,
		  InterfaceMismatchException) 
{
	// if it have not orb defined, fail
	if (this->getORB() == NULL) {
		PR_LOG( log, PR_LOG_ERROR, 
			("ErlXPCOMStub::createCall(): Stub not registered in ORB"));
		throw InternalErrorException("Stub not registered in ORB");
	}
	
	XPCOMCall* theCall = new XPCOMCall(this, callID, methodName, inParams, type);
	return theCall;
}

std::string ErlXPCOMStub::toString() {
	std::ostringstream oss;
	oss << "ErlXPCOMStub{nsISupports = " << std::hex << xpcomObject << 
		   ", oid = " <<  oid <<
		   ", iid = " << iid.ToString() << "}";
	return oss.str();
}

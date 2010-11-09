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
#ifndef __ERLXPCOMSTUB_H
#define __ERLXPCOMSTUB_H

#include <epi.hpp>

#include <nsISupports.h>
#include <nsCOMPtr.h>

// Event queues and related stuff
#include <nsIServiceManager.h>
#include <nsIEventQueueService.h>
#include <nsIEventQueue.h>
#include <nsIThread.h>
#include <plevent.h>

// XPCOM reflection
#include <xptcall.h>
#include <xptinfo.h>

// Interface info
#include <nsIInterfaceInfo.h>
#include <nsIInterfaceInfoManager.h>

#include "ErlXPCOMLog.h"
#include "ErlXPCOMException.h"
#include "ErlXPCOMDefs.h"
#include "Call.h"

namespace erlxpcom {

class ErlangORB;

class ErlXPCOMStub {
public:
	/**
	 * Create a new Stub to enmascare XPCOM objects. 
	 * This constructor will:
	 *  - get the event queue for the given thread.
	 *  - get the interface info for this object
	 * @param xpcomObject XPCOM object to enmascare
	 * @param keyThread Owning thread of the xpcom object. Normally
	 *	we use the same thread that creates the object. It can be a thread id
	 *  or the keys:
	 *	 - NS_CURRENT_THREAD
	 *   - NS_UI_THREAD
	 * The object will try to get the EventQueue for this thread, and if
	 * fails it will try with NS_UI_THREAD. 
	 * @throw XPCOMException if there is an XPCOM internal error.
	 */
	ErlXPCOMStub(nsISupports *_xpcomObject, nsIID _iid, void* keyThread)
		throw (XPCOMException);

	inline lfOID getOID() const {
		return oid;
	}

	inline void setOID(lfOID oid) {
		this->oid = oid;
	}

	inline nsIID getIID () const {
		return iid;
	}
		
	inline ErlangORB* getORB() const {
		return orb;
	}
	
	inline void setORB(ErlangORB *orb) {
		this->orb = orb;
	}
	
	inline nsISupports* getXPCOMObject() {
		return xpcomObject;
	}
	
	inline nsIInterfaceInfo* getInterfaceInfo() {
		return interfaceInfo.get();
	}
	
	inline nsIEventQueue* getEventQueue() {
		return owningEventQ.get();
	}
	
	/**
	 * Checks if this Stub is owned by the current thread
	 */
	bool isOwnedByCurrentThread();
	
	/**
	 * Create a new call for the encasulapted xpcom object
	 * @throw InterfaceMismatchException if the method does not exists
	 *  or the parameters are wrong.
	 * @throw InternalErrorException if there is an error building the call, 
	 *  particulary while query the method interface
	 */
	Call* createCall(lfCallID callID, 
	                 std::string methodName, ErlList* inParams,
					 call_type type)
		throw(InternalErrorException, InterfaceMismatchException);
	
	/**
	 * get string representation of this class
	 */
	std::string toString();
	
private:
	PRLogModuleInfo *log;

	/* Owning thread data */
    PRThread* owningThread;
    nsCOMPtr<nsIEventQueue> owningEventQ;
    nsCOMPtr<nsIEventQueueService>  eventQService;
	
	nsCOMPtr<nsISupports> xpcomObject;
	nsIID iid;

	nsCOMPtr<nsIInterfaceInfo> interfaceInfo;

	lfOID oid;
	ErlangORB* orb;
	
};

}


#endif // __ERLXPCOMSTUB_H

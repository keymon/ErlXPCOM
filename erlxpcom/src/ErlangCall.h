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
#ifndef __ERLANGCALL_H
#define __ERLANGCALL_H

#include <memory>
#include <string>

#include <prtypes.h>
#include <prmem.h>
#include <prcvar.h>
#include <prlock.h>

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

#include "ErlXPCOMMethodInfo.h"
#include "ErlXPCOMMethod.h"
#include "ErlXPCOMRemoteMethod.h"
#include "ErlXPCOMMethodUnmarshaller.h"
#include "ErlXPCOMMethodMarshaller.h"

#include "Call.h"
#include "ErlXPCOMLog.h"
#include "ErlXPCOMException.h"
#include "ErlXPCOMHelper.h"
#include "ErlangOrb.h"
#include "RequestTransport.h"
#include "ErlXPCOMProxy.h"

namespace erlxpcom {

//class ErlXPCOMProxy;

class ErlangCall: public ReplyObserver, public Call {

public:
	/**
	 * Create a new call to a ErlangCall object. 
	 * @param xpcomProxy proxy that encapsulates the Erlang object
	 * @param callID call identifier 
	 * @param methodInfo Method info of method to call
	 * @param miniVariants array of parameters
	 * @throw ErlXPCOMInternalErrorException if there is an error while quering
	 * the interface of the object.
	 * @throw ErlXPCOMInterfaceMismatchException if there is a problem with 
	 *	    method info, like: 
	 *		- method is not scriptable
	 *		- method does not exist
	 *		- etc.
	 */
	ErlangCall(ErlXPCOMProxy *_xpcomProxy, lfCallID callID, 
			   PRUint16 _methodIndex, const nsXPTMethodInfo *_methodInfo, 
			   nsXPTCMiniVariant* params)
		throw (InternalErrorException,
		 	   InterfaceMismatchException);

	virtual ~ErlangCall();
	
	inline ErlXPCOMMethodInfo* getMethodInfo() {
		return methodInfo.get();
	}

	/** 
	 * Will get the thread event queue and process incoming events until 
	 * call is completed.
	 */
	virtual void waitForReturn() throw(InternalErrorException);
	
	virtual void reply(lfCallID callID, ErlTuple* reply);
	
protected:
	virtual void doCall()
		throw (InterfaceMismatchException, 
			   InternalErrorException);
	
	virtual void doReturn() 
		throw (InternalErrorException);


//	void doRealCall()
//		throw (InterfaceMismatchException, 
//			   InternalErrorException);


	/**
	 * Event Handler for reply response
	 */
	static void* EventHandler(PLEvent *self);
	
	/**
	 * Event destroy handler
	 */
	static void DestroyHandler(PLEvent *self);


private:
	PRLogModuleInfo *log;

	ErlXPCOMProxy* xpcomProxy;
	
	ErlangORB* orb; 

	std::auto_ptr<ErlXPCOMRemoteMethod> method;
	std::auto_ptr<ErlXPCOMMethodInfo> methodInfo;

	call_type type;
	
	// Calling thread event queue
    nsCOMPtr<nsIEventQueue> currentEventQ;
    nsCOMPtr<nsIEventQueueService>  eventQService;

	bool noEventLoop;
	bool eventLoopCreated;

	// Lock and condition for waitForReturn. Used only if there is no eventQueue
	PRLock *callLock;
	PRCondVar *callCond;

};

} // namespace erlxpcom; 


#endif // __ERLANGCALL_H

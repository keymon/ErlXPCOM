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
#ifndef __XPCOMCALL_H
#define __XPCOMCALL_H

#include <memory>

#ifdef DEBUG
#include <nsDebug.h>
#endif 

#include <prtypes.h>
#include <prmem.h>
#include <prcvar.h>
#include <prlock.h>

#include <xptcall.h>
#include <xptinfo.h>
#include <nsIInterfaceInfo.h>

#include "ErlXPCOMMethodInfo.h"
#include "ErlXPCOMMethod.h"
#include "ErlXPCOMLocalMethod.h"
#include "ErlXPCOMMethodUnmarshaller.h"
#include "ErlXPCOMMethodMarshaller.h"

#include "Call.h"
#include "ErlXPCOMLog.h"
#include "ErlXPCOMStub.h"
#include "ErlXPCOMException.h"
#include "ErlXPCOMHelper.h"
#include "ErlangOrb.h"
#include "RequestTransport.h"

namespace erlxpcom {

class XPCOMCall: public Call {
public:
	/**
	 * Create a new call to a XPCOM object. 
	 * @param xpcomStub stub that encapsulates the XPCOM object
	 * @param callID call identifier 
	 * @param methodName name of the method to call. 
	 * @param inParams in parameters
	 * @param type Type of call (call method, set/get attribute)
	 * @throw ErlXPCOMInternalErrorException if there is an error while quering
	 * the interface of the object.
	 * @throw ErlXPCOMInterfaceMismatchException if there is a problem with 
	 *	    method info, like: 
	 *		- method is not scriptable
	 *		- method does not exist
	 *		- etc.
	 * @throw UnknownMethodException if the method is not implemented by 
	 *      this object
	 */
	XPCOMCall(ErlXPCOMStub *_xpcomStub, lfCallID callID, 
			  std::string methodName, ErlList* inParams, call_type type)
			  throw (InternalErrorException,
					 InterfaceMismatchException,
					 UnknownMethodException);

	virtual ~XPCOMCall();
	
	// This method will not really be used: in current design no thread will 
	// wait for reply, but is here for testing proposes
	virtual void waitForReturn()
		throw (InternalErrorException);
	
protected:
	virtual void doCall()
		throw (InterfaceMismatchException, 
			   InternalErrorException);
	
	/** 
	 *  WARNING; WARNING; WARNING; WARNING; WARNING; WARNING; WARNING; WARNING; 
	 * This method finally deletes this instance. Who should delete it if not?
	 */ 
	virtual void doReturn() 
		throw (InternalErrorException);


	void doRealCall()
		throw (InterfaceMismatchException, 
			   InternalErrorException);

	/**
	 * Event Handler that executes the method in the object owner thread
	 */
	static void* EventHandler(PLEvent *self);
	
	/**
	 * Event destroy handler
	 */
	static void DestroyHandler(PLEvent *self);

private:
	PRLogModuleInfo *log;

	ErlXPCOMStub* xpcomStub;
	
	ErlangORB* orb; 
	
	std::auto_ptr<ErlXPCOMMethodInfo> methodInfo;
	std::auto_ptr<ErlXPCOMLocalMethod> method;
	
	call_type type;
	
	// Current event queue
	nsCOMPtr<nsIEventQueue> currentEventQ;
	// Lock and condition for waitForReturn. (only for testing)
	//PRLock *callLock;
	//PRCondVar *callCond;
};

} // namespace erlxpcom; 

#endif // __XPCOMCALL_H

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
#ifndef __ERLXPCOMPROXY_H
#define __ERLXPCOMPROXY_H

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

#include "ErlXPCOMDefs.h"
#include "ErlXPCOMHelper.h"
#include "ErlXPCOMException.h"

#include "ErlXPCOMLog.h"

// uuid{686684c2-96b4-4911-9deb-a716b7c64d66}
#define ERLXPCOMPROXY_IID \
{ 0x686684c2, 0x96b4, 0x4911, { 0x9d, 0xeb, 0xa7, 0x16, 0xb7, 0xc6, 0x4d, 0x66 } }

namespace erlxpcom {

class ErlangCall;
class ErlangORB;

/**
 * Proxy of remote erlang objects. 
 * This class implements nsXPTCStubBase, that forwards all the method
 * calls to the CallMethod method. 
 */
class ErlXPCOMProxy: public nsXPTCStubBase {
public:	
    NS_DEFINE_STATIC_IID_ACCESSOR(ERLXPCOMPROXY_IID);

    NS_DECL_ISUPPORTS;

	/**
	 * Create a new ErlXPCOMProxy
	 * @param oid Object identifier number
	 * @param iid interface identifier for this instance
	 * @throw XPCOMException if there is an error getting interface info
	 */
    ErlXPCOMProxy(lfOID _oid, const nsIID &_iid)
		throw (XPCOMException);
	
    virtual ~ErlXPCOMProxy();
    
	/**
	 * Return the oid of the remote object
	 */
	lfOID getOID() const;
	
	/**
	 * Return the iid 
	 */
	nsIID getIID() const;
	
	/**
	 * Return the interface info not referenced counted
	 */
	nsIInterfaceInfo *getInterfaceInfo() const;
	
	/**
	 * Set the orb
	 */
	void setORB(ErlangORB *orb);
	
	/**
	 * Get associated Orb
	 */ 
	ErlangORB* getORB() const; 
		
	/** 
	 * Get a string describing this object
	 */
	std::string toString();
	
	/**
	 * Create a call to a method of this object
	 */
	ErlangCall* createCall(PRUint16 methodIndex,
                           const nsXPTMethodInfo* methodInfo,
                           nsXPTCMiniVariant* params)
		throw (InternalErrorException,
		 	   InterfaceMismatchException);
	
	/**
	 * Returns the refcountered interface info of this proxy. It will only
	 * return the interface info for the iid given in constructor.
	 */
	NS_IMETHOD GetInterfaceInfo(nsIInterfaceInfo** info);

	/**
	 * Call to remote erlang object.
	 */
    NS_IMETHOD CallMethod(PRUint16 methodIndex,
                          const nsXPTMethodInfo* info,
                          nsXPTCMiniVariant* params);
private:
 	PRLogModuleInfo *log;
 
	lfOID oid;
    nsID iid;
    ErlangORB *orb;

	nsCOMPtr<nsIInterfaceInfo> interfaceInfo;

};

} // namespace erlxpcom


#endif // __ERLXPCOMPROXY_H

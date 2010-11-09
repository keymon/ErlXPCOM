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

#include <sstream>
 
#include "ErlXPCOMProxy.h"
#include "ErlangCall.h"
#include "nsIClassInfo.h"
#include "nsIXPConnect.h"

using namespace erlxpcom;
using namespace epi::type;
using namespace epi::error;

ErlXPCOMProxy::ErlXPCOMProxy(lfOID _oid, const nsIID &_iid)
	throw (XPCOMException):
	log(ErlXPCOMLog::getLog()), oid(_oid), iid(_iid)
{
	// Get the interface info for the given IID.
    nsCOMPtr<nsIInterfaceInfoManager> iimgr = XPTI_GetInterfaceInfoManager();
	if (iimgr == 0) {
		PR_LOG(log, PR_LOG_ERROR, 
			("ErlXPCOMProxy(): Error getting interfaceInfoManager"));
		throw XPCOMException("Can't get info for object", 0);
	}
	
	nsresult rv = iimgr->GetInfoForIID(&iid, getter_AddRefs(interfaceInfo));
    if (NS_FAILED(rv)) {
		PR_LOG(log, PR_LOG_ERROR, 
			("ErlXPCOMProxy(): Error getting info for IID=%s", iid.ToString()));
		throw XPCOMException("Can't get info for object", rv);
	}
	
}

ErlXPCOMProxy::~ErlXPCOMProxy() {
	// release this object in orb
	orb->dropProxy(this);
}


nsrefcnt ErlXPCOMProxy::AddRef(void) { 
    nsrefcnt cnt = (nsrefcnt) PR_AtomicIncrement((PRInt32*)&mRefCnt);
    PR_LOG(log, PR_LOG_DEBUG, ("ErlXPCOMProxy[0x%08x]::AddRef() -> %d\n",
		this, (unsigned)cnt));
    return cnt;
}

nsrefcnt ErlXPCOMProxy::Release(void) {
    nsrefcnt cnt = (nsrefcnt) PR_AtomicDecrement((PRInt32*)&mRefCnt);
    PR_LOG(log, PR_LOG_DEBUG, ("ErlXPCOMProxy[0x%08x]::Release() %d\n",
		this, (unsigned)cnt));
    if(0 == cnt) {  
       delete this;
    }
    return cnt;
}

NS_IMETHODIMP ErlXPCOMProxy::QueryInterface(REFNSIID aIID, void** aInstancePtr) {
	
	// Self resolved IIDs
    if (aIID.Equals(NS_GET_IID(nsISupports)) ||
		aIID.Equals(NS_GET_IID(ErlXPCOMProxy)) ||
		aIID.Equals(iid)) { 
		PR_LOG(log, PR_LOG_DEBUG, 
			("ErlXPCOMProxy[0x%08x]::QueryInterface(%s): self resolved", 
			 this, aIID.ToString()));
        *aInstancePtr = this;
		NS_ADDREF(this);
        return NS_OK;
    }
	// WARNING Actually we do not support the nsIClassInfo 
	// or the nsIXPConnectWrappedJS and others. Return failure here to
	// optize..
    if (aIID.Equals(NS_GET_IID(nsIClassInfo))||
		aIID.Equals(NS_GET_IID(nsIXPConnectWrappedJS))) {
		*aInstancePtr = 0;
        return NS_NOINTERFACE;
	}

	PR_LOG(log, PR_LOG_DEBUG, 
		("ErlXPCOMProxy[0x%08x]::QueryInterface(%s)", this, aIID.ToString()));
		
	// Get info for "QueryInterface" method. MethodIndex = 0.
    PRUint16 methodIndex = 0;
    const nsXPTMethodInfo *info;
    interfaceInfo->GetMethodInfo(methodIndex, &info); 
	// this should not fail
	if (info == 0) return NS_ERROR_FAILURE;

	// ... execute the call
    nsXPTCMiniVariant params[2];
    params[0].val.p = (void*)&aIID;
    params[1].val.p = aInstancePtr;
    nsresult result = CallMethod(methodIndex, info, params);

    if (NS_FAILED(result) || *aInstancePtr == NULL) {
        PR_LOG(log, PR_LOG_DEBUG, 
			("--ErlXPCOMProxy[0x%08x]::QueryInterface(%s): No Interface\n", 
				this, aIID.ToString()));
        result = NS_NOINTERFACE;
    } else {
		PR_LOG(log, PR_LOG_DEBUG, 
			("--ErlXPCOMProxy[0x%08x]::QueryInterface(%s): got object ptr=0x%08x\n", 
				this, aIID.ToString(), *aInstancePtr));
		// Increase the reference counter
		// I increase it in the unmarshaller NS_ADDREF((nsISupports*)*aInstancePtr);
	}
    return result;
}

lfOID ErlXPCOMProxy::getOID() const {
	return oid;
}

nsIID ErlXPCOMProxy::getIID() const {
	return iid;
}


nsIInterfaceInfo *ErlXPCOMProxy::getInterfaceInfo () const {
	return interfaceInfo.get();
}

void ErlXPCOMProxy::setORB(ErlangORB* orb) {
	this->orb = orb;
}

ErlangORB* ErlXPCOMProxy::getORB() const {
	return orb;
}

std::string ErlXPCOMProxy::toString() {
	std::ostringstream oss;
	oss << "ErlXPCOMProxy{this = " << std::hex << this << 
		   ", oid = " <<  oid <<
		   ", iid = " << iid.ToString() << "}";
	return oss.str();	
}

ErlangCall* ErlXPCOMProxy::createCall(PRUint16 methodIndex,
                                      const nsXPTMethodInfo* methodInfo,
                                      nsXPTCMiniVariant* params) 
	throw (InternalErrorException,
		   InterfaceMismatchException)
{
	lfCallID callId = orb->newCallId();
	return new ErlangCall(this, callId, methodIndex, methodInfo, params);
}


NS_IMETHODIMP ErlXPCOMProxy::GetInterfaceInfo(nsIInterfaceInfo** info) {
    if(!info) {
        return NS_ERROR_NULL_POINTER;
    }
	
    *info =  interfaceInfo.get();
    NS_ADDREF(*info);
    return NS_OK;
}


NS_IMETHODIMP ErlXPCOMProxy::CallMethod(PRUint16 methodIndex,
                                        const nsXPTMethodInfo* methodInfo,
                                        nsXPTCMiniVariant* params) {
										
    std::auto_ptr<ErlangCall> call(createCall(methodIndex, methodInfo, params));

	PR_LOG(log, PR_LOG_DEBUG, 
		("ErlXPCOMProxy[0x%08x]::CallMethod(method=\"%s\", callId=%i)", 
			this, call->getMethodInfo()->getName().c_str(), 
			call->getCallID()));
	
	try {
		call->call();
		call->waitForReturn();
	} catch(ErlXPCOMException &e) {
		PR_LOG(log, PR_LOG_DEBUG, 
			("ErlXPCOMProxy[0x%08x]::CallMethod(%s)\n  Exception: %s", 
			 methodInfo->GetName(), e.what()));
		return NS_ERROR_FAILURE;
	}
	
	PR_LOG(log, PR_LOG_DEBUG, 
		("ErlXPCOMProxy[0x%08x]::CallMethod(method=\"%s\", callId=%i), returning %s", 
			this, call->getMethodInfo()->getName().c_str(), 
			call->getCallID(), 
			call->isSuccess()? "success": "failure"));
			
	return call->getResult();
}


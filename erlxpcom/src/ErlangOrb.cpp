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
#include "ErlangOrb.h"
#include "RequestTransport.h"

using namespace erlxpcom;

ErlangORB::ErlangORB(RequestTransport* _transport): 
	log(ErlXPCOMLog::getLog()), transport(_transport), 
	oidCounter(0), callIdCounter(0),
	stubLock(PR_NewLock()), proxyLock(PR_NewLock())
{	
	transport->setOrb(this);
}

void ErlangORB::registerStub(ErlXPCOMStub* stub) {
	NSPRScopedLock _scopedLock(stubLock);

	lfOID oid = oidCounter++;
	stub->setOID(oid);
	stub->setORB(this);
	stubs[oid] = stub;

	PR_LOG(log, PR_LOG_DEBUG, 
		("ErlangORB::registerStub(%s)", stub->toString().c_str()));

	inverseStubs[StubKey(stub->getXPCOMObject(), stub->getIID())]  = stub;
}

ErlXPCOMStub* ErlangORB::dropStub(lfOID oid) {
	NSPRScopedLock _scopedLock(stubLock);
	if (stubs.count(oid)) {
		ErlXPCOMStub* stub = stubs[oid];
		PR_LOG(log, PR_LOG_DEBUG, 
			("ErlangORB::dropStub(%s)", stub->toString().c_str()));
		inverseStubs.erase(StubKey(stub->getXPCOMObject(), stub->getIID()));
		stubs.erase(oid);
		return stub;
	}
	return NULL;
}

ErlXPCOMStub* ErlangORB::getStub(lfOID oid) 
	throw (InternalErrorException)
{
	NSPRScopedLock _scopedLock(stubLock);

    if (stubs.count(oid)) {
        return stubs[oid];
    } else {
		PR_LOG(log, PR_LOG_ERROR, 
			("ErlangORB::getStub(): unknown stub with oid=%i", oid));
		throw InternalErrorException("Unregistered stub, object does not exists");
    }	
}

ErlXPCOMStub* ErlangORB::searchStub(lfOID oid) 
{
	NSPRScopedLock _scopedLock(stubLock);
    if (stubs.count(oid)) {
        return stubs[oid];
    } else {
		return 0;
    }	
}

/* Search stub using obj_ptr and iid as key... */
ErlXPCOMStub* ErlangORB::searchStub(nsISupports *obj, nsIID &iid) 
{
	NSPRScopedLock _scopedLock(stubLock);
	StubKey key(obj, iid);

    if (inverseStubs.count(key)) {
        return inverseStubs[key];
    } else {
		return 0;
    }	
}

void ErlangORB::registerProxy(ErlXPCOMProxy* proxy) {
	NSPRScopedLock _scopedLock(proxyLock);

	PR_LOG(log, PR_LOG_DEBUG, 
		("ErlangORB::registerProxy(%s)",
		 proxy->toString().c_str()));
	
	lfOID oid = proxy->getOID();
	proxy->setORB(this);
	proxies[ProxyKey(oid, proxy->getIID())] = proxy;
	inverseProxies[proxy]  = oid;
	
	addrefRemote(oid);
	
	// WARNING FIXME, solve segfaults if I quit this extra reference coutn
	NS_ADDREF(proxy);
	
	
	
}

void ErlangORB::dropProxy(ErlXPCOMProxy *proxy) {
	NSPRScopedLock _scopedLock(proxyLock);

	PR_LOG(log, PR_LOG_DEBUG, 
		("ErlangORB::drop-Proxy(%s)",
		 proxy->toString().c_str()));

	proxies.erase(ProxyKey(proxy->getOID(), proxy->getIID()));
	inverseProxies.erase(proxy);

	// Decrease the counter to erlang remote object  
	releaseRemote(proxy->getOID());

}

bool ErlangORB::searchProxy(nsISupports* obj, lfOID *oid) {
	NSPRScopedLock _scopedLock(proxyLock);
	ErlXPCOMProxy *proxy = (ErlXPCOMProxy*) obj;
    if (inverseProxies.count(proxy)) {
		*oid = proxy->getOID();
		return true;
	} else {
		return false;
	}	
}

ErlXPCOMProxy* ErlangORB::searchProxy(lfOID oid, nsIID &iid) {
	NSPRScopedLock _scopedLock(proxyLock);
	ProxyKey key(oid, iid); 
    if (proxies.count(key)) {
        return proxies[key];
    } else {
		return 0;
    }	
}

void ErlangORB::addrefRemote(const lfOID oid) {
    if (remoteCounters.count(oid)) {
        remoteCounters[oid]++;
    } else {
		remoteCounters[oid] = 1;
    }	
}
void ErlangORB::releaseRemote(const lfOID oid) {
    remoteCounters[oid]--;
	if (remoteCounters[oid]<=0) {
		PR_LOG(log, PR_LOG_DEBUG, 
			("ErlangORB::releaseRemote() droping remote %i",
				oid));
		try {
			transport->dropRemoteObject(oid);
		} catch (InternalErrorException &e) {
			PR_LOG(log, PR_LOG_ERROR, 
				("ErlangORB::releaseRemote(), failed droping remote %s",
					e.what()));
		}
		remoteCounters.erase(oid);
	}
}

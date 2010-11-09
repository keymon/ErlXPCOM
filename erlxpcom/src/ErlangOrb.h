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
#ifndef __ERLANGORB_H
#define __ERLANGORB_H

#include <map>

#include <nsISupports.h>

#include "ErlXPCOMDefs.h"
#include "ErlXPCOMStub.h"
#include "ErlXPCOMProxy.h"
#include "xpcom_utils.hpp"

namespace erlxpcom {

class RequestTransport;

class ErlangORB {
	// Class used as key of the inverse stub map
	class StubKey {
		nsISupports* obj;
		nsIID iid;
		void operator=(const StubKey &k2) {}
	public:
		bool operator<(const StubKey &k2) const {
			return obj < k2.obj?
						true : 
						obj == k2.obj? 
							this->iid < k2.iid:
							false;
		}
		StubKey(nsISupports* _obj, nsIID _iid): obj(_obj), iid(_iid) {}
		StubKey(const StubKey &k): obj(k.obj), iid(k.iid) {}
	};

	// Class used as key of the proxy map
	class ProxyKey {
		lfOID oid;
		nsIID iid;
	public:
		bool operator<(const ProxyKey &k2) const {
			return this->oid < k2.oid?
						true : 
						this->oid == k2.oid? 
							this->iid < k2.iid:
							false;
		}
		inline ProxyKey(lfOID _oid, nsIID _iid): oid(_oid), iid(_iid) {}
	};

	/**
	 * Map of stubs indexed by oid
	 */
	typedef std::map<lfOID, ErlXPCOMStub*> stub_map;

	/**
	 * Map with the reference counter of the remote objects
	 */
	typedef std::map<lfOID, int> remote_counter_map;
	
	
	/** 
	 * Map of stubs indexed by nsISupports and IID
	 * It's necesary index by pointer and IID since each IID has diferent 
	 * interface info
	 */
	typedef std::map<StubKey, ErlXPCOMStub*> stub_inverse_map;

	/**
	 * Map of proxies indexed by oid and iid
	 */
	typedef std::map<ProxyKey, ErlXPCOMProxy*> proxy_map;

	/**
	 * Map of proxies indexed by pointer to proxy
	 */
	typedef std::map<ErlXPCOMProxy*, lfOID> proxy_inverse_map;


public:
	
	ErlangORB(RequestTransport* _transport);
	
	/**
	 * Get the requestTransport reference
	 */
	inline RequestTransport* getRequestTransport() {
		return transport;
	}
	
	/**
	 * Get a new callId 
	 */
	inline lfCallID newCallId() {
		return callIdCounter++;
	}
	
	/**
	 * Register the given stub and give it a new oid. 
	 * The pointer owership is not transfered. The stub can be deleted
	 * after the call to dropStub.
	 * FIXME: do this method syncronized
	 */
	void registerStub(ErlXPCOMStub* stub);

	/** 
	 * Delete a stub. Returns the pointer to stub if found or null if not.
	 * Caller can delete the pointer after this call.
	 * FIXME: do this method syncronized
	 */
	ErlXPCOMStub* dropStub(lfOID oid);

	/**
	 * Get the stub for the given object id
	 * throw InternalErrorException if the stub is not registered
	 * FIXME: do this method syncronized
	 */ 
	ErlXPCOMStub* getStub(lfOID oid) throw (InternalErrorException);

	/**
	 * Search the stub by the nsISupports pointer of the corresponding 
	 * XPCOM object and the iid. Returns 0 if not found.
	 */ 
	ErlXPCOMStub* searchStub(nsISupports* obj, nsIID &iid);

	/**
	 * Search the stub by the oid Returns 0 if not found.
	 */ 
	ErlXPCOMStub* searchStub(lfOID oid);
	
	/**
	 * Register the given stub and give it a new oid. 
	 * The pointer owership is not transfered
	 * FIXME: do this method syncronized
	 */
	void registerProxy(ErlXPCOMProxy* proxy);

	/**
	 * Drop an XPCOM Proxy
	 */
	void dropProxy(ErlXPCOMProxy *proxy);
	
	/**
	 * Search the proxy by the nsISupports pointer.
	 * @param obj proxy to search
	 * @param oid where result will be write
	 * @return true if found false if not
	 */ 
	bool searchProxy(nsISupports* obj, lfOID *oid);

	/**
	 * Search the proxy by the oid and the iid
	 * Returns 0 if not found.
	 */ 
	ErlXPCOMProxy* searchProxy(lfOID oid, nsIID &iid);

protected:
	/**
	 * A remote object can be referenced by more than one 
	 * proxy object. So we need to count how many proxies
	 * are pointing the remote object to known when we can drop
	 * the remote object.
	 * This methods are accesed from the registerProxy and dropProxy methods.
	 */
	void addrefRemote(const lfOID oid);
	void releaseRemote(const lfOID oid);

private:
	PRLogModuleInfo *log;
	
	RequestTransport* transport;
	lfOID oidCounter;
	lfCallID callIdCounter;

	stub_map stubs;		
	stub_inverse_map inverseStubs;		
	proxy_map proxies;		
	proxy_inverse_map inverseProxies;		
	
	// We will have a counter for each remote object. see 
	// addrefRemote() and releaseRemote()
	remote_counter_map remoteCounters;

	// Lock for stub maps.
	PRLock *stubLock;

	// Lock for proxy maps.
	PRLock *proxyLock;
	
};

} // namespace erlxpcom 

#endif // __ERLANGORB_H

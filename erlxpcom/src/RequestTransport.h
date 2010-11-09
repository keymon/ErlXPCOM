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
#ifndef __REQUESTTRANSPORT_H
#define __REQUESTTRANSPORT_H

#include <memory>
#include <string>
#include <map>

#include <epi.hpp>

#include <nscore.h>

#include "NSPRUtils.h"
#include "ErlXPCOMException.h"
#include "ErlXPCOMLog.h"
#include "ErlXPCOMDefs.h"
#include "ErlangOrb.h"

namespace erlxpcom {

using namespace epi::node;
using namespace epi::type;

class RequestTransportAcceptor;

/** 
 * Observer that will receive incoming replies
 */
class ReplyObserver {
public:
	virtual void reply(lfCallID callID, ErlTuple* reply) = 0;
};

class RequestTransport {
	typedef std::map<lfCallID, ReplyObserver*> reply_observer_map;
	friend class RequestTransportAcceptor;
public:

	/** 
	 * Create a new Request Transport 
	 * @param mailbox Mailbox to use to comunicate with Erlang side
	 * @param theErlangServerPid pid of the server process in Erlang side, 
	 *	destination of calls and replies
	 */
	RequestTransport(MailBox* mailbox, ErlPid* theErlangServerPid);

	virtual ~RequestTransport();
	
	/** 
	 * Send a call to Erlang side. 
	 * @param oid Erlang object ID 
	 * @param callID unique call ID
	 * @param methodName method name to call
	 * @param inParams parameter list
	 * @param type call type (call method, set/get attribute)
	 * @param replyObserver Pointer to reply observer instance, observer
	 *		which will be called when the reply for this callId arrives 
	 * @throw ErlangException if there is a fail in comunication
	 *		with erlang node.
	 */
	void sendCall(lfOID oid, lfCallID callID, 
				  std::string methodName, 
				  ErlList* inParams,
				  call_type type,
				  ReplyObserver *replyObserver)
		throw (ErlangException);
	
	/**
	 * Send the reply for a call to the Erlang node
	 * @param callID unique ID of call
	 * @param reply tuple with reply to send
	 * @throw ErlangException if there is a fail in comunication
	 *		with erlang node.
	 */ 
	void sendReply(lfCallID callID, ErlTuple* reply)
		throw (ErlangException);
	
	/**
	 * Process an incoming call request to a XPCOM object. This method is 
	 * asynchronous.
	 * @param oid Erlang object ID 
	 * @param callID unique call ID
	 * @param methodName method name to call
	 * @param inParams parameter list
	 * @param type call type (call method, set/get attribute)
	 * @throw InternalErrorException if there is an error 
	 *		launching the call.
	 * @throw UnknownMethodException if the method is not implemented by 
	 *      this object
	 */
	void dispatchCall(lfOID oid, lfCallID callID, 
					  std::string methodName, ErlList* inParams, call_type type)
		throw (InternalErrorException, UnknownMethodException);
	
	/**
	 * Process an incoming reply for a sent call. This method is 
	 * asynchronous.
	 * @param callID unique ID of call
	 * @param reply tuple with reply to send
	 * @throw ErlXPCOMConnectionException if there is a fail in comunication
	 *		with erlang node.
	 */
	void dispatchReply(lfCallID callID, ErlTuple* reply)	
		throw (InternalErrorException);
	
	/**
	 * Drop remote erlang object
	 */
	void dropRemoteObject(lfOID oid)
		throw (InternalErrorException);
	
	/** 
	 * Start the thread that will wait and process incoming messages 
	 * @throw InternalErrorException if the orb is not defined
	 */
	void start() throw (InternalErrorException);

	/** Stop the listener thread  */
	void stop();
	
	inline void setOrb(ErlangORB *orb) {
		this->orb = orb;
	}

	inline ErlangORB * getOrb() {
		return orb;
	}

protected:
	// Log module info 
	PRLogModuleInfo *log;
		
private: 

	// The destination ORB of incoming messages
	ErlangORB* orb;
	
	// This class is not the owner of this mailbox.
	MailBox* mailbox;
	
	// The remote erlang process to send the messages
	ErlTermPtr<ErlPid> erlangServerPid;
	
	RequestTransportAcceptor* acceptor;

	reply_observer_map replyObservers;
	
	// Lock for reply observer map.
	PRLock *replyObserverLock;

};

} // namespace erlxpcom

#endif // __REQUESTTRANSPORT_H

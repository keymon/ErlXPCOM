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
#include <Config.hpp>

#include <iostream>

#include "NSPRUtils.h"
#include "ErlXPCOMLog.h"
#include "ErlXPCOMHelper.h"
#include "RequestTransport.h"

using namespace epi::type;
using namespace epi::node;
using namespace epi::error;

namespace erlxpcom {
/**
 * This class implements the thread that will listen and receive messages 
 * from remote erlang node, and forwarding them to the correct method.
 */ 
class RequestTransportAcceptor: public NSPRThread {
public:
	RequestTransportAcceptor(MailBox* aMailbox, RequestTransport* aTransport);
	
	virtual ~RequestTransportAcceptor();
	
	/** Stop the thread */
	void stop();
	
	/** RequestTransportAcceptor main loop */
	void run(); 
	
private:
	MailBox* mailbox;
	RequestTransport* transport;
	bool threadExit;
	PRLogModuleInfo *log;
	
};

} // namespace erlxpcom

using namespace erlxpcom;

RequestTransportAcceptor::RequestTransportAcceptor(MailBox* aMailbox, 
	RequestTransport* aTransport):
	NSPRThread(), mailbox(aMailbox), transport(aTransport)
{
	log = aTransport->log;
}

RequestTransportAcceptor::~RequestTransportAcceptor() {
	this->stop();
}

void RequestTransportAcceptor::stop() {
	threadExit = true;
	this->join();
}


void RequestTransportAcceptor::run() {
	ErlTermPtr<ErlTuple> msgCall;
	ErlTermPtr<ErlTuple> msgGet;
	ErlTermPtr<ErlTuple> msgSet;
	ErlTermPtr<ErlTuple> msgReply;
	ErlTermPtr<ErlTuple> msgDrop;
	try {
		
		// ErlTuples to do pattern matching  
		// Fixme. I don't like this way, study modify the EPI library
		msgCall.reset(new ErlTuple(5));
		msgCall->initElement(new ErlAtom("call_method")); 
		msgCall->initElement(new ErlVariable("ObjectId"));
		msgCall->initElement(new ErlVariable("CallId"));
		msgCall->initElement(new ErlVariable("MethodName"));
		msgCall->initElement(new ErlVariable("Params"));

		msgGet.reset(new ErlTuple(5));
		msgGet->initElement(new ErlAtom("get_attribute")); 
		msgGet->initElement(new ErlVariable("ObjectId"));
		msgGet->initElement(new ErlVariable("CallId"));
		msgGet->initElement(new ErlVariable("MethodName"));
		msgGet->initElement(new ErlVariable("Params"));

		msgSet.reset(new ErlTuple(5));
		msgSet->initElement(new ErlAtom("set_attribute")); 
		msgSet->initElement(new ErlVariable("ObjectId"));
		msgSet->initElement(new ErlVariable("CallId"));
		msgSet->initElement(new ErlVariable("MethodName"));
		msgSet->initElement(new ErlVariable("Params"));

		msgReply.reset(new ErlTuple(3));
		msgReply->initElement(new ErlAtom("reply")); 
		msgReply->initElement(new ErlVariable("CallId"));
		msgReply->initElement(new ErlVariable("Reply"));

		msgDrop.reset(new ErlTuple(2));
		msgDrop->initElement(new ErlAtom("drop_object")); 
		msgDrop->initElement(new ErlVariable("ObjectId"));

	} catch (EpiException &e) {
		// TODO what to do here? :?
		PR_LOG( log, PR_LOG_DEBUG, 
			("RequestTransportAcceptor: Exception catched before init loop:%s\n", 
			 e.getMessage().c_str())); 
		return;
	} 			
	// Count exceptions catched
	int errorCounter = 0;

	PR_LOG( log, PR_LOG_DEBUG, ("RequestTransportAcceptor: loop started\n"));

	ErlTermPtr<> receivedTerm;
	VariableBinding binding;
	threadExit = false;
	do {
		try {
		// Receive a message each 100ms
		receivedTerm.reset(mailbox->receive(200));
		if (receivedTerm.get() != 0) {	
			PR_LOG( log, PR_LOG_DEBUG, 
				("RequestTransportAcceptor: <== %s\n", 
				receivedTerm->toString().c_str()));
			// {call_method, ...} | {get_attribute,...} | {set_attribute...}
			bool iscall = false, isget = false, isset = false;
			
			if ((iscall = msgCall->match(receivedTerm.get(), &binding)) ||
				(isget = msgGet->match(receivedTerm.get(), &binding)) ||
				(isset = msgSet->match(receivedTerm.get(), &binding))) {
				// Get the call parameters
				lfOID oid = 
					ErlXPCOMHelper::OIDFromErlang(
						ErlLong::cast(binding.search("ObjectId")));

				lfCallID callid = 
					ErlXPCOMHelper::CallIDFromErlang(
						ErlLong::cast(binding.search("CallId")));
				const std::string& method_name = 
					ErlAtom::cast(binding.search("MethodName"))->atomValue();
				ErlList *params = 
					ErlList::cast(binding.search("Params"));
					
				// dispatch the call
				try {
					call_type type; 
					if (iscall) type = CALL_METHOD;
					if (isget) type = GET_ATTRIBUTE;
					if (isset) type = SET_ATTRIBUTE;
					transport->dispatchCall(oid, callid, method_name, params, type);
				} catch (UnknownMethodException &e) {
					transport->sendReply(callid, 
						new ErlTuple(new ErlAtom("error"), 
									 new ErlTuple(new ErlAtom("unknown_method"),
												  new ErlString(e.getMethodName().c_str()))));
				} catch (InternalErrorException &e) {
					// FIXME: improve this
					transport->sendReply(callid, 
								new ErlTuple(new ErlAtom("error"), 
											 new ErlLong(NS_ERROR_FAILURE)));
				}
			} 
			// {reply, ...}
			else if(msgReply->match(receivedTerm.get(), &binding)) {
				// Get the reply parameters
				lfCallID callid = 
					ErlXPCOMHelper::OIDFromErlang(
						ErlLong::cast(binding.search("CallId")));
				ErlTuple *reply = 
					ErlTuple::cast(binding.search("Reply"));
				// dispatch the reply
				try {
					transport->dispatchReply(callid, reply);
				} catch (InternalErrorException &e) {
					// FIXME: improve this
					transport->sendReply(callid, 
								new ErlTuple(new ErlAtom("error"), 
											 new ErlLong(NS_ERROR_FAILURE)));
				}
			} 
			else if (msgDrop->match(receivedTerm.get(), &binding)) {
				// Get the call parameters
				lfOID oid = 
					ErlXPCOMHelper::OIDFromErlang(
						ErlLong::cast(binding.search("ObjectId")));
				PR_LOG( log, PR_LOG_DEBUG, 
					("RequestTransportAcceptor: Dropping object:%i\n", oid)); 
				// Drop the stub and deleting it
				ErlXPCOMStub* stub = transport->getOrb()->dropStub(oid);
				delete stub;
			} else {
				PR_LOG( log, PR_LOG_WARN, 
					(" -- Unknown message received")); 
			}
			errorCounter = 0;
		}
		} catch (EpiException &e) {
			// TODO Implement a better error management
			PR_LOG( log, PR_LOG_DEBUG, 
				("RequestTransportAcceptor: Exception catched in loop: %s, continuing\n", 
				 e.getMessage().c_str())); 
			errorCounter++;
			if (errorCounter > 10) {
				PR_LOG( log, PR_LOG_DEBUG, ("-- Too much errors, exiting loop")); 
				threadExit = true;
			}
		} 			
		// reset the binding
		binding.reset();
	} while (!threadExit);
	PR_LOG( log, PR_LOG_DEBUG, ("RequestTransportAcceptor: exit loop\n"));
		
}

///////////////////////////////////////////////////////////////////////////////
RequestTransport::RequestTransport(MailBox* aMailbox, 
								   ErlPid* theErlangServerPid): 
	log(ErlXPCOMLog::getLog()), mailbox(aMailbox), 
	erlangServerPid(theErlangServerPid), acceptor(0), 
	replyObserverLock(PR_NewLock())
{
}

RequestTransport::~RequestTransport() {}

void RequestTransport::sendCall(lfOID oid, lfCallID callId, 
			  std::string methodName, ErlList* inParams, call_type type,
			  ReplyObserver *replyObserver)
	throw (ErlangException)
{
	NSPRScopedLock _scopedLock(replyObserverLock);
	// add observer if necesary
	if (replyObserver) {
		replyObservers[callId] = replyObserver;
	}
	_scopedLock.unlock();
	
	try {
		// Construct the call tuple 
		ErlTermPtr<ErlTuple> callTuple(new ErlTuple(5));
		switch (type) {
		case CALL_METHOD:
			callTuple->initElement(new ErlAtom("call_method"));
			break;
		case GET_ATTRIBUTE:
			callTuple->initElement(new ErlAtom("get_attribute"));
			break;
		case SET_ATTRIBUTE:
			callTuple->initElement(new ErlAtom("set_attribute"));
			break;
		}
		callTuple->initElement(ErlXPCOMHelper::OIDToErlang(oid));
		callTuple->initElement(ErlXPCOMHelper::CallIDToErlang(callId));
		callTuple->initElement(new ErlAtom(methodName.c_str()));
		callTuple->initElement(inParams);

		PR_LOG( log, PR_LOG_DEBUG, 
			("RequestTransportAcceptor: ==> %s\n", 
			callTuple->toString().c_str()));
		
		mailbox->send(erlangServerPid.get(), callTuple.get());
		
	} catch(EpiException &e) {
		throw ErlangException("Send call error", e);
	}
}

void RequestTransport::sendReply(lfCallID callID, ErlTuple* reply)
	throw (ErlangException)
{
	try {
		// Construct the call tuple 
		ErlTermPtr<ErlTuple> replyTuple(new ErlTuple(3));
		replyTuple->initElement(new ErlAtom("reply"));
		replyTuple->initElement(ErlXPCOMHelper::CallIDToErlang(callID));
		replyTuple->initElement(reply);
		
		PR_LOG( log, PR_LOG_DEBUG, 
			("RequestTransportAcceptor: ==> %s\n", 
			replyTuple->toString().c_str()));

		mailbox->send(erlangServerPid.get(), replyTuple.get());
	} catch(EpiException &e) {
		throw ErlangException("Send call error", e);
	}
}

void RequestTransport::dropRemoteObject(lfOID oid)
		throw (InternalErrorException) 
{
	try {
		ErlTermPtr<ErlTuple> dropTuple(new ErlTuple(2));
		dropTuple->initElement(new ErlAtom("drop_object"));
		dropTuple->initElement(ErlXPCOMHelper::OIDToErlang(oid));
		mailbox->send(erlangServerPid.get(), dropTuple.get());
	} catch(EpiException &e) {
		throw ErlangException("Send call error", e);
	}
}


void RequestTransport::dispatchCall(lfOID oid, lfCallID callid,
				  std::string methodName, ErlList* inParams, call_type type)
	throw (InternalErrorException, UnknownMethodException) 
{
	ErlXPCOMStub *stub = orb->getStub(oid);

	Call *call = stub->createCall(callid, methodName, inParams, type);

	try {
		call->call();
	} catch (AlreadyExecutedException &e) {
		// The call have been executed already, return an InternalError (?)
		throw InternalErrorException(e.getMessage());
	} catch (InterfaceMismatchException &e) {
		// Interface mismatch return a failure
		sendReply(callid, 
			ErlXPCOMHelper::buildFailureReplyTuple(NS_ERROR_INVALID_ARG));		
	}
	
	// TODO: not implemented
}

void RequestTransport::dispatchReply(lfCallID callId, ErlTuple* reply)	
	throw (InternalErrorException) 
{
	NSPRScopedLock _scopedLock(replyObserverLock);

	// search the reply observer
    if (replyObservers.count(callId)) {
		// Call the observer callback
		ReplyObserver *observer = replyObservers[callId];
		observer->reply(callId, reply);
		// delete the observer
		replyObservers.erase(callId);
    } else {
		PR_LOG( log, PR_LOG_DEBUG, 
			("RequestTransport::dispatchReply() no observer for callId", 
			callId));
    }	
	
}

void RequestTransport::start() throw (InternalErrorException) {
	// check if there is an orb defined
	if (orb == 0) {
		throw InternalErrorException("There is not ORB defined");
	}

	if(acceptor == 0) {
		acceptor = new RequestTransportAcceptor(mailbox, this);
	}
	acceptor->start();	
}

void RequestTransport::stop() {
	acceptor->stop();	
}






#include <nscore.h>

#include "XPCOMCall.h"
#include "ErlXPCOMHelper.h"

#include "NSPRUtils.h"

using namespace erlxpcom;

XPCOMCall::XPCOMCall(ErlXPCOMStub *_xpcomStub, lfCallID callID, 
					 std::string methodName, ErlList* _inParams, call_type _type) 
	throw (InternalErrorException,
		 InterfaceMismatchException,
		 UnknownMethodException):
	Call(_xpcomStub->getOID(), callID, methodName),
	xpcomStub(_xpcomStub), orb(_xpcomStub->getORB()), type(_type)
	//,callLock(PR_NewLock()), callCond(PR_NewCondVar(callLock))
{
    log = ErlXPCOMLog::getLog();

	PR_ASSERT(_inParams);
	PR_ASSERT(xpcomStub);

	inParams.reset(_inParams);
	
	// Get method info
	nsIInterfaceInfo *interfaceInfo = xpcomStub->getInterfaceInfo();

	nsresult result;
	nsXPTMethodInfo *_methodInfo;
	PRUint16 methodIndex;
	
	result = interfaceInfo->
				GetMethodInfoForName(methodName.c_str(), 
									 &methodIndex, 
									 (const nsXPTMethodInfo**) &_methodInfo);
	
	if (!NS_SUCCEEDED(result)) {
		PR_LOG(log, PR_LOG_DEBUG, (" `-> Failed in interfaceInfo->GetMethodInfoForName() -> unknown method"));
		throw UnknownMethodException(methodName); 
	}
	// If is a set attribute call, get the info of the next method. Setter
	// are ALWAYS next to getters.
	if (type == SET_ATTRIBUTE) {
		methodIndex++;
		result = interfaceInfo->GetMethodInfo(methodIndex, 
											  (const nsXPTMethodInfo**) &_methodInfo);
		// Check if success and if it's the same method name 
		if (!NS_SUCCEEDED(result) || methodName != _methodInfo->name) {
			PR_LOG(log, PR_LOG_DEBUG, 
				(" `-> Failed in interfaceInfo->GetMethodInfoForName() for setter -> unknown method"));
			throw UnknownMethodException(methodName); 
		}
	}
	
	// create the methodInfo
	methodInfo.reset(new ErlXPCOMMethodInfo(interfaceInfo, 
							_methodInfo, methodIndex));

	method.reset(new ErlXPCOMLocalMethod(methodInfo.get()));

	// Check if it is setter/getter
	switch(type) {
	case CALL_METHOD: 
		// No check for call method
		break;
	case GET_ATTRIBUTE:
		if (!methodInfo->isGetter()) {
			PR_LOG(log, PR_LOG_DEBUG, (" `-> Method is not getter -> unknown method"));
			throw UnknownMethodException(methodName); 
		}
		break;
	case SET_ATTRIBUTE:
		if (!methodInfo->isSetter()) {
			PR_LOG(log, PR_LOG_DEBUG, (" `-> Method is not setter -> unknown method"));
			throw UnknownMethodException(methodName); 
		}
		break;
	}

}

XPCOMCall::~XPCOMCall() {
//	PR_DestroyCondVar(callCond);
//	PR_DestroyLock(callLock);
}


void XPCOMCall::waitForReturn() 
	throw (InternalErrorException)
{
// Only for testing
//	NSPRScopedLock _scopedLock(callLock);
//	while(!isCompleted()) {
//		PR_WaitCondVar(callCond, PR_INTERVAL_NO_TIMEOUT);
//	}
}

void XPCOMCall::doCall()
	throw (InterfaceMismatchException, 
		   InternalErrorException)
{
    PR_LOG(log, PR_LOG_DEBUG, 
		("XPCOMCall::doCall(method=\"%s\", callId=%i)",
		methodInfo->getName().c_str(), callID));
	
	// Unmarshall in params ???? Must be here or in event process :?	
	ErlXPCOMMethodUnmarshaller unmarshaller(orb, true, inParams.get());
	
	method->processInParams(unmarshaller);
	
	// Check if the object is owned by current thread. So it will execute the
	// method directly or not. 
	if (xpcomStub->isOwnedByCurrentThread()) {
		PR_LOG(log, PR_LOG_DEBUG, (" `-> This thread owns the object"));
		this->doRealCall();
	} else {
		PR_LOG(log, PR_LOG_DEBUG, 
				(" `-> sending event to owning thread, with event queue: 0x%08x", 
				  xpcomStub->getEventQueue()));
		// Get the event queue from the stub
		nsCOMPtr<nsIEventQueue> stubEventQueue = xpcomStub->getEventQueue();
		
		// Create and send the event to owning thread. The owner is this.
		ErlXPCOMHelper::sendEventQueue(stubEventQueue, this, 
									   XPCOMCall::EventHandler,
									   XPCOMCall::DestroyHandler);
		// Now we can return
	}
}

void XPCOMCall::doRealCall()
	throw (InterfaceMismatchException, 
		   InternalErrorException)
{
	// Locks for testing: unsupported now
	// NSPRScopedLock _scopedLock(callLock);
    PR_LOG(log, PR_LOG_DEBUG, 
			("XPCOMCall::doRealCall(method=\"%s\", callId=%i): doing XPTC_InvokeByIndex",
			  methodInfo->getName().c_str(), callID));

	nsresult result = 
		XPTC_InvokeByIndex(xpcomStub->getXPCOMObject(), 
						   methodInfo->getMethodIndex(),
						   methodInfo->getParamCount(), 
						   method->getVariants());
	// Check if it is success, calling the correct return method.
	if (NS_SUCCEEDED(result)) {
		PR_LOG(log, PR_LOG_DEBUG, 
			("XPCOMCall::doRealCall(method=\"%s\", callId=%i): returned success",
			  methodInfo->getName().c_str(), callID));

		// Unmarshall the out data
		ErlXPCOMMethodMarshaller marshaller(orb, false, 
											methodInfo->getParamOutCount()-
											methodInfo->getParamOutSizeCount());
		method->processOutParams(marshaller);
		
		ErlTermPtr<ErlList> outParams(marshaller.getParams());
		
		// WARNING HACK!! 
		// The QueryInterface call returns an addref object, but reference 
		// counters in erlang and mozilla are separated. We must decrement
		// the counter after the QueryInterface call. The ORB will mantain
		// the uniq reference from erlang side
		if (methodInfo->getMethodIndex() == 0) { // If is QueryInterface
			nsXPTCVariant *variant = (nsXPTCVariant*)method->getVariants()[1].ptr;
			nsISupports *obj = (nsISupports*) variant->val.p;
			NS_RELEASE(obj);
		}
		
		// return success
		this->returnSuccess(outParams.get());
		
	} else {
		PR_LOG(log, PR_LOG_DEBUG, 
			("XPCOMCall::doRealCall(method=\"%s\", callId=%i): returned failure",
			  methodInfo->getName().c_str(), callID));
		this->returnFailure(result);
	}
}

void XPCOMCall::doReturn() 
	throw (InternalErrorException)
{
	/** 
	 *  WARNING; WARNING; WARNING; WARNING; WARNING; WARNING; WARNING; WARNING; 
	 *  This method deletes this instance. AND the lock must be freed first!
	 */ 
	std::auto_ptr<XPCOMCall> delete_me(this);
	
	try { 
		// Get the transport
		RequestTransport* transport = orb->getRequestTransport();

		// Create the reply tuple for success/fail case and send it
		if (this->isSuccess()) {
			ErlTermPtr<ErlTuple> replyTuple(
				ErlXPCOMHelper::buildSuccessReplyTuple(this->getOutParams()));
			transport->sendReply(this->getCallID(), replyTuple.get());
		} else {
			ErlTermPtr<ErlTuple> replyTuple(
				ErlXPCOMHelper::buildFailureReplyTuple(this->getResult()));
			
			transport->sendReply(this->getCallID(), replyTuple.get());
		}
		completed = true;
		// Notify waiters 
		//PR_NotifyAllCondVar(callCond);
	} catch (EpiException &e) {
		PR_LOG(log, PR_LOG_ERROR, 
			("XPCOMCall::doReturn(): exception building reply: %s\n", 
				e.what()));
		throw ErlangException("Error building reply", e);
	}
	
}

void* XPCOMCall::EventHandler(PLEvent *event) {
    PRLogModuleInfo *log = ErlXPCOMLog::getLog();

	/* Get the call from the pointer to event owner */
    XPCOMCall* call = (XPCOMCall*)PL_GetEventOwner(event);
	PR_ASSERT(call);
	
	/* Execute real call */
	try {
		call->doRealCall();
	} catch (std::exception &e) {
		PR_LOG(log, PR_LOG_ERROR, ("-- XPCOMCall::EventHandler(): catched exception \"%s\"", e.what()));
	}

    return NULL;
}

void XPCOMCall::DestroyHandler(PLEvent *event) {
	// Delete the event after execution. 
	delete event;
}


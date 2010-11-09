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
#ifndef __CALL_H
#define __CALL_H

#include <epi.hpp>
#include <nscore.h>

#include "ErlXPCOMDefs.h"
#include "ErlXPCOMException.h"

using namespace epi::node;
using namespace epi::type;
using namespace epi::error;

namespace erlxpcom {

class Call {
public:
	Call(lfOID oid, lfCallID callID, std::string methodName);
	
	inline virtual ~Call() {}
	
	/**
	 * Get the list of in parameters of the call
	 */ 
	inline ErlList* getInParams() {
		return inParams.get();
	}
	/**
	 * Get the list of out parameters. Can be 0 if call fails.
	 */
	inline ErlList* getOutParams() {
		return outParams.get();
	}

	/**
	 * Get object id 
	 */ 
	inline lfOID getOID() {
		return oid;
	}
	
	/**
	 * get the interface id
	 */
//	inline nsIID getIID() {
//		return iid;
//	}
	
	/**
	 * get the callID
	 */
	inline lfCallID getCallID() {
		return callID;
	}
	
	/** 
	 * Check if call finished successfull or not. This method has not sense if 
	 * call is not completed.
	 */
	inline bool isSuccess() {
		return result == NS_OK;
	}
	
	/**
	 * Check if call is finish (a the return method have been 
	 * executed successfully)
	 */
	inline bool isCompleted() {
		return completed;
	}
	
	/**
	 * Check if the call have been launched (call() method called sucessfully)
	 */
	inline bool isLaunched() {
		return launched;
	}
	
	/**
	 * get the returned result of the call
	 */
	inline nsresult getResult() {
		return result;
	}
	
	/**
	 * Set the call state to returned and success with given outParams and NS_OK as
	 * result. 
	 * This method will call the doReturn() template method.
	 * @param list of out params
	 * @throw AlreadyFinishedExcepcion if this method is called after
	 * other call to returnSuccess or returnFailure
	 * @throw InternalErrorException if there is an internal error while
	 *  doing return
	 */
	void returnSuccess(ErlList* outParams) 
		throw(AlreadyExecutedException, InternalErrorException);
	
	/**
	 * Set the call state to returned and fail with given result. 
	 * This method will call the doReturn() template method.
	 * @param returned result
	 * @throw AlreadyFinishedExcepcion if this method is called after
	 * other call to returnSuccess or returnFailure
	 * @throw InternalErrorException if there is an internal error while
	 *  doing return
	 */
	void returnFailure(nsresult result)
		throw(AlreadyExecutedException, InternalErrorException);

	/**
	 * Execute this call. This method does not lock 
	 * This method delegates in doCall() method.
	 * @throw AlreadyExecutedException if this method have been executed
	 *	sucessfully already.
	 */
	void call()
		throw (AlreadyExecutedException, 
			   InterfaceMismatchException, 
			   InternalErrorException);
			   
	/**
	 * Wait until the call finalizes. This method will lock the current
	 * thread until a return method is executed. 
	 */
	virtual void waitForReturn() throw (InternalErrorException) = 0;

protected:
	
	/**
	 * Template method. Code that must be execute to execute the call.
	 * It will unmarshall/marshall the input parameters and send them
	 * to the object. This method is called by the call() method.
	 * @throw AlreadyExecutedException if this method had been executed
	 *  already.
	 * @throw InterfaceMismatchException if the params does not 
	 *	match with the signature of the method.
	 * @throw InternalErrorException if there is an error with the
	 *  XPCOM API or an error in object method call. 
	 */
	virtual void doCall() 
		throw (InterfaceMismatchException, 
			   InternalErrorException)= 0;
	
	/**
	 * Template method. Code that must be execute to return the result to 
	 * caller. This method is called by the methods returnSuccess() and 
	 * returnFailure().
	 * This method must set the completed flag
	 */
	virtual void doReturn() 
		throw (InternalErrorException) = 0;
	
	ErlTermPtr<ErlList> inParams;
	ErlTermPtr<ErlList> outParams;
	lfOID oid;
	lfCallID callID;
	std::string methodName;
	

//	nsIID iid;
	bool completed;
	bool launched;
	nsresult result;
	
};

} // namespace erlxpcom

#endif  // __CALL_H

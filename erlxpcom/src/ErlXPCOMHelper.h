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
#ifndef __ERLXPCOMHELPER_H
#define __ERLXPCOMHELPER_H

#include <epi.hpp>

// Event queues and related stuff
#include <nsIEventQueue.h>
#include <plevent.h>

#include "ErlXPCOMDefs.h"

namespace erlxpcom {

using namespace epi::type;

class ErlXPCOMHelper {
public:
	
	inline static lfOID OIDFromErlang(ErlLong* along) {
		return along->longValue();
	}
	inline static lfCallID CallIDFromErlang(ErlLong* along) {
		return along->longValue();
	}
	inline static ErlLong* OIDToErlang(lfOID oid) {
		return new ErlLong(oid);
	}
	inline static ErlLong* CallIDToErlang(lfCallID callID) {
		return new ErlLong(callID);
	}

	static ErlTuple *buildFailureReplyTuple(nsresult result);

	static ErlTuple *buildSuccessReplyTuple(ErlList *outParams);
	
	/**
	 * Send a event to an event queue. More info in PL_InitEvent() and 
	 *  nsIEventQueue::PostEvent() documentation.
	 * @param queue Destination event queue
	 * @param owner Pointer to owner of event
	 * @param handler Pointer to event handler function 
	 * @param destructor Pointer to event destruction  handler function
	 */
	static void sendEventQueue(nsIEventQueue* queue,
							   void* owner, 
							   PLHandleEventProc handler,
							   PLDestroyEventProc destructor);
};

}

#endif // __ERLXPCOMHELPER_H

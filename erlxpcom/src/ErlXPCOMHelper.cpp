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
#include <Config.hpp>

#include "ErlXPCOMHelper.h"

// Logging
#include "prlog.h"

using namespace erlxpcom;

void ErlXPCOMHelper::sendEventQueue(nsIEventQueue* queue,
									void* owner, 
									PLHandleEventProc handler,
									PLDestroyEventProc destructor) 
{
	PR_ASSERT(queue);
	PR_ASSERT(handler);
	PR_ASSERT(destructor);
	
	// Create and send the event to owning thread
	PLEvent *event = new PLEvent();
	PL_InitEvent(event, owner, handler, destructor);
	queue->PostEvent(event);	
}

ErlTuple *ErlXPCOMHelper::buildFailureReplyTuple(nsresult result) {
	return new ErlTuple(new ErlAtom("fail"), new ErlLong(result));
}

ErlTuple *ErlXPCOMHelper::buildSuccessReplyTuple(ErlList *outParams) {
	return new ErlTuple(new ErlAtom("ok"), outParams);
}


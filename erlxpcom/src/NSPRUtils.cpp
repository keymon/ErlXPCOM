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
#include "NSPRUtils.h"
#include "ErlXPCOMLog.h"
//////////////////////////////////////////////////////////////////////////

using namespace erlxpcom;
NSPRThread::NSPRThread():_thread(0) {
}

NSPRThread::~NSPRThread() {}

void NSPRThread::start() throw(NSPRThreadCreationFailedException) {
	if (_thread != 0) {
		throw NSPRThreadCreationFailedException();
	} 
	_thread = PR_CreateThread(PR_USER_THREAD, 
							  this->pr_run,
							  this, 
							  PR_PRIORITY_NORMAL, 
							  PR_LOCAL_THREAD,
							  PR_JOINABLE_THREAD,
							  0); 
	if (_thread == 0) {
		throw NSPRThreadCreationFailedException();
	}
}

bool NSPRThread::isRunning() {
	return _thread != 0;
}

int NSPRThread::join() {
	if (isRunning()) {
		PR_JoinThread(_thread);
		return 1;
	}
	return 0;
}

void NSPRThread::pr_run(void *arg) {
	NSPRThread *thread = (NSPRThread *) arg;
	thread->run();
	thread->_thread = 0;
}	

NSPRScopedLock::NSPRScopedLock(PRLock *_lock): lock(_lock) {
	PR_Lock(lock);
}

NSPRScopedLock::~NSPRScopedLock() {
	unlock();
}

void NSPRScopedLock::unlock() {
	if (lock) {
		PR_Unlock(lock);
		lock = 0;
	}
}


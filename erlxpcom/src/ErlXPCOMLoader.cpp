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
#include <epi.hpp>
#include <nsIModule.h>
#include <nsIFile.h>
#include <nsString.h>

#include <prmem.h>

#include "ErlXPCOMInit.h"

#include "ErlXPCOMLog.h"

using namespace erlxpcom;
static ErlXPCOMInit *erlXPCOMInit = NULL;

nsresult initErlXPCOM(nsIComponentManager *componentManager, 
					  const char* componentLocation) {
	if (erlXPCOMInit) {
		return NS_OK;
	}
	try {
		nsresult rv;
		nsIServiceManager *serviceManager;

		if (NS_FAILED(rv = NS_GetServiceManager(&serviceManager))) {
			throw XPCOMException("Can't get service manager", rv);
		}

		erlXPCOMInit = new ErlXPCOMInit(componentLocation);
		erlXPCOMInit->launchErlang();
		erlXPCOMInit->init(componentManager, serviceManager);
		
		return NS_OK;
		
	} catch (ErlXPCOMException e) {
		PR_LOG(ErlXPCOMLog::getLog(), PR_LOG_ERROR, 
			("Can't initialize ErlXPCOM, Exception: %s", 
			 e.getMessage().c_str()));
		delete erlXPCOMInit;
		erlXPCOMInit = 0;
		return NS_ERROR_FAILURE;
	}
}

extern "C" NS_EXPORT nsresult NSGetModule(nsIComponentManager *componentManager,
                                          nsIFile* location,
                                          nsIModule** result)
{

	nsString aPath;
	
	if(NS_FAILED(location->GetPath(aPath))) {
        return NS_ERROR_FACTORY_NOT_LOADED;
    }
	
	char *locationPath = ToNewCString(aPath);
	
    if(NS_FAILED(initErlXPCOM(componentManager, locationPath))) {
		PR_Free(locationPath);
        return NS_ERROR_FACTORY_NOT_LOADED;
    }

	PR_Free(locationPath);

    nsIModule * module = erlXPCOMInit->getLoaderModule();
	*result = module;
//    nsresult rv;
//	rv = module->QueryInterface(nsIModule::GetIID(), (void**) result);	
//    if(NS_FAILED(rv))
//    {
//        result = nsnull;
//        NS_ASSERTION(PR_FALSE, "QueryInterface module loader failed");
//        return NS_ERROR_FACTORY_NOT_LOADED;
//    }
    return NS_OK;
}

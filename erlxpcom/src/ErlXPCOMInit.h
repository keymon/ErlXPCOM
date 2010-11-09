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
#include <memory>

#include <nsIModule.h>
#include <nsIComponentManager.h>
#include <nsIServiceManager.h>

#include "ErlangOrb.h"
#include "ErlXPCOMException.h"
#include "ErlXPCOMLog.h"

#include <prproces.h>

#define ERLXPCOM_BEANS_EXT ".beams"

namespace erlxpcom {

/** 
 * ErlXPCOM initializer. It connects to remote node and sets ErlXPCOM
 */
class ErlXPCOMInit {
//	/** environment variable with default local node name */
//	static const char *LOCAL_NODENAME_VAR;
//	/** environment variable with default remote node name */
//	static const char *REMOTE_NODENAME_VAR;
//	/** environment variable with default cookie */
//	static const char *COOKIE_VAR;
//	/** environment variable with flag to launch erlang or not. 0 to not launch */
//	static const char *LAUNCH_ERLANG_VAR;
//	/** environment variable with command and arguments to launch erlang */
//	static const char *ERLANG_COMMAND_VAR;
//	static const char *LETTER_POOL;
public:
	
	ErlXPCOMInit(std::string _componentLocation);
	
	virtual ~ErlXPCOMInit();
	
	/** 
	 * Launch/Ping the erlang node. 
	 * This function will 
	 * It will initialize the local node and launch a new erlang node using 
	 * fork, with a random cookie and nodename, or connect to a concrete node 
	 * with a concrete name or cookie acording configuration.
	 * 
	 * @throws InternalErrorException if there is a failure while launching
	 *	erlang node. 
	 */ 
	void launchErlang()	
		throw (InternalErrorException);
	
	/** 
	 * Connect to remote node and init ErlXPCOM Orb
	 */
	void init(nsIComponentManager *ComponentManager, 
			  nsIServiceManager *ServiceManager) 
		throw (InternalErrorException);
		
	/**
	 * Gets the module for the Erlang Component Loader. 
	 * The module is an addref'd object.
	 * That's a remote object.
	 */
	nsIModule* getLoaderModule()
		throw (InternalErrorException);

	/**
	 * Shutdown.
	 * Stops the remote erlang node and destroys the orb and all the objects.
	 */
	void shutdown() 
		throw (InternalErrorException);

	inline ErlangORB *getOrb() {
		return orb.get();
	}

private:
	PRLogModuleInfo *log;

	/** 
	 * Get the erlang node name. this method gets a erlang node name from
	 * configuration or creates a random one 
	 */ 
	std::string getErlangNodeName();

	/** 
	 * Get the local node name. this method gets a local node name from
	 * configuration or creates a random one 
	 */ 
	std::string getLocalNodeName();

	/** 
	 * Get the erlang cookie. this method gets a cookie name from
	 * configuration or creates a random one 
	 */ 
	std::string getCookie();

	/**
	 * Check the configuration if ErlXPCOM must launch a erlang node or not.
	 * and launch it if necesary. 
	 * @throw InternalErrorException if can't fork the process
	 */
	void forkErlangRuntime() throw(InternalErrorException);
	
	nsIModule* loaderModule;

	std::auto_ptr<ErlangORB> orb;
	std::auto_ptr<AutoNode> node;
	std::auto_ptr<std::string> localNodeName;
	std::auto_ptr<std::string> remoteNodeName;
	std::auto_ptr<std::string> cookie;
	
	PRProcess *erlang_runtime;

	std::string componentLocation;
	std::string beamLocation;
	
	
};

}

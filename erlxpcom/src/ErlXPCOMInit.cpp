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
#include <string>

#include <prenv.h>
#include <prrng.h>
#include <prsystem.h>
#include <prnetdb.h>
#include <stdlib.h>
#include <string.h>

#include <sys/types.h>
#include <unistd.h>


#include "ErlXPCOMInit.h" 
#include "RequestTransport.h"

#include "command_line_tokenizer.h"

using namespace erlxpcom;
using namespace epi::node;
using namespace epi::type;
using namespace epi::error;

/** environment variable with default local node name */
const char *LOCAL_NODENAME_VAR = "ERLXPCOM_LOCAL_NODENAME";
/** environment variable with default remote node name */
const char *REMOTE_NODENAME_VAR = "ERLXPCOM_REMOTE_NODENAME";
/** environment variable with default cookie */
const char *COOKIE_VAR = "ERLXPCOM_COOKIE";
/** environment variable with flag to launch erlang or not. 0 to not launch */
const char *LAUNCH_ERLANG_VAR = "ERLXPCOM_LAUNCH_ERLANG";
/** environment variable with command and arguments to launch erlang */
const char *ERLANG_COMMAND_VAR = "ERLXPCOM_ERLANG_COMMAND_VAR";
const char *LETTER_POOL = "0123456789qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM";


ErlXPCOMInit::ErlXPCOMInit(std::string _componentLocation): 
	log(ErlXPCOMLog::getLog()), erlang_runtime(NULL), 
	componentLocation(_componentLocation)
{
	// FIXME use portable file separator
	// get the beamLocation
	std::string::size_type pos = componentLocation.find_last_of(".");
	std::ostringstream oss;
	oss << componentLocation.substr(0, pos) << ERLXPCOM_BEANS_EXT;
	beamLocation = oss.str();
}

ErlXPCOMInit::~ErlXPCOMInit() {
	int exitcode;
	if (erlang_runtime) {
		PR_KillProcess(erlang_runtime);
		PR_WaitProcess(erlang_runtime, &exitcode); 
	}
}

std::string getFullyHostName() {
	char hostname[SYS_INFO_BUFFER_LENGTH+1];
	PR_GetSystemInfo(PR_SI_HOSTNAME, hostname, SYS_INFO_BUFFER_LENGTH);
	PRHostEnt hostEnt;
	char buffer[PR_NETDB_BUF_SIZE+1];
	PR_GetHostByName(hostname, buffer, PR_NETDB_BUF_SIZE, &hostEnt);
	
	return std::string(buffer);
}

std::string ErlXPCOMInit::getErlangNodeName() {
	if (!remoteNodeName.get()) {
		char *remotenodename;
		// Get the name from environment
		if ((remotenodename = PR_GetEnv(REMOTE_NODENAME_VAR))) {
			remoteNodeName.reset(new std::string(remotenodename));
		} else {
			// Or create a new one: erlang-<threadid>@<hostname>
			std::ostringstream oss;	
			oss << "erlang" << (unsigned int) getpid() << 
				"@" << getFullyHostName();
			remoteNodeName.reset(new std::string(oss.str()));
		}
	}
	return *remoteNodeName;
}

std::string ErlXPCOMInit::getLocalNodeName() {
	if (!localNodeName.get()) {
		char *localnodename;
		// Get the name from environment
		if ((localnodename = PR_GetEnv(LOCAL_NODENAME_VAR))) {
			localNodeName.reset(new std::string(localnodename));
		} else {
			// Or create a new one: mozilla-<threadid>@<hostname>
			char hostname[SYS_INFO_BUFFER_LENGTH+1];
			PR_GetSystemInfo(PR_SI_HOSTNAME, hostname, SYS_INFO_BUFFER_LENGTH);
			std::ostringstream oss;	
			oss << "mozilla"<< (unsigned int) getpid() << 
				"@" << getFullyHostName();
			localNodeName.reset(new std::string(oss.str()));
		}
	}
	return *localNodeName;

}

std::string ErlXPCOMInit::getCookie() {
	if (!cookie.get()) {
		char *env_cookie;
		// Get the cookie from environment
		if ((env_cookie = PR_GetEnv(COOKIE_VAR))) {
			cookie.reset(new std::string(env_cookie));
		} else {
			// Or create a new random one
			int cookiesize = 8;
			unsigned char noise[cookiesize];
			char random_cookie[cookiesize+1];
			PR_GetRandomNoise(noise, cookiesize);
			int poolsize = strlen(LETTER_POOL);
			for (int i=0; i<cookiesize; i++) {
				random_cookie[i] = LETTER_POOL[noise[i]%poolsize];
			}
			random_cookie[cookiesize] = 0;
			cookie.reset(new std::string(random_cookie));
		}
	}
	return *cookie;
}

void ErlXPCOMInit::forkErlangRuntime() throw(InternalErrorException) {

	if (erlang_runtime) {
		return;
	}
	
	std::ostringstream oss;	

	char* launch_erlang;
	if ((launch_erlang = PR_GetEnv(LAUNCH_ERLANG_VAR))) {
		if (launch_erlang[0] == '0' && launch_erlang[1] == 0)
			return;
	}
	
	char* erlangcommand;
	
	if (!(erlangcommand = PR_GetEnv(ERLANG_COMMAND_VAR))) {
		erlangcommand = "erl";
	}

	oss << erlangcommand << 
		" -name " << getErlangNodeName() << 
		" -setcookie " << getCookie() << 
		" -pa " << beamLocation <<
		" -noinput";
	
	
	PR_LOG(log, PR_LOG_ERROR, 
		("ErlXPCOMInit: Executing: %s", oss.str().c_str()));

	char** argv = comand_line_tokenizer(oss.str().c_str());

	erlang_runtime = PR_CreateProcess(argv[0], argv, NULL, NULL);
	
	argv_free(argv);

	if (!erlang_runtime) {
		throw InternalErrorException("Can't fork erlang runtime");
	}
	
}

void ErlXPCOMInit::launchErlang() throw (InternalErrorException) {
	try {
		std::string localnode = getLocalNodeName();
		std::string remotenode = getErlangNodeName();
		std::string cookie = getCookie();
		
		PR_LOG(log, PR_LOG_DEBUG, 
			("ErlXPCOMInit::launchErlang(): Creating node and connecting remote\n"
			 "  `-> Local: %s | Remote: %s | Cookie: %s", 
			 localnode.c_str(), remotenode.c_str(), cookie.c_str()));

		forkErlangRuntime();
	
		// Create the node
		
		node.reset(new AutoNode(localnode, cookie));

		// Ping remote node to create the conection
		if (!node->ping(remotenode, 5000)) {
			PR_LOG(log, PR_LOG_ERROR, 
				("ErlXPCOMInit::launchErlang(): Node unracheable"));
			throw RemoteNodeUnreacheableException("Can't connect to remote node");
		}	
	} catch (EpiException &e) {
		throw ErlangException("Failed creating node", e);
	}
	
}
	
void ErlXPCOMInit::init(nsIComponentManager *componentManager, 
						nsIServiceManager *serviceManager) 
	throw (InternalErrorException) 
{
	PR_LOG(log, PR_LOG_DEBUG, 
		("ErlXPCOMInit::init(): Launching remote server (Thread 0x%08x)\n", 
		 PR_GetCurrentThread()));
		 
	PR_ASSERT(node.get());
	
	ErlPid* erlangServerPid;
	ErlTermPtr<> receivedTerm;
	MailBox *mailbox;
	
	try {
		// Get a mailbox. The node has the pointer owership!!!
		mailbox = node->createMailBox();

		// ... register the mailbox with erlxpcom name (for debug only)
		// node->registerMailBox("erlxpcom", mailbox);
		// Launch remote server :)
		// TODO: Add timeout here
		receivedTerm.reset(mailbox->RPC(getErlangNodeName(),
							"erlxpcom_orb", "start", 
							new ErlConsList(mailbox->self())));
		erlangServerPid = ErlPid::cast(receivedTerm.get());
		
	} catch (EpiException &e) {
		PR_LOG(log, PR_LOG_ERROR, ("ErlXPCOMInit::init(): Failed. Received term %s\n", 
									receivedTerm.get()? 
										receivedTerm->toString().c_str():
										""));

		throw ErlangException("Failed initializing remote server", e);
	}

	PR_LOG(log, PR_LOG_DEBUG, ("ErlXPCOMInit::init(): Initializing local ORB\n", 
								receivedTerm->toString().c_str()));
	
	// Create a new transport
	RequestTransport *transport = new RequestTransport(mailbox, erlangServerPid);

	// Create the ORB and register this object 
	orb.reset(new ErlangORB(transport));
	
	// Register the Component manager and the service manageer.
	// They will be the very first remote objects in erlang (0 and 1 OID)
	orb->registerStub(
		new ErlXPCOMStub(componentManager, 
			componentManager->GetIID(), NS_CURRENT_THREAD));

	orb->registerStub(
		new ErlXPCOMStub(serviceManager, 
			serviceManager->GetIID(), NS_CURRENT_THREAD));

	// Register the nsIModule of the erlang component loader. This module
	// will be the very first object registered in erlang side. (OID = 0)
	loaderModule = (nsIModule*) new ErlXPCOMProxy(0, nsIModule::GetIID());
	orb->registerProxy((ErlXPCOMProxy*) loaderModule);
	NS_ADDREF(loaderModule);

	// Start the transport
	transport->start();
	
}
		
nsIModule* ErlXPCOMInit::getLoaderModule() throw (InternalErrorException) {
	return loaderModule;
}

void ErlXPCOMInit::shutdown() throw (InternalErrorException) {
	// Unimplemented
}


#include <Config.hpp>

#include <iostream>
#include <memory>
#include <string>
#include <unistd.h>

#include <epi.hpp>

#include "RequestTransport.h"

// Use namespaces
using namespace epi::type;
using namespace epi::error;
using namespace epi::node;
using namespace erlxpcom;

int main (int argc, char **args) {
	
	if (argc < 4) {
		std::cout << "Use: " << args[0] << 
			" <local node name> <remote node name> <cookie>" <<
			std::endl;
		exit(0);
	}
    
    const std::string LOCALNODE(args[1]);
    const std::string REMOTENODE(args[2]);
    const std::string COOKIE(args[3]);

//	if (argc > 4 && args[4][0] == '1') {
//		Debug( dc::notice.on() );
//		Debug( dc::erlang.on() );
//		//Debug( dc::erlang_warning.on() );
//		Debug( dc::connect.on() );
//		Debug( libcw_do.on() );
//    }

    try {
        // Create the node
        AutoNode node(LOCALNODE, COOKIE);

        // Get a mailbox. The node has the pointer owership!!!
        MailBox *mailbox = node.createMailBox();
		
		// register the mailbox with erlxpcom name
		node.registerMailBox("erlxpcom", mailbox);
		// start the connection acceptor 
		// node.startAcceptor();
		
		// Ping remote node to create the conection
		if (!node.ping(REMOTENODE, 3000)) {
			std::cout << "Remote node is unreachable" << std::endl;
			return 1;
		}
				
		// Launch remote server :)
		ErlPid* erlangServerPid = ErlPid::cast(
			mailbox->RPC(REMOTENODE, "reply_server", "start", new ErlEmptyList()));
				
		// Create a new transport
		RequestTransport transport(mailbox, erlangServerPid);
		transport.start();
		
		sleep(100);

    } catch (EpiException &e) {
        std::cout << "Exception catched: " << e.getMessage() << std::endl;
        return 1;

    }
    return 0;
}

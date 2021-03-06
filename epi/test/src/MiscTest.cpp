

#include <iostream>
#include <memory>
#include <string>

#include "epi.hpp"


// Use namespaces
using namespace epi::type;
using namespace epi::error;
using namespace epi::node;

int main (int argc, char **args) {
	
	if (argc < 4) {
		std::cout << "Use: " << args[0] << 
			" <local node name> <remote node name> <cookie> [debug = 1|0]" <<
			std::endl;
		exit(0);
	}
    
    const std::string LOCALNODE(args[1]);
    const std::string REMOTENODE(args[2]);
    const std::string COOKIE(args[3]);

    if (argc > 4 && args[4][0] == '1') {
	    Debug( dc::notice.on() );
	    Debug( dc::erlang.on() );
	    //Debug( dc::erlang_warning.on() );
	    Debug( dc::connect.on() );
	    Debug( libcw_do.on() );
    }

    try {
        // Create the node
        AutoNode node(LOCALNODE, COOKIE);

        // Get a mailbox. The node has the pointer owership!!!
        MailBox *mailbox = node.createMailBox();

		if (node.ping(REMOTENODE, 30000)) {
			std::cout << "De pinga" << std::endl;
		} else {
			std::cout << "Non pinga" << std::endl;
		}
		mailbox->RPC(REMOTENODE, "reply_server", "proba_rpc", new ErlEmptyList());

		// Create the tuple {self(), hello}
/*
        ErlTermPtr<ErlTuple> tuple(new ErlTuple(2));
		tuple->initElement(
				new ErlAtom("hello"))->initElement(
					new ErlAtom("longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonghello")
					);
				
			
        ErlTermPtr<> tuple2(
			new ErlTuple(
				new ErlAtom("hello"), 
				new ErlAtom("longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonghello")
				)
			);
*/
        // Send the term to a server in the remote node
        // mailbox->send(REMOTENODE, "reply_server", tuple.get());

		

        // Receive the response
		while (true) {
			ErlTermPtr<> received(mailbox->receive(950));
		}

        // Print it
        //std::cout << "Received response: " <<
        //        received->toString() << std::endl;
    } catch (EpiException &e) {
        std::cout << "Exception catched: " << e.getMessage() << std::endl;
        return 1;

    }
    return 0;
}



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
			" <local node name> <remote node name> <cookie>" <<
			std::endl;
		exit(0);
	}
    
    const std::string LOCALNODE(args[1]);
    const std::string REMOTENODE(args[2]);
    const std::string COOKIE(args[3]);

    try {
        // Create the node
        AutoNode node(LOCALNODE, COOKIE);

        // Get a mailbox. The node has the pointer owership!!!
        MailBox *mailbox = node.createMailBox();

		// Create the tuple {self(), hello}
        ErlTermPtr<ErlTuple> tuple(new ErlTuple(2));
		tuple->initElement(mailbox->self())->initElement(new ErlAtom("hello"));

        // Send the term to a server in the remote node
        mailbox->send(REMOTENODE, "reply_server", tuple.get());

        // Receive the response
        ErlTermPtr<> received(mailbox->receive());

        // Print it
        std::cout << "Received response: " <<
                received->toString() << std::endl;
    } catch (EpiException &e) {
        std::cout << "Exception catched: " << e.getMessage() << std::endl;
        return 1;

    }
    return 0;
}

/*
***** BEGIN LICENSE BLOCK *****

This file is part of the EPI (Erlang Plus Interface) Library.

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


#include <iostream>
#include <sstream>
#include <ostream>
#include <memory>

#include "epi.hpp"

using namespace epi::error;
using namespace epi::type;
using namespace epi::node;

std::string LOCALNODE;
std::string REMOTENODE;
std::string COOKIE;

bool exit_flag = false;


// Create the set of term to test
ErlTermPtr<ErlTerm> * create_test_set()
        throw (EpiException)
{
    // Create an array of ErlTerm
    const int count = 30;
    ErlTermPtr<ErlTerm> *term_array = new ErlTermPtr<ErlTerm>[count];
    for (int i=0; i<count; i++) {
        term_array[i] = 0;
    }
    int index = 0;

    // An atom
    term_array[index++] = ErlTerm::format("an_atom");
    // A float
    term_array[index++] = ErlTerm::format("3.141617");
    // An integer
    term_array[index++] = ErlTerm::format("123456789");
    // A reference
    unsigned int ids[] = {1,2,3};
    term_array[index++] = new ErlRef(LOCALNODE, ids, 4);
    // A pid
    term_array[index++] = new ErlPid(LOCALNODE, 1, 2, 3);
    // A binary
    char *data="Mary has a little cat";
    term_array[index++] = new ErlBinary(data, strlen(data), true);
    // A port
    term_array[index++] = new ErlPort(LOCALNODE, 1, 2);
    // Some tuples
    term_array[index++] = new ErlTuple((unsigned int) 0);
    term_array[index++] = ErlTerm::format("{an_atom_in_a_tuple}");
    term_array[index++] = ErlTerm::format("{an_atom_in_a_tuple, 123}");
    term_array[index++] = ErlTerm::format("{{an_atom_in_a_tuple_in_a_tuple, 123}, 456}");
    // Lists
    term_array[index++] = new ErlEmptyList();
    term_array[index++] = ErlTerm::format("[an_atom_in_a_list, 123]");

    term_array[index++] = ErlTerm::format("[{an_atom_in_tuple_in_a_list, 123},"
            "[an_atom_in_a_list_in_a_list, 456]]");

    ErlConsList *list = new ErlConsList();
    list->addElement(new ErlAtom("an_atom_in_a_list"));
    list->addElement(new ErlLong(123));
    list->close(new ErlAtom("an_atom_closing_a_list"));
    term_array[index++] = list;
    index++;

    return term_array;
}

// Test send and receive with a reply server.
bool send_reply_server(ErlTerm *t, MailBox *mailbox)
        throw(EpiException)
{

    std::cout << "Sending term: " << t->toString() << "\n";

    OutputBuffer *buffer = mailbox->newOutputBuffer();

    // Create tuple with pid
    ErlPid *pid = mailbox->self();
    ErlTermPtr<ErlTuple> tuple(new ErlTuple(pid, t));

    buffer->writeTerm(tuple.get());

    mailbox->sendBuf(REMOTENODE, "reply_server", buffer);

    std::cout << "Sent. Receiving response:\n";

    std::auto_ptr<EpiMessage> msg(mailbox->receiveMsg());

    if (msg->messageType() != ERL_MSG_SEND) {
        std::cout << "Message is not SEND type\n";
        return false;
    }

    ErlTerm* t2 = ((ErlangMessage *) msg.get())->getMsg();

    std::cout << "Got term: " << t2->toString() << "\n";

    bool equals = t->equals(*t2);
    if (equals) {
        std::cout << "Equals, ok" << "\n";
    } else {
        std::cout << "Not equals, error!" << "\n";
    }
    return equals;
}


bool test_connect(AutoNode &node)
        throw (EpiException)
{

    std::cout << "Testing connect to a node with a reply server\n";

    MailBox* mailbox = node.createMailBox();

    bool ret=true;
    ErlTermPtr<ErlTerm> *test_set = create_test_set();
    ErlTermPtr<ErlTerm> *p = test_set;

    std::cout << "Testing send and receive data\n";
    while(p->get()) {
        if (!send_reply_server(p->get(), mailbox)) {
            ret = false;
            break;
        }
        p++;
    }


    delete [] test_set;

    return ret;
}

bool test_accept(AutoNode &node)
        throw (EpiException)
{

    MailBox* mailbox = node.createMailBox();
    node.registerMailBox("reply_server", mailbox);	
	node.startAcceptor();
	 
    std::auto_ptr<ErlangMessage> msg;
    ErlTerm* received_term;
    OutputBuffer *buffer = mailbox->newOutputBuffer();

    bool loop = true;
    std::cout << "Waiting incomming messages\n";
    while (loop) {
        msg.reset(mailbox->receiveMsg());

        if (!(msg->messageType() == ERL_MSG_SEND ||
              msg->messageType() == ERL_MSG_REG_SEND))
        {
            std::cout << "Message is not SEND type\n";
            return false;
        }

        received_term = msg->getMsg();

        if (msg->messageType() == ERL_MSG_REG_SEND) {
            std::cout << "Received to " << ((RegSendMessage *) msg.get())->getRecipientName() <<
                    ": " << received_term->toString() << "\n";
        } else {
            std::cout << "Received to " << ((SendMessage *) msg.get())->getRecipientPid()->toString() <<
                    ": " << received_term->toString() << "\n";
        }

        if (received_term->instanceOf(ERL_TUPLE)) {
            ErlTuple *tuple = (ErlTuple *) received_term;
            if (tuple->arity() == 2) {
                ErlTerm* term1 = tuple->elementAt(0);
                ErlTerm* term2 = tuple->elementAt(1);
                if (term1->instanceOf(ERL_PID)) {

                    std::cout << "Sending " << term2->toString() <<
                            " to " << term1->toString() << "\n";

                    buffer->reset();
                    buffer->writeTerm(term2);
                    mailbox->sendBuf((ErlPid *) term1, buffer);
                    std::cout << "Sent.\n";
                }
            }
        }

        if (received_term->instanceOf(ERL_ATOM)) {
            if (((ErlAtom *) received_term)->atomValue() == "exit") {
                loop = false;
            }
        }
    }
    return true;
}

bool test_local(AutoNode &node)
        throw (EpiException)
{

    std::cout << "Testing sending data localy\n";

    MailBox* mailbox1 = node.createMailBox();
    MailBox* mailbox2 = node.createMailBox();

    bool ret=true;
    ErlTermPtr<ErlTerm> *test_set = create_test_set();
    ErlTermPtr<ErlTerm> *p = test_set;

    std::cout << "Testing send and receive data local\n";
    ErlTermPtr<ErlTerm> term;
    while(p->get()) {
        mailbox1->send(mailbox2->self(), p->get());
        term.reset(mailbox2->receive());
        std::cout << "Local received " << term->toString() << "\n";
        if (*p == term) {
            std::cout << "Equals\n";

        }
        p++;
    }

    delete [] test_set;

    return ret;
}


// Test code
int main(int argc, char **argv) {


	if (argc < 4) {
		std::cout << "Use: " << argv[0] << 
			" <local node name> <remote node name> <cookie>" <<
			std::endl;
		exit(0);
	}
    
	LOCALNODE = argv[1];
	REMOTENODE = argv[2];
	COOKIE = argv[3];

    if (argc > 4 && argv[4][0] == '1') {
	    Debug( dc::notice.on() );
	    Debug( dc::erlang.on() );
	    //Debug( dc::erlang_warning.on() );
	    Debug( dc::connect.on() );
	    Debug( libcw_do.on() );
    }
    
    try {
        std::cout << "Starting node " << LOCALNODE << "\n";

        AutoNode node(LOCALNODE, COOKIE);

        std::cout << "Testing connect" << std::endl;
        if (!test_connect(node)) exit(1); 
        std::cout << "Testing accept" << std::endl;
        if (!test_accept(node)) exit(1);
        std::cout << "Testing local" << std::endl;
        if (!test_local(node)) exit(1);

    } catch (EpiException &e) {
        std::cout << "Catched exception: " << e.getMessage() << "\n";
        exit(1);
    }

    return 0;
}




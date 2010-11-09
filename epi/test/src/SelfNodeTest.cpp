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

// Main config file

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>

#include <iostream>

#include "epi.hpp"

using namespace epi::error;
using namespace epi::type;
using namespace epi::node;
using namespace epi::util;

std::string LOCALNODE;
std::string REMOTENODE;
std::string COOKIE;


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

    mailbox->sendBuf("reply_server", buffer);

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

bool send_rpc(ErlTerm *t, MailBox *mailbox)
    throw(EpiException)
{
    std::cout << "Sending RPC: reply_server:print(" <<
            t->toString() << ")\n";

    ErlTermPtr<ErlConsList> list = new ErlConsList(t);

    mailbox->sendRPC(REMOTENODE, "reply_server", "print", list.get());

    std::cout << "Sent. Receiving response:\n";

    ErlTermPtr<ErlTerm> t2(mailbox->receiveRPC());


    std::cout << "Got term: " << t2->toString() << "\n";

    bool equals = t->equals(*t2);
    if (equals) {
        std::cout << "Equals, ok" << "\n";
    } else {
        std::cout << "Not equals, error!" << "\n";
    }
    return equals;
}

class TestCommand1: public MatchingCommand {
public:

    virtual inline void execute(ErlTerm* term, VariableBinding *binding)
            throw (EpiException)
    {
        std::cout << "Got term by TestCommand1 "<< term->toString() <<
                " Variable X=" << binding->search("X")->toString() <<  std::endl;
    }
    virtual inline ~TestCommand1() {}

};

class TestCommand2: public MatchingCommand {
public:

    virtual inline void execute(ErlTerm* term, VariableBinding *binding)
            throw (EpiException)
    {
        std::cout << "Got term by TestCommand2 "<< term->toString() <<
                " Variable X=" << binding->search("X")->toString() <<  std::endl;
    }
    virtual inline ~TestCommand2() {}

};

bool test_connect(LocalNode &node)
        throw (EpiException)
{

    std::cout << "Testing connect to a node with a reply server\n";

    std::cout << "Connecting to " << REMOTENODE << "\n";

    std::auto_ptr<Connection> connection(node.connect(REMOTENODE));
    std::auto_ptr<MailBox> mailbox(node.createMailBox(connection.get()));
    connection->start();

    bool ret=true;
    ErlTermPtr<ErlTerm> *test_set = create_test_set();
    ErlTermPtr<ErlTerm> *p = test_set;
    /*

    std::cout << "Testing send and receive data\n";
    while(p->get()) {
        if (!send_reply_server(p->get(), mailbox.get())) {
            ret = false;
            break;
        }
        p++;
    }
    */
    std::cout << "Testing pattern Matching\n";
    ErlTermPtr<ErlTerm> hellomoon(new ErlTuple(new ErlAtom("hello"), new ErlAtom("moon")));
    ErlTermPtr<ErlTerm> helloworld(new ErlTuple(new ErlAtom("hello"), new ErlAtom("world")));
    ErlTermPtr<ErlTerm> hellocat(new ErlTuple(new ErlAtom("hello"), new ErlAtom("cat")));
    ErlTermPtr<ErlTerm> hellodog(new ErlTuple(new ErlAtom("hello"), new ErlAtom("dog")));
    ErlTermPtr<ErlTerm> varmoon(new ErlTuple(new ErlVariable(), new ErlAtom("moon")));
    ErlTermPtr<ErlTerm> varworld(new ErlTuple(new ErlVariable(), new ErlAtom("world")));
    ErlTermPtr<ErlTerm> varcat(new ErlTuple(new ErlVariable("X"), new ErlAtom("cat")));
    ErlTermPtr<ErlTerm> vardog(new ErlTuple(new ErlVariable(), new ErlAtom("dog")));
    mailbox->send("reply_server", new ErlTuple(mailbox->self(), hellomoon.get()));
    mailbox->send("reply_server", new ErlTuple(mailbox->self(), helloworld.get()));
    mailbox->send("reply_server", new ErlTuple(mailbox->self(), hellocat.get()));
    mailbox->send("reply_server", new ErlTuple(mailbox->self(), hellodog.get()));

    ErlTermPtr<ErlTerm> received_pattern;
    received_pattern.reset(mailbox->receive(varworld.get()));
    std::cout << "Pattern matched: " << received_pattern->toString() << "\n";
    received_pattern.reset(mailbox->receive(vardog.get()));
    std::cout << "Pattern matched: " << received_pattern->toString() << "\n";
    received_pattern.reset(mailbox->receive(varmoon.get()));
    std::cout << "Pattern matched: " << received_pattern->toString() << "\n";


    ComposedGuard *guard = new ComposedGuard();
    guard->addGuard(new MatchingCommandGuard(vardog.get(), new TestCommand1()));
    guard->addGuard(new MatchingCommandGuard(varcat.get(), new TestCommand2()));

    std::auto_ptr<ErlangMessage> a_msg(mailbox->receive(guard));
    std::cout << "Pattern matched: " << a_msg->getMsg()->toString() << "\n";

    std::cout << "Testing rpc calls\n";
    p = test_set;
    while(p->get()) {
        if (!send_rpc(p->get(), mailbox.get())) {
            ret = false;
            break;
        }
        p++;
    }

    delete [] test_set;

    return ret;
}

bool test_accept(LocalNode &node)
        throw (EpiException)
{

    node.publishPort();

    std::auto_ptr<Connection> connection(node.accept());
    std::auto_ptr<MailBox> mailbox(node.createMailBox(connection.get()));
    connection->start();

    std::auto_ptr<ErlangMessage> msg;
    ErlTerm* received_term;
    OutputBuffer *buffer = mailbox->newOutputBuffer();

    bool loop = true;
    while (loop) {
        msg.reset(mailbox->receiveMsg());

        if (!(msg->messageType() == ERL_MSG_SEND ||
              msg->messageType() == ERL_MSG_REG_SEND)) {
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
// Test code
int main(int argc, char **argv) {

	if (argc < 4) {
		std::cout << "Use: " << argv[0] << 
			" <local node name> <remote node name> <cookie> [debug=1|0]" <<
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


    std::cout << "Starting node " << LOCALNODE << "\n";
    LocalNode node(LOCALNODE, COOKIE);
    try {
        if (!test_connect(node)) exit(1);
        if (!test_accept(node)) exit(1);
    } catch (EpiException &e) {
    std::cout << "Catched exception: " << e.getMessage() << "\n";
        exit(1);
    }

    return 0;
}

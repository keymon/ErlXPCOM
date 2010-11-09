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



#include <unistd.h>

#include <iostream>

#include "EpiReceiver.hpp"
#include "EpiObserver.hpp"
#include "EpiSender.hpp"
#include "PlainBuffer.hpp"
#include "EpiMailBox.hpp"
#include "EpiMessage.hpp"
#include "ErlTypes.hpp"
#include <OpenThreads/Thread>

using namespace epi::node;
using namespace epi::type;
using namespace epi::error;

class SenderTest: public EpiSender {
public:

    OutputBuffer* newOutputBuffer() {
        std::cout << "Creating an outputbuffer \n";
        return new PlainBuffer();
    }


    void sendBuf( epi::type::ErlPid* from,
                  epi::type::ErlPid* to,
                  epi::node::OutputBuffer* buffer )
            throw (EpiConnectionException)
    {
        std::cout << "Sending a message from " << from->toString() << " to " << to->toString() << " buffer =" << buffer <<  "\n";
    }
    void sendBuf( epi::type::ErlPid* from,
                  const std::string &to,
                  epi::node::OutputBuffer* buffer )
            throw (EpiConnectionException)
    {
        std::cout << "Sending a message from " << from->toString() << " to " << to << "\n";
    }
    void sendBuf( epi::type::ErlPid* from,
                  const std::string &nodename,
                  const std::string &to,
                  epi::node::OutputBuffer* buffer )
            throw (EpiConnectionException)
    {
        std::cout << "Sending a message from " << from->toString() << " to " << to  << " at" <<  nodename<< "\n";
    }

};

class MyConnection: public OpenThreads::Thread, public EpiObserver {
public:
    MyConnection(EpiReceiver *receiver): mReceiver(receiver), mExit(false) {
    }

    void event(EpiObservable* observed, EpiEventTag event)  {
        if (event == EVENT_DESTROY) {
            this->close();
        }
    }

    void close () {
        if (!mExit)
            mExit = true;
    }

    void run() {
        std::cout << "MyConnection start\n";
        ErlTermPtr<ErlAtom> atom(new ErlAtom("hola"));
        int counter = 0;
        while (!mExit) {
            sleep(2);
            std::cout << "MyConnection: delivering...\n";

            PlainBuffer *buffer = new PlainBuffer();
            buffer->writeTerm(atom.get());

            counter = (counter + 1) % 5;

            if (counter != 0) {
                mReceiver->deliver(this, new SendMessage(new ErlPid("ahost", 1, 2, 3), buffer));
            } else {
                mReceiver->deliver(this, new ErrorMessage(new EpiConnectionException("An Connection exception")));
            }
        }

    }

    ~MyConnection() {
        std::cout << "MyConnection destroying...\n";
        this->close();
        join();
        std::cout << "connection destroyed\n";
    }
private:
    EpiReceiver *mReceiver;
    bool mExit;

};


int main() {

    Debug( dc::notice.on() );
    Debug( dc::erlang.on() );
    //Debug( dc::erlang_warning.on() );
    Debug( dc::connect.on() );
    Debug( libcw_do.on() );

    SenderTest sender;
    MailBox mailbox(new ErlPid("here@host", 1, 2, 3));
    mailbox.setSender(&sender);
    MyConnection connection(&mailbox);
    mailbox.addObserver(&connection);
    connection.start();

    try {
        mailbox.send(new ErlPid("toahost", 1, 2, 3), new ErlAtom("hola"));
        ErlTermPtr<ErlTerm> t;
        t.reset(mailbox.receive());
        std::cout << "received: " << t->toString() << "\n";
        ErlTerm *p = mailbox.receive(100);
        //t.reset(p);
        if (p != 0) {
            std::cout << "received: " << t->toString() << "\n";
        } else {
            std::cout << "timeout\n";

        }
        t.reset(mailbox.receive());
        std::cout << "received: " << t->toString() << "\n";
        t.reset(mailbox.receive());
        std::cout << "received: " << t->toString() << "\n";
        t.reset(mailbox.receive());
        std::cout << "received: " << t->toString() << "\n";
        t.reset(mailbox.receive());
        std::cout << "received: " << t->toString() << "\n";

    } catch (EpiException &e) {
        std::cout << "Excepcion: " << e.getMessage() << "\n";
    }

}

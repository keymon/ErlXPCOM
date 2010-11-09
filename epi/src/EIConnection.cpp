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



#include "EIConnection.hpp"
#include "EIOutputBuffer.hpp"
#include "EIInputBuffer.hpp"
#include "EpiUtil.hpp"

using namespace epi::type;
using namespace epi::error;
using namespace epi::node;
using namespace epi::util;
using namespace epi::ei;

///////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////

namespace epi {
namespace ei {
/**
 * This class will accept all incoming messages from a socket,
 * delivering them to the receiver of the connection
 */
class EIMessageAcceptor: public OpenThreads::Thread  {
public:
    /**
     * The acceptor thread will start with creation of
     * the object.
     */
    EIMessageAcceptor(EIConnection *connection);

    ~EIMessageAcceptor();

    /**
     * This method will stop and destroy the acceptor
     */
    void stop();

    virtual void run();

private:
    EIConnection *mConnection;
    bool mThreadExit;

};
}// ei
}// epi

EIMessageAcceptor::EIMessageAcceptor(EIConnection *connection):
        mConnection(connection), mThreadExit(false)
{
    start();
}

EIMessageAcceptor::~EIMessageAcceptor()
{
    this->stop();
    if (this->isRunning()) {
        Dout(dc::connect, "["<<this<<"]"<< "EIMessageAcceptor::~EIMessageAcceptor(): joining thread");
        this->join();
    }
}

void EIMessageAcceptor::stop() {
    mThreadExit = true;
}

void EIMessageAcceptor::run() {

    //Dout(dc::connect, "["<<this<<"]"<< "EIMessageAcceptor::run(): Thread started");

    erlang_msg msg;
    int receive_res = 0;
    std::auto_ptr<EIInputBuffer> buffer;
    EpiMessage *msgResult;
    OpenThreads::Mutex &mutex = mConnection->_socketMutex;
    Socket *socket = mConnection->mSocket;

    while(!mThreadExit) {

        // Use a buffer with magic version... ¬_¬
        buffer.reset(new EIInputBuffer());

        do {
            if (mThreadExit) {
                break;
            }
            mutex.lock();
            // Check for incoming messages. Each 100 ms, check if thread must exit.
            receive_res = ei_xreceive_msg_tmo(socket->getSystemSocket(),
                                              &msg, buffer->getBuffer(), 100);
            mutex.unlock();
        } while((receive_res == ERL_TICK ||
                 (receive_res == ERL_ERROR &&
                         (erl_errno == ETIMEDOUT ||
                         erl_errno == EAGAIN ||
                         erl_errno == 0 // ... pq pasa isto? non sei. librería EI APESTA! :-/
                         ))) &&
                 !mThreadExit); // ignore ticks and timeouts

        // Exit if necesary
        if (mThreadExit) {
            break;
        }

        if (receive_res == ERL_ERROR) {
            //Dout(dc::connect, "["<<this<<"]"<<
            //        "EIMessageAcceptor: sending connection error");
            // FIXME, give more information
            msgResult = new ErrorMessage(
                    new EpiEIException("Error in receive", erl_errno));
            // Check if is necesary exit.
            if (erl_errno == EIO) {
                stop();
            }
        } else {

            // FIXME: Check the cookie
            if (! (mConnection->getCookie() == msg.cookie)) {
                std::ostringstream oss;
                oss << "Cookies differ " << mConnection->getCookie() << "!=" << msg.cookie;
                msgResult = new ErrorMessage(new EpiAuthException(oss.str()));
            }

            try {
            //    Dout(dc::connect, "["<<this<<"]"<<
            //            "EIMessageAcceptor: sending connection message");
                msgResult = epi::util::ToMessage(&msg, buffer.get());
            } catch (EpiConnectionException &e) {
            //    Dout(dc::connect, "["<<this<<"]"<<
            //            "EIMessageAcceptor: sending connection error (unknown message)");
                msgResult = new ErrorMessage(new EpiConnectionException(e));
            }
        }

        // All ok, the buffer is referenced by the message and we can (and have to)
        // release the auto_ptr to it.
        buffer.release();

        mConnection->deliver(this, msgResult);
    }
    //Dout(dc::connect, "["<<this<<"]"<< "EIMessageAcceptor:: Thread exit");
}

///////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////
EIConnection::EIConnection(PeerNode *peer,
                           std::string cookie,
                           Socket *aSocket):
        Connection(peer, cookie),
        mSocket(aSocket),
        mAcceptor(0)
{
}

EIConnection::~EIConnection() {
    Dout(dc::connect, "["<<this<<"]"<< "EIConnection::~EIConnection()");
    this->close();
}

OutputBuffer* EIConnection::newOutputBuffer() {
    return new EIOutputBuffer();
}


void EIConnection::sendBuf( ErlPid * from, ErlPid * to, OutputBuffer * _buffer )
        throw( EpiConnectionException)
{
    Dout_continue(dc::connect, _continue, " failed.",
            "["<<this<<"]"<< "EIConnection::sendBuf(from=" <<
                    from->toString() << ", to=" << to->toString() << ", buffer):");


    EIOutputBuffer *buffer = (EIOutputBuffer *) _buffer;

    // std::auto_ptr<erlang_pid> _from = ErlPid2EI(from);
    std::auto_ptr<erlang_pid> _to(ErlPid2EI(to));
    int ei_res = ei_send_encoded(mSocket->getSystemSocket(), _to.get(),
                                 (char *) buffer->getInternalBuffer(),
                                 *(buffer->getInternalIndex()));

    // FIXME: throw more expecific exceptions
    if (ei_res < 0) {
        throw EpiEIException("Error sending data", erl_errno);
    }

    Dout_finish(_continue, " sent.");

}

void EIConnection::sendBuf( ErlPid * from, const std::string &to,
                          OutputBuffer * _buffer )
        throw( EpiConnectionException)
{
    Dout_continue(dc::connect, _continue, " failed.",
                  "["<<this<<"]"<< "EIConnection::sendBuf(from=" <<
                          from->toString() << ", to=" << to << ", buffer): ");

    EIOutputBuffer *buffer = (EIOutputBuffer *) _buffer;

    std::auto_ptr<erlang_pid> _from(ErlPid2EI(from));

    int ei_res = ei_send_reg_encoded(mSocket->getSystemSocket(), _from.get(),
                                     (char *) to.c_str(),
                                     (char *) buffer->getInternalBuffer(),
                                     *(buffer->getInternalIndex()));

    // FIXME: throw more expecific exceptions
    if (ei_res < 0) {
        throw EpiEIException("Error sending data", erl_errno);
    }

    Dout_finish(_continue, " sent.");


}

void EIConnection::sendBuf( ErlPid* from,
                            const std::string &node,
                            const std::string &to,
                            OutputBuffer* buffer )
        throw (EpiConnectionException)
{
    // the connection is connected to one peer, just send all to it
    sendBuf(from, to, buffer);
}


void EIConnection::start() {
    if (mAcceptor == 0) {
        mAcceptor = new EIMessageAcceptor(this);
    }
}

void EIConnection::stop() {
    if (mAcceptor) {
        mAcceptor->stop();
        delete mAcceptor;
        mAcceptor = 0;
    }
}


void EIConnection::close()
{
    this->stop();
    _socketMutex.lock();
    delete mSocket;
    mSocket = 0;
    _socketMutex.unlock();
}


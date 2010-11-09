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


#include "Socket.hpp"
#include "EpiLocalNode.hpp"
#include "EpiMailBox.hpp"
#include "EpiConnection.hpp"
#include "EpiUtil.hpp"

#include "ErlangTransportManager.hpp"

using namespace epi::node;
using namespace epi::error;
using namespace epi::type;
using namespace epi::util;


//////////////////////////////////////////////////////////////////////////
// LocalNode
LocalNode::LocalNode(const std::string aNodeName)
        throw (EpiBadArgument, EpiConnectionException, EpiException):
        mCreation(smCreationCounter++)
{
    Dout_continue(dc::connect, _continue, " failed.",
                  "new LocalNode(name="<< aNodeName <<
                          ", cookie="<< smDefaultCookie <<
                          ", creation=" << mCreation << ")");
    init(aNodeName, smDefaultCookie, 0);
    Dout_finish(_continue, ".");
}

LocalNode::LocalNode(const std::string aNodeName, const std::string aCookie)
        throw (EpiBadArgument, EpiConnectionException, EpiException):
        mCreation(smCreationCounter++)
{
    Dout_continue(dc::connect, _continue, " failed.",
            "new LocalNode(name="<< aNodeName <<
                    ", cookie="<< aCookie <<
                    ", creation=" << mCreation << ")");
    init(aNodeName, aCookie, 0);
    Dout_finish(_continue, ".");
}

LocalNode::LocalNode(const std::string aNodeName,
                     const std::string aCookie,
                     ErlangTransport *transport)
        throw (EpiBadArgument, EpiConnectionException):
        mCreation(smCreationCounter++)
{
    Dout_continue(dc::connect, _continue, " failed.",
                  "new LocalNode(name="<< aNodeName <<
                          ", cookie="<< aCookie <<
                          ", creation=" << mCreation <<
                          ", transport="<< transport << ")");
    init(aNodeName, aCookie, transport);
    Dout_finish(_continue, ".");
}

LocalNode::~LocalNode() {
    Dout(dc::connect, "Destroing node "<< mNodeName << "#" <<mCreation);
    delete mTransport;
}

void LocalNode::init(const std::string aNodeId,
                     const std::string aCookie,
                     ErlangTransport *transport)
        throw (EpiException)
{
    // Init the counters
    mPidCount = 1;
    mPortCount = 1;
    mSerial = 0;
    mRefId[0] = 1;
    mRefId[1] = 0;
    mRefId[2] = 0;

    mCookie = aCookie;

    if (transport) {
        mTransport = transport;
    } else {
        mTransport = ErlangTransportManager::createErlangTransport(aNodeId, aCookie);
    }

    initNodeName(mTransport->getNodeName());

}


ErlPid* LocalNode::createPid() {
    ErlPid *p = new ErlPid(mNodeName, mPidCount, mSerial, mCreation);

    mPidCount++;
    if (mPidCount > 0x7fff) {
        mPidCount = 0;

        mSerial++;
        if (mSerial > 0x1fff) { /* 13 bits */
            mSerial = 0;
        }
    }

    return p;
}

ErlPort* LocalNode::createPort() {
    ErlPort *newPort =
            new ErlPort(getNodeName(),mPortCount,getCreation());

    mPortCount++;

    if (mPortCount > 0xfffffff) { /* 28 bits */
        mPortCount = 0;
    }

    return newPort;

}

ErlRef* LocalNode::createRef() {
    ErlRef *newRef =
            new ErlRef(getNodeName(), mRefId, getCreation());

    // increment ref ids (3 ints: 18 + 32 + 32 bits)
    mRefId[0]++;
    if (mRefId[0] > 0x3ffff) {
        mRefId[0] = 0;

        mRefId[1]++;
        if (mRefId[1] == 0) {
            mRefId[2]++;
        }
    }

    return newRef;
}

Connection* LocalNode::connect(const std::string node)
        throw(EpiConnectionException)
{
    return mTransport->connect(node);
}

Connection* LocalNode::connect(const std::string node, const std::string cookie)
        throw(EpiConnectionException)
{
    return mTransport->connect(node, cookie);
}

Connection* LocalNode::accept(long timeout)
        throw(EpiConnectionException)
{
    return mTransport->accept(timeout);
}

Connection* LocalNode::accept(const std::string cookie, long timeout)
        throw(EpiConnectionException)
{
    return mTransport->accept(cookie, timeout);
}

void LocalNode::publishPort()
        throw (EpiConnectionException)
{
    mTransport->publishPort();
}

void LocalNode::unPublishPort() throw (EpiConnectionException)
{
    mTransport->unPublishPort();
}

MailBox* LocalNode::newMailBox() {
    return new MailBox(createPid());
}

MailBox *LocalNode::createMailBox(Connection *connection) {
    MailBox* mailbox = newMailBox();
    mailbox->setSender(connection);
    connection->setReceiver(mailbox);
    return mailbox;
}



// node creation counter
short LocalNode::smCreationCounter = 0;

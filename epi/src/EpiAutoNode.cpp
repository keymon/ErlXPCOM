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



#include <set>

#include <OpenThreads/ScopedLock>

#include "EpiAutoNode.hpp"
#include "PlainBuffer.hpp"

#include <unistd.h>

using namespace epi::node;
using namespace epi::type;
using namespace epi::error;

/**
 * erase elements in a map by value
 * @return number of elements erased
 */
template<class M, class K, class T, class Compare>
int eraseByValue(M &aMap, T &value) {

    std::set<K, Compare> keys;

    for (typename M::const_iterator p = aMap.begin(); p!=aMap.end(); p++) {
        if ((*p).second == value) {
            keys.insert(K((*p).first));
        }
    }
    for (typename std::set<K, Compare>::const_iterator p = keys.begin();
         p!=keys.end(); p++)
    {
        aMap.erase(*p);
    }
    return keys.size();
}

AutoNode::AutoNode( const std::string aNodeName )
    throw( EpiBadArgument, EpiConnectionException):
        LocalNode(aNodeName), mThreadExit(false),
        mMailBoxes(), mConnections(), mRegMailBoxes(),
        mFlushConnections(),
        _connectionsMutex(), _mailboxesMutex(),
        _regmailboxesMutex(), _socketMutex()
{
}

AutoNode::AutoNode( const std::string aNodeName,
                    const std::string aCookie)
        throw( EpiBadArgument, EpiConnectionException):
        LocalNode(aNodeName, aCookie), mThreadExit(false),
        mMailBoxes(), mConnections(), mRegMailBoxes(),
        mFlushConnections(),
        _connectionsMutex(), _mailboxesMutex(),
        _regmailboxesMutex(), _socketMutex()
{
}

AutoNode::AutoNode( const std::string aNodeName,
                    const std::string aCookie,
                    ErlangTransport *transport)
        throw( EpiBadArgument, EpiConnectionException):
        LocalNode(aNodeName, aCookie, transport), mThreadExit(false),
        mMailBoxes(), mConnections(), mRegMailBoxes(),
        mFlushConnections(),
        _connectionsMutex(), _mailboxesMutex(),
        _regmailboxesMutex(), _socketMutex()
{
}

AutoNode::~AutoNode() {
    OpenThreads::ScopedLock<OpenThreads::Mutex> lock1(_regmailboxesMutex);
    OpenThreads::ScopedLock<OpenThreads::Mutex> lock2(_connectionsMutex);
    OpenThreads::ScopedLock<OpenThreads::Mutex> lock3(_mailboxesMutex);

    Dout(dc::connect, "["<<this<<"]"<< "AutoNode::~AutoNode()");

    flushConnections();

    destroyConnections();
    destroyMailBoxes();

    this->close();

    if (this->isRunning()) {
        Dout(dc::connect, "["<<this<<"]"<< "AutoNode::~AutoNode(): joining thread");
        this->join();
        Dout(dc::connect, "["<<this<<"]"<< "joined");
    }
}

MailBox *AutoNode::createMailBox() {
    MailBox* mailbox = newMailBox();
    mailbox->setSender(this);
    addMailBox(mailbox);
    return mailbox;
}

void AutoNode::deattachMailBox(MailBox *mailbox) {
    mailbox->setSender(0);
    removeMailBox(mailbox);
}

void AutoNode::registerMailBox(const std::string name, MailBox *mailbox) {
    OpenThreads::ScopedLock<OpenThreads::Mutex> lock(_regmailboxesMutex);
    mRegMailBoxes[name] = mailbox;
}

void AutoNode::unRegisterMailBox(const std::string name) {
    OpenThreads::ScopedLock<OpenThreads::Mutex> lock(_regmailboxesMutex);
    mRegMailBoxes.erase(name);
}

void AutoNode::unRegisterMailBox(MailBox* mailbox) {
    OpenThreads::ScopedLock<OpenThreads::Mutex> lock(_regmailboxesMutex);
    eraseByValue<registered_mailbox_map, std::string,
    MailBox*, std::less<std::string> >(mRegMailBoxes, mailbox);
}


void AutoNode::close() {
    mThreadExit = true;
}

void AutoNode::startAcceptor()
        throw (EpiConnectionException)
{
	// start the acceptor thread
    start();
	
	// Publish the port. Try to publish it, and if it fails, tray
	// to unpublish an publish it.
	try {
		this->publishPort();
	} catch (EpiConnectionException &e) {
		this->unPublishPort();
		this->publishPort();
	}
		
}

void AutoNode::run() {
    //Dout(dc::connect, "["<<this<<"]"<< "AutoNode::run(): Thread started");
    Connection* connection;
    do {
        //Dout(dc::connect, "["<<this<<"]"<< "AutoNode::run(): Waiting for connections");
        if (mThreadExit) {
            break;
        }
        try {
            // accept a connection, add and start it
            connection = this->accept(100);
		    if(connection) {             
            		//Dout(dc::connect, "["<<this<<"]"<< "AutoNode::run(): New connection from "<<
                 //   	connection->getPeer()->getNodeName());
            		addConnection(connection);
            		connection->start();
		    }
        } catch (EpiConnectionException &e) {
            // FIXME: Inform about the error
            //Dout(dc::connect, "["<<this<<"]"<< "AutoNode::run(): Exception: \"" << e.getMessage() <<"\"");
            // FIXME: Check when is necesary stop the thread
            // mThreadExit = true;
        } catch (EpiTimeout &e) {
        }
    } while(!mThreadExit);

//    Dout(dc::connect, "["<<this<<"]"<< "AutoNode::run(): Thread exit");
}

/* internal info about the message formats...
 *
 * the request:
 *  -> REG_SEND {6,#Pid<bingo@aule.1.0>,'',net_kernel}
 *  {'$gen_call',{#Pid<bingo@aule.1.0>,#Ref<bingo@aule.2>},{is_auth,bingo@aule}}
 *
 * the reply:
 *  <- SEND {2,'',#Pid<bingo@aule.1.0>}
 *  {#Ref<bingo@aule.2>,yes}
 */
bool AutoNode::ping(const std::string remoteNode, long timeout) {
	if (remoteNode == this->getAliveName() ||	
		remoteNode == this->getNodeName()) {
		return true;
	}
	try {
		// Create the mailbox 
		MailBox* mailbox (this->createMailBox());
		VariableBinding binding;
		// Create the ping tuple, and the reply pattern
		ErlTerm* pingRef = createRef();
		
		ErlTermPtr<ErlTuple> pingTuple(new ErlTuple(3));
		pingTuple->initElement(new ErlAtom("$gen_call"));
		pingTuple->initElement(new ErlTuple(mailbox->self(), pingRef));
		pingTuple->initElement(new ErlTuple(new ErlAtom("is_auth"), 
					 					    new ErlAtom(getNodeName().c_str())));
		
		ErlTermPtr<ErlTuple> replyPattern(new ErlTuple(pingRef, 
										  new ErlAtom("yes")));
		
		// send it. It will try to connect each 500ms until timeout
		while (timeout >= 0) {
			try {
				// try to connect 
				mailbox->send(remoteNode, "net_kernel", pingTuple.get());
				break;
			} catch (EpiConnectionException &e) {
				usleep(500000);
				timeout -= 500;
				if (timeout < 0) {
					return false;
				}
			}
		}

		// Get the reply
		ErlTermPtr<> reply(mailbox->receive(replyPattern.get(), timeout));

		// If we get a reply, return it
		if (reply.get() != 0) {
			return true;
		}
	} catch (EpiException &e) {
		
	}
	return false;
		
}

void AutoNode::deliver( void *origin, EpiMessage* msg ) {
    Dout(dc::connect, "AutoNode::deliver(msg)");
    switch(msg->messageType()) {
        case ERL_MSG_ERROR:
            // FIXME: Implement a system to notify connection failure
            removeConnection((Connection *) origin);
            break;
        case ERL_MSG_SEND:
            if (1==1) { // hardcoded scope :)
                SendMessage *smsg = (SendMessage*) msg;

                Dout_continue(dc::connect, _continue, " failed.", 
                		"AutoNode::deliver(msg) -> Send type to "<<
                    	smsg->getRecipientPid()->toString());

                MailBox *recipientMailBox = getMailBox(smsg->getRecipientPid());
                if (recipientMailBox) {
                    recipientMailBox->deliver(origin, msg);
                    Dout_finish(_continue, " Sent to recipient");
                } else {
                    Dout_finish(_continue, " no recipient found, ignored");
                    // Ignore message
                    delete msg;
                }
            }
            break;
        case ERL_MSG_REG_SEND:
            if (1==1) { // hardcoded scope :)
                RegSendMessage *rsmsg = (RegSendMessage*) msg;

                Dout_continue(dc::connect, _continue, " failed.", 
                		"AutoNode::deliver(msg) -> RegSend type to "<<
                     rsmsg->getRecipientName());

                MailBox *recipientMailBox = getMailBox(rsmsg->getRecipientName());
                if (recipientMailBox) {
                    recipientMailBox->deliver(origin, msg);
                    Dout_finish(_continue, " Sent to recipient");
                } else {
                    Dout_finish(_continue, " no recipient found, ignored");
                    // Ignore message
                    delete msg;
                }
            }
            break;
        case ERL_MSG_UNLINK:
        case ERL_MSG_LINK:
        case ERL_MSG_EXIT:
        // TODO: Add code for control messages
            break;
        default:
            break;
    }

}

void AutoNode::sendBuf( epi::type::ErlPid* from,
                        epi::type::ErlPid* to,
                        epi::node::OutputBuffer* buffer )
        throw (epi::error::EpiConnectionException)
{

    if (isSameHost(to->node(), this->getNodeName(), this->getHostName())) {
        SendMessage *message = new SendMessage(to, buffer->getInputBuffer());
        deliver(this, message);
    } else {
        Connection *connection = attempConnection(to->node());

        // Encode data to output buffer for this connection
        PlainBuffer *plainbuffer = (PlainBuffer *) buffer;
        OutputBuffer *outbuffer = connection->newOutputBuffer();
        ErlTerm *t;
        do {
            t = plainbuffer->readTerm();
            if (t) {
                outbuffer->writeTerm(t);
            }
        } while (t != 0);
        connection->sendBuf(from, to, outbuffer);
    }
}

void AutoNode::sendBuf( epi::type::ErlPid* from,
                        const std::string &to,
                        epi::node::OutputBuffer* buffer )
        throw (epi::error::EpiConnectionException)
{
    RegSendMessage *message = new RegSendMessage(from, to, buffer->getInputBuffer());
    deliver(this, message);
}

void AutoNode::sendBuf( epi::type::ErlPid* from,
                        const std::string &node,
                        const std::string &to,
                        epi::node::OutputBuffer* buffer )
        throw (epi::error::EpiConnectionException)
{
    if (isSameHost(node, this->getNodeName(), this->getHostName())) {
        RegSendMessage *message = new RegSendMessage(from, to, buffer->getInputBuffer());
        deliver(this, message);
    } else {
        Connection *connection = attempConnection(node);

        // Encode data to output buffer for this connection
        PlainBuffer *plainbuffer = (PlainBuffer *) buffer;
        OutputBuffer *outbuffer = connection->newOutputBuffer();
        ErlTerm *t;
        do {
            t = plainbuffer->readTerm();
            if (t) {
                outbuffer->writeTerm(t);
            }
        } while (t != 0);

        connection->sendBuf(from, to, outbuffer);
    }
}

OutputBuffer* AutoNode::newOutputBuffer() {
    return new PlainBuffer();
}


void AutoNode::event(EpiObservable* observed, EpiEventTag event) {
}

void AutoNode::addMailBox(MailBox *mailbox) {
    OpenThreads::ScopedLock<OpenThreads::Mutex> lock(_mailboxesMutex);
    mMailBoxes[mailbox->self()] = mailbox;
}

MailBox *AutoNode::getMailBox(ErlPid *pid) {
    OpenThreads::ScopedLock<OpenThreads::Mutex> lock(_mailboxesMutex);
    if (mMailBoxes.count(pid)) {
        return mMailBoxes[pid];
    } else {
        return 0;
    }
}

MailBox *AutoNode::getMailBox(std::string name) {
    OpenThreads::ScopedLock<OpenThreads::Mutex> lock(_regmailboxesMutex);
    if (mRegMailBoxes.count(name)) {
        return mRegMailBoxes[name];
    } else {
        return 0;
    }
}

void AutoNode::removeMailBox(MailBox *mailbox) {
    // I don't use ScopedLock to prevent interlock
    _mailboxesMutex.lock();
    eraseByValue<mailbox_map, ErlTermPtr<ErlPid>, MailBox*, ErlPidPtrCompare>(mMailBoxes, mailbox);
    _mailboxesMutex.unlock();
    _regmailboxesMutex.lock();
    eraseByValue<registered_mailbox_map, std::string,
    MailBox*, std::less<std::string> >(mRegMailBoxes, mailbox);
    _regmailboxesMutex.unlock();
}

Connection* AutoNode::getConnection(std::string name) {
    OpenThreads::ScopedLock<OpenThreads::Mutex> lock(_connectionsMutex);
    if (mConnections.count(name)) {
        return mConnections[name];
    } else {
        return 0;
    }
}

void AutoNode::addConnection(Connection* connection) {
    OpenThreads::ScopedLock<OpenThreads::Mutex> lock(_connectionsMutex);
    mConnections[connection->getPeer()->getNodeName()] = connection;
    connection->setReceiver(this);
    // Flush the connections that must be deleted
    flushConnections();
}

void AutoNode::removeConnection(Connection* connection) {
    OpenThreads::ScopedLock<OpenThreads::Mutex> lock(_connectionsMutex);
    int count =
            eraseByValue<connection_map, std::string, Connection*, std::less<std::string> >(mConnections, connection);
    // If count>0 => the connection belongs to this node and will be deleted
    if (count > 0) {
        mFlushConnections.push_back(connection);
    }
}

void AutoNode::flushConnections() {
    for (connection_list::const_iterator p = mFlushConnections.begin();
         p!=mFlushConnections.end(); p++)
    {
        delete *p;
    }
    mFlushConnections.clear();
}

Connection *AutoNode::attempConnection(std::string name)
        throw (EpiConnectionException)
{
    Connection *connection = getConnection(name);
    if (connection == 0) {
        connection = this->connect(name);
        addConnection(connection);
        connection->start();
    }
    return connection;
}

void AutoNode::destroyConnections() {
    for (connection_map::const_iterator p =
         mConnections.begin(); p!=mConnections.end(); p++)
    {
        delete (*p).second;
    }
}

void AutoNode::destroyMailBoxes() {
    for (mailbox_map::const_iterator p =
         mMailBoxes.begin(); p!=mMailBoxes.end(); p++)
    {
        delete (*p).second;
    }
}

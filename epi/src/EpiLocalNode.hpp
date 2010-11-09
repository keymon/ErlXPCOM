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

#ifndef _EPILOCALNODE_H
#define _EPILOCALNODE_H

#include "EpiNode.hpp"
#include "EpiMailBox.hpp"
#include "ErlangTransport.hpp"

namespace epi {
namespace node {

/**
 * Self managed node
 */
class LocalNode: public AbstractNode {
public:
    /**
     * Create a new node, using default cookie an any port
     * @param aNodeId node identifier with the protocol, alivename,
     *  hostname and port. ex: "ei:mynode@host.somewhere.com:3128"
     * @throws EpiBadArgument if node name is too long
     * @throws EpiConnectionException if there is a network problem
     * @throws EpiException if there is an error in transport creation
     */
    LocalNode(const std::string aNodeId)
            throw (EpiBadArgument, EpiConnectionException, EpiException);

    /**
     * Create a new node, using given cookie
     * @param aNodeId node identifier with the protocol, alivename,
     *  hostname and port. ex: "ei:mynode@host.somewhere.com:3128"
     * @param cookie cookie to use
     * @throws EpiBadArgument if node name is too long
     * @throws EpiConnectionException if there is a network problem
     * @throws EpiException if there is an error in transport creation
     */
    LocalNode(const std::string aNodeId, const std::string aCookie)
            throw (EpiBadArgument, EpiConnectionException, EpiException);

    /**
     * Create a new node, using given cookie
     * @param aNodeName node name
     * @param aCookie cookie to use
     * @param transport to use
     * @throws EpiBadArgument if node name is too long
     * @throws EpiConnectionException if there is a network problem
     */
    LocalNode(const std::string aNodeName,
              const std::string aCookie,
              ErlangTransport *transport)
            throw (EpiBadArgument, EpiConnectionException);


    ~LocalNode();

    /**
     * Create  a new unique pid
     */
    ErlPid* createPid();

    /**
     * Create  a new unique port
     */
    ErlPort* createPort();

    /**
     * Create  a new unique ref
     */
    ErlRef* createRef();


    /**
     * Get creation number
     * FIXME: Where must be the creation number??? How does creation number work?
     * EI documentation and code does not explain this :(.
     */
    inline short getCreation() const {
        return mCreation;
    }

    /**
     * Set up a connection to an Erlang node, using the default cookie
     * @param node node name to connect
     * @returns A new connection. The connection has no receiver defined
     * and is not started.
     */
    Connection* connect(const std::string node)
            throw(EpiConnectionException);

    /**
     * Set up a connection to an Erlang node, using other cookie
     * @param node node name to connect
     * @param cookie cookie to use
     * @returns A new connection. The connection has no receiver defined
     * and is not started.
     */
    Connection* connect(const std::string node, const std::string cookie)
            throw(EpiConnectionException);

    /**
     * Accept a connection from a client process.
     * This method sets the socket to listen incoming connections.
     * @param timeout timeout in ms
     * @returns A new connection. The connection has no receiver defined
     * and is not started. returns 0 if timeout.
     */
    Connection* accept(long timeout = 0)
            throw(EpiConnectionException);

    /**
     * Accept a connection from a client process using other cookie
     * This method sets the socket to listen incoming connections.
     * @param timeout timeout in ms
     * @returns A new connection. The connection has no receiver defined
     * and is not started. returns 0 if timeout.
     */
    Connection* accept(const std::string cookie, long timeout = 0)
            throw(EpiConnectionException);

    /**
     * Publish the node port
     */
    void publishPort() throw (EpiConnectionException);

    /**
     * Unpublish the node port
     */
    void unPublishPort() throw (EpiConnectionException);

    /**
     * Return new MailBox with a new pid.
     * This mailbox has not a sender defined and must be set using setSender()
     */
    MailBox *newMailBox();

    /**
     * Create a new MailBox with a new pid associated to the given connection:
     * the sender of mailbox is connection and the receiver of connection is
     * the mailbox
     */
    MailBox *createMailBox(Connection *connection);

protected:

    /**
     * Init counters for pids, ports and refs and the ErlangTransport
     * to use
     */
    void init(const std::string aNodeId,
              const std::string aCookie,
              ErlangTransport *transport)
            throw (EpiException);


    short mCreation;
    static short smCreationCounter;

    unsigned int mPidCount;
    unsigned int mPortCount;
    unsigned int mSerial;
    unsigned int mRefId[3];

    ErlangTransport* mTransport;
};



} // node
} // epi


#endif // _EPILOCALNODE_H

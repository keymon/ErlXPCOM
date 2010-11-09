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

#ifndef _EICONNECTION_H
#define _EICONNECTION_H

#include "OpenThreads/Thread"
#include "OpenThreads/Mutex"

#include "Socket.hpp"
#include "EpiConnection.hpp"

namespace epi {
namespace ei {

using namespace epi::type;
using namespace epi::error;
using namespace epi::node;

class EIMessageAcceptor;

/**
 * This class represents a connection with an erlang node using
 * EI library
 */
class EIConnection: public Connection
{
    friend class EIMessageAcceptor;
public:
    /**
     * Create a new connection. The connection is stoped by default, and
     * no receiver is defined. Programmer MUST define a receiver and
     * start the thread using setReceiver() and  start() method
     * @param peer Peer information
     * @param cookie Cookie for this connection
     * @param aSocket Socket for this connection
     */
    EIConnection(PeerNode *peer, std::string cookie, Socket *aSocket);

    virtual ~EIConnection();

    /**
     * Create a new OutputBuffer to be used with this sender
     */
    virtual OutputBuffer* newOutputBuffer();

    /**
     * Send a buffer to a pid.
     * @param from From pid
     * @param to Destination pid
     * @param buffer OutputBuffer to send data
     * @throw EpiConnectionException if send fails
     */
    virtual void sendBuf( epi::type::ErlPid* from,
                          epi::type::ErlPid* to,
                          epi::node::OutputBuffer* buffer )
            throw (EpiConnectionException);

    /**
     * Send a buffer to a registered server.
     * @param from From pid
     * @param to Destination name
     * @param buffer OutputBuffer to send data
     * @throw EpiConnectionException if send fails
     */
    virtual void sendBuf( epi::type::ErlPid* from,
                          const std::string &to,
                          epi::node::OutputBuffer* buffer )
            throw (EpiConnectionException);

    /**
     * Send a buffer to a registered server in the given node.
     * In the connection, this method will send the data directly
     * to remote node, ignoring the node name.
     * @param from From pid
     * @param node Note to send message
     * @param to Destination name
     * @param buffer OutputBuffer to send data
     * @throw EpiConnectionException if send fails
     */
    virtual void sendBuf( ErlPid* from,
                          const std::string &node,
                          const std::string &to,
                          OutputBuffer* buffer )
            throw (EpiConnectionException);

    virtual void start();

    virtual void stop();

    virtual void close();

protected:
    Socket *mSocket;
    EIMessageAcceptor *mAcceptor;
    OpenThreads::Mutex _socketMutex;
};


} // ei
} // epi

#endif

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

#ifndef _EPICONNECTION_H
#define _EPICONNECTION_H

#include "EpiNode.hpp"
#include "EpiMessage.hpp"
#include "EpiObserver.hpp"
#include "EpiReceiver.hpp"
#include "EpiSender.hpp"

namespace epi {
namespace node {

/**
 * This class represents a connection with an erlang node
 */
class Connection: public EpiSender, public EpiReceiver
{
public:

    /**
     * Init this connection data.
     * Programmer MUST define a receiver before start to
     * deliver messages
     * @param peer Peer information
     * @param cookie Cookie for this connection
     */
    Connection(PeerNode *peer, std::string cookie);

    /**
     * Destroy the connection
     */
    virtual ~Connection();

    /**
     * Create a new OutputBuffer to be used with this sender
     */
    virtual OutputBuffer* newOutputBuffer() = 0;

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
            throw (EpiConnectionException) = 0;

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
            throw (EpiConnectionException) = 0;

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
    virtual void sendBuf( epi::type::ErlPid* from,
                          const std::string &node,
                          const std::string &to,
                          epi::node::OutputBuffer* buffer )
            throw (epi::error::EpiConnectionException) = 0;

    /**
     * Start accepting and delivering messages. This method will
     * usually start a thread that receives incoming messages for
     * this connection and delivers it to receiver
     */
    virtual void start() = 0;

    /**
     * Stop accepting and delivering messages. It will stop
     * the thread that receives and delivers messages
     */
    virtual void stop() = 0;

    /**
     * Close this connection. Stops the message aceptor and closes
     * the connection.
     */
    virtual void close() = 0;

    /**
     * Get the cookie for this connection
     */
    std::string getCookie() const;

    /**
     * Get the peer for this connection
     */
    PeerNode *getPeer();

    /**
     * Set the receiver.
     */
    void setReceiver(EpiReceiver *receiver);

    /**
     * Deliver a message to this connection. The connection will
     * foward it to the receiver
     */
    void deliver( void *origin, epi::node::EpiMessage* msg );


protected:
    EpiReceiver *mReceiver;
    std::auto_ptr<PeerNode> mPeer;
    std::string mCookie;

};

} // node
} // epi

#endif

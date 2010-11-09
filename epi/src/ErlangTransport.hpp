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

#ifndef _ERLANGTRANSPORT_HPP
#define _ERLANGTRANSPORT_HPP

#include <string>
#include "EpiError.hpp"

namespace epi {
namespace node {

using namespace epi::error;

/**
 * Provides an abstract interface for different implementations of
 * connections to erlang world, like Erlang messages via TCP/IP,
 * drivers, etc...
 */
class ErlangTransport {
public:

    /**
     * Set up a connection to an Erlang node, using the default cookie
     * @param node node name to connect
     * @returns A new connection. The connection has no receiver defined
     * and is not started.
     */
    virtual Connection* connect(const std::string node)
            throw(EpiConnectionException) = 0;

    /**
     * Set up a connection to an Erlang node, using other cookie
     * @param node node name to connect
     * @param cookie cookie to use
     * @returns A new connection. The connection has no receiver defined
     * and is not started.
     */
    virtual Connection* connect(const std::string node, const std::string cookie)
            throw(EpiConnectionException) = 0;

    /**
     * Accept a connection from a client process.
     * This method sets the socket to listen incoming connections.
     * @param timeout timeout. 0 to infinite
     * @returns A new connection. The connection has no receiver defined
     * and is not started. Returns 0 if timeout.
     */
    virtual Connection* accept( long timeout = 0 )
            throw(EpiConnectionException) = 0;

    /**
     * Accept a connection from a client process using other cookie
     * This method sets the socket to listen incoming connections.
     * @param cookie cookie to use
     * @param timeout timeout. 0 to infinite
     * @returns A new connection. 
     * The connection has no receiver defined and is not started.
     * Returns 0 if timeout
     */
    virtual Connection* accept(const std::string cookie, long timeout = 0)
            throw(EpiConnectionException) = 0;

    /**
     * Publish the node port
     */
    virtual void publishPort() throw (EpiConnectionException) = 0;

    /**
     * Unpublish the node port
     */
    virtual void unPublishPort() throw (EpiConnectionException) = 0;

    /**
     * Get the node name for this transport
     */
    virtual std::string getNodeName() = 0;

protected:

};

} // node
} // epi


#endif

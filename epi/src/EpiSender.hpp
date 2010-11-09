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

#ifndef __EPISENDER_HPP
#define __EPISENDER_HPP

#include "EpiException.hpp"
#include "ErlTypes.hpp"
#include "EpiOutputBuffer.hpp"

namespace epi {
namespace node {

class EpiSender
{

public:
    /**
     * Create a new OutputBuffer to be used with this sender
     */
    virtual OutputBuffer* newOutputBuffer() = 0;


    /**
     * Send a buffer to a pid.
     * The caller maintains the buffer owership, but
     * the buffer can be modified.
     * @param from From pid
     * @param to Destination pid
     * @param buffer OutputBuffer to send data
     * @throw EpiConnectionException if send fails
     */
	virtual void sendBuf( epi::type::ErlPid* from,
                          epi::type::ErlPid* to,
                          epi::node::OutputBuffer* buffer )
            throw (epi::error::EpiConnectionException)= 0;
    /**
     * Send a buffer to a registered server.
     * The caller maintains the buffer owership, but
     * the buffer can be modified.
     * @param from From pid
     * @param to Destination name
     * @param buffer OutputBuffer to send data
     * @throw EpiConnectionException if send fails
     */
    virtual void sendBuf( epi::type::ErlPid* from,
                          const std::string &to,
                          epi::node::OutputBuffer* buffer )
            throw (epi::error::EpiConnectionException) = 0;

    /**
     * Send a buffer to a registered server in the given node
     * The caller maintains the buffer owership, but
     * the buffer can be modified.
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

};

} // namespace node
} // namespace epi

#endif //__EPISENDER_HPP

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

/**
 * Misc util functions, like type conversion between EI library and
 * epi library.
 */

#ifndef _EPIUTIL_H
#define _EPIUTIL_H

#include <string>

#include "ErlTypes.hpp"
#include "EpiMessage.hpp"
#include "ei.h"


namespace epi {
namespace util {

using namespace epi::type;
using namespace epi::node;

/**
 * New ErlPid from EI representation.
 * @param pid erlang_pid pointer to EI pid struct
 * @return a new pointer to an ErlPid, or null if pid==null or
 *         pid is invalid
 */
ErlPid* EI2ErlPid(const erlang_pid* pid);

/**
 * New EI pid from an ErlPid
 * @param pid pointer to an ErlPid
 * @return a pointer to a new erlang_pid, or null if pid=null or
   *         is invalid
 */
erlang_pid* ErlPid2EI(const ErlPid *pid);

/**
 * New ErlRef from EI representation
 * @param ref erlang_ref pointer to ref struct
 * @return a new pointer to an ErlRef, or null if ref==null or
 *         ref is invalid
 */
ErlRef* EI2ErlRef(const erlang_ref* ref);

/**
 * New EI ref from an ErlRef
 * @param ref pointer to an ErlRef
 * @return a pointer to a new erlang_ref, or null if ref=null or
 *         is invalid
 */
erlang_ref* ErlRef2EI(const ErlRef *ref);

/**
 * New ErlPort from EI representation
 * @param port erlang_port pointer to port struct
 * @return a new pointer to an ErlPort, or null if port==null or
 *         port is invalid
 */
ErlPort* EI2ErlPort(const erlang_port* port);

/**
 * New EI port from an ErlPort
 * @param port pointer to an ErlPort
 * @return a pointer to a new erlang_port, or null if port=null or
 *         is invalid
 */
erlang_port* ErlPort2EI(const ErlPort *port);

/**
 * Copy an ei_cnode with an change the cookie
 */
ei_cnode *EiCNodeChangeCookie(const ei_cnode *ec,
                              const std::string newCookie);

/**
 * Convert a erlang_msg to a EpiMessage
 *
 * @param msg erlang_msg to convert
 * @param buffer OutputBuffer to use. Owership of buffer will be transfered
 */
EpiMessage *ToMessage(erlang_msg* msg, InputBuffer *buffer)
        throw (EpiUnknownMessageException);

} // namespace error
} // namespace epi

#endif

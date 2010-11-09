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


#include <memory>

#include "EpiUtil.hpp"

using namespace epi::type;
using namespace epi::node;
//using namespace epi::util;

/**
 * New ErlPid from EI representation.
 * @param pid erlang_pid pointer to EI pid struct
 * @return a new pointer to an ErlPid, or null if pid==null or
 *         pid is invalid
 */
ErlPid* epi::util::EI2ErlPid(const erlang_pid* pid) {
     if (pid) {
         ErlPid* newPid = new ErlPid(pid->node, pid->num,
                                     pid->serial, pid->creation);
         return newPid;
     }
     return 0;
}

/**
 * New EI pid from an ErlPid
 * @param pid pointer to an ErlPid
 * @return a pointer to a new erlang_pid, or null if pid=null or
 *         is invalid
 */
erlang_pid* epi::util::ErlPid2EI(const ErlPid *pid) {
     if (pid && pid->isValid()) {
          erlang_pid *newPid = new erlang_pid;
          strcpy(newPid->node, pid->node().c_str());
          newPid->num = pid->id();
          newPid->serial = pid->serial();
          newPid->creation = pid->creation();
          return newPid;
     }
     return 0;
}

/**
 * New ErlRef from EI representation
 * @param ref erlang_ref pointer to ref struct
 * @return a new pointer to an ErlPid, or null if pid==null or
 *         pid is invalid
 */
ErlRef* epi::util::EI2ErlRef(const erlang_ref* ref) {
    if (ref) {
        bool newStyle = ref->len != 1;
        ErlRef* newRef = new ErlRef(ref->node, ref->n,
                                    ref->creation, newStyle);

        if (newRef->isValid())
            return newRef;
        else
            delete newRef;
    }
    return 0;
}

/**
 * New EI ref from an ErlRef
 * @param ref pointer to an ErlRef
 * @return a pointer to a new erlang_ref, or null if ref=null or
 *         is invalid
 */
erlang_ref* epi::util::ErlRef2EI(const ErlRef *ref) {
    if (ref && ref->isValid()) {
        erlang_ref *newRef = new erlang_ref;
        strcpy(newRef->node, ref->node().c_str());
        if (ref->isNewStyle()) {
            newRef->len = 3;
            newRef->n[0] = ref->id(0);
            newRef->n[1] = ref->id(1);
            newRef->n[2] = ref->id(2);
        } else {
            newRef->len = 1;
            newRef->n[0] = ref->id(0);
        }
        newRef->creation = ref->creation();
        return newRef;
    }
    return 0;
}

/**
 * New ErlPort from EI representation
 * @param port erlang_port pointer to port struct
 * @return a new pointer to an ErlPid, or null if pid==null or
 *         pid is invalid
 */
ErlPort* epi::util::EI2ErlPort(const erlang_port* port) {
    if (port) {
        ErlPort* newPort = new ErlPort(port->node, port->id,
                                       port->creation);

        return newPort;
    }
    return 0;
}

/**
 * New EI port from an ErlPort
 * @param port pointer to an ErlPort
 * @return a pointer to a new erlang_port, or null if port=null or
 *         is invalid
 */
erlang_port* epi::util::ErlPort2EI(const ErlPort *port) {
    if (port && port->isValid()) {
        erlang_port *newPort = new erlang_port;
        strcpy(newPort->node, port->node().c_str());
        newPort->id = port->id();
        newPort->creation = port->creation();
        return newPort;
    }
    return 0;
}

ei_cnode *epi::util::EiCNodeChangeCookie(const ei_cnode *ec,
                                         const std::string newCookie) {
    ei_cnode *new_ec = new ei_cnode;
    memcpy(new_ec, ec, sizeof(ei_cnode));
    memcpy(new_ec->ei_connect_cookie, newCookie.c_str(), newCookie.length());
    return new_ec;
}

EpiMessage *epi::util::ToMessage(erlang_msg* msg, InputBuffer *buffer)
    throw (EpiUnknownMessageException)
{
    EpiMessage *msgResult = 0;
    // Ensure buffer deletion
    std::auto_ptr<InputBuffer> _buffer(buffer);

    switch (msg->msgtype) {
    case ERL_SEND:
        msgResult = new SendMessage(EI2ErlPid(&msg->to), _buffer.release());
        break;
    case ERL_REG_SEND:
        msgResult = new RegSendMessage(EI2ErlPid(&msg->from),
                                       msg->toname, _buffer.release());
        break;
    case ERL_EXIT:
    case ERL_EXIT2:

        ErlTerm *t;

        try {
            t = _buffer->readTerm();
        } catch (EpiDecodeException &e) {
            throw EpiUnknownMessageException("Error decoding reason atom");
        }
        if (t->instanceOf(ERL_ATOM)) {
            msgResult = new ExitMessage(EI2ErlPid(&msg->from),
                                        EI2ErlPid(&msg->to),
                                        (ErlAtom *) t);
        } else {
            throw EpiUnknownMessageException("Error decoding reason atom");
        }
        break;

    case ERL_LINK:
        msgResult = new LinkMessage(EI2ErlPid(&msg->from),
                                    EI2ErlPid(&msg->to));
        break;
    case ERL_UNLINK:
        msgResult = new UnLinkMessage(EI2ErlPid(&msg->from),
                                      EI2ErlPid(&msg->to));
        break;

    default:
        throw EpiUnknownMessageException("Unknown message type received");
    }

    return msgResult;
}

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

#ifndef _ERLPID_HPP
#define _ERLPID_HPP

#include "ErlTerm.hpp"

namespace epi {
namespace type {

/**
 * Representation of erlang Pids.
 * A pid has 4 parameters, nodeName, id, serial and creation number
 *
 */
class ErlPid: public ErlTerm {
public:

    /**
     * Create a unitialized Pid
     */
    inline ErlPid():ErlTerm() { }

    /**
     * Create an Erlang pid from its components.
     * @param node the nodename.
     * @param id an arbitrary number. Only the low order 15 bits will
     * be used.
     * @param serial another arbitrary number. Only the low order 13 bits
     * will be used.
     * @param creation yet another arbitrary number. Only the low order
     * 2 bits will be used.
     * @throw EpiBadArgument if node is empty or greater than MAX_NODE_LENGTH
     **/
    ErlPid(std::string node, int id, int serial, int creation)
            throw(EpiBadArgument ):ErlTerm()
    {
        try {
            init(node, id, serial, creation);
        } catch (EpiAlreadyInitialized &e) {
        }
    }

    /**
     * Init the Pid.
     *
     * @param node the nodename.
     * @param id an arbitrary number. Only the low order 15 bits will
     * be used.
     * @param serial another arbitrary number. Only the low order 13 bits
     * will be used.
     * @param creation yet another arbitrary number. Only the low order
     * 2 bits will be used.
     * @throws EpiBadArgument if nodename size is greater than
     *   MAX_NODE_LENGTH. Results in no change in the term
     *   (same value, same state).
     * @throws EpiAlreadyInitialized  if the pid is already initialized
     */
    void init(std::string node, int id, int serial, int creation)
            throw(EpiBadArgument, EpiAlreadyInitialized);

    /**
     * Get the node name from the PID.
     *
     * @return the node name from the PID.
     **/
    inline std::string node() const {
        return mNode;
    }

    /**
     * Get the id number from the PID.
     *
     * @return the id number from the PID.
     **/
    inline int id() const {
        return mId;
    }

    /**
     * Get the serial number from the PID.
     *
     * @return the serial number from the PID.
     **/
    inline int serial() const {
        return mSerial;
    }

    /**
     * Get the creation number from the PID.
     *
     * @return the creation number from the PID.
     **/
    inline int creation() const {
        return mCreation;
    }

    bool equals(const ErlTerm &t) const;

    /**
     * Get the string representation of the PID. Erlang PIDs are printed
     * as #Pid&lt;node.id.serial&gt;
     * @return the string representation of the PID.
     **/
    std::string toString(const VariableBinding *binding = 0) const;

    IMPL_TYPE_SUPPORT(ErlPid, ERL_PID);

private:
    ErlPid(const ErlPid &t) {}
    inline ~ErlPid() { }


protected:
    std::string mNode;
    int mId;
    int mSerial;
    int mCreation;

};

/** less operator, needed for maps */
bool operator<(const ErlPid &t1, const ErlPid &t2);


} //namespace type
} //namespace epi

#endif // _ERLPID_HPP


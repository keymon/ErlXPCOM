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

#ifndef _ERLPORT_HPP
#define _ERLPORT_HPP

#include "ErlTerm.hpp"

namespace epi {
namespace type {

/**
 * Representation of erlang Ports.
 * A port has 3 parameters, nodeName, id and creation number
 */
class ErlPort: public ErlTerm {
public:

    /**
     * Create a unitialized Port
     */
    inline ErlPort():ErlTerm() {}

    /**
     * Create an Erlang port from its components.
     * If node string size is greater than MAX_NODE_LENGTH or = 0,
     * the ErlAtom object is created but invalid.
     * @param node the nodename.
     * @param id an arbitrary number. Only the low order 18
     * bits will be used.
     * @param creation yet another arbitrary number. Only the low order
     * 2 bits will be used.
     * @throw EpiBadArgument if node is empty or greater than MAX_NODE_LENGTH
     **/
    ErlPort(const std::string node, const int id, const int creation)
            throw(EpiBadArgument): ErlTerm()
    {
        try {
            init(node, id, creation);
        } catch (EpiAlreadyInitialized &e) {
        }
    }

    /**
     * Init the Port.
     * @param node the nodename.
     * @param id an arbitrary number. Only the low order 18
     * bits will be used.
     * @param creation yet another arbitrary number. Only the low order
     * 2 bits will be used.
     * @throws EpiBadArgument if nodename size is greater than
     *   MAX_NODE_LENGTH. Results in no change in the term
     *   (same value, same state).
     * @throws EpiAlreadyInitialized  if the pid is already initialized
     */
    void init(const std::string node, const int id, const int creation)
            throw(EpiBadArgument, EpiAlreadyInitialized);

    /**
     * Get the node name from the PORT.
     *
     * @return the node name from the PORT.
     **/
    inline std::string node() const {
        return mNode;
    }

    /**
     * Get the id number from the PORT.
     *
     * @return the id number from the PORT.
     **/
    inline int id() const {
        return mId;
    }

    /**
     * Get the creation number from the PORT.
     *
     * @return the creation number from the PORT.
     **/
    inline int creation() const {
        return mCreation;
    }

    bool equals(const ErlTerm &t) const;

    /**
     * Get the string representation of the PORT. Erlang PORTs are printed
     * as #Port<node.id>;
     * @return the string representation of the PORT.
     **/
    std::string toString(const VariableBinding *binding = 0) const;

    IMPL_TYPE_SUPPORT(ErlPort, ERL_PORT);

private:
    ErlPort(const ErlPort &t) {}
    inline ~ErlPort() {}


protected:
    std::string mNode;
    int mId;
    int mCreation;

};

} //namespace type
} //namespace epi

#endif // _ERLPORT_HPP


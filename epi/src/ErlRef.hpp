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

#ifndef _ERLREF_HPP
#define _ERLREF_HPP

#include "ErlTerm.hpp"

namespace epi {
namespace type {

/**
 * Representation of erlang Refs.
 * A ref has 3 parameters, nodeName, ids and creation number
 * The ids
 * There are two styles of Erlang refs, old style (one id value)
 * and new style (array of id values).
 */
class ErlRef: public ErlTerm {
public:

    /**
     * Create a unitialized Ref
     */
    inline ErlRef():ErlTerm() {};

    /**
     * Create an Erlang ref from its components.
     * If node string size is greater than MAX_NODE_LENGTH or = 0,
     * the ErlAtom object is created but invalid.
     * @param node the nodename.
     * @param ids an array of arbitrary numbers. Only the low order 18
     * bits of the first number will be used.
     * @param creation yet another arbitrary number. Only the low order
     * 2 bits will be used.
     * @throw EpiBadArgument if node is empty or greater than MAX_NODE_LENGTH
     **/
    ErlRef(const std::string node, const unsigned int ids[],
           const unsigned int creation, const bool newStyle=true)
            throw(EpiBadArgument): ErlTerm()
    {
        try {
            init(node, ids, creation, newStyle);
        } catch (EpiAlreadyInitialized &e) {
        }
    }

    inline ~ErlRef() { }

    /**
     * Init the Ref.
     *
     * @param node the nodename.
     * @param ids an array of arbitrary numbers. Only the low order 18
     * bits of the first number will be used.
     * @param creation yet another arbitrary number. Only the low order
     * 2 bits will be used.
     * @param newStyle The ref is new style (id = array of three numbers)
     * @throws EpiBadArgument if nodename size is greater than
     *   MAX_NODE_LENGTH. Results in no change in the term
     *   (same value, same state).
     * @throws EpiAlreadyInitialized  if the ref is already initialized
     */
    void init(const std::string node, const unsigned int ids[],
              const unsigned int creation, const bool newStyle=true)
            throw(EpiBadArgument, EpiAlreadyInitialized);

    /**
     * Get the node name from the REF.
     *
     * @return the node name from the REF.
     **/
    inline std::string node() const {
        return mNode;
    }

    /**
     * Get an id number from the REF.
     * @param index Index of id to return, from 0 to 2
     *
     * @return the id number from the REF.
     **/
    inline int id(int index) const {
        return mIds[index];
    }

    /**
     * Get the id array from the REF.
     * @return the id array number from the REF. It must be deleted
     **/
    inline int* ids(int index) const {
        int *ids = new int[3];
        ids[0]=mIds[0];
        ids[1]=mIds[1];
        ids[2]=mIds[2];
        return ids;
    }

    /**
     * Get the creation number from the REF.
     * @return the creation number from the REF.
     **/
    inline int creation() const {
        return mCreation;
    }

    /**
     * Check if the ref is new style reference
     */
    inline bool isNewStyle() const {
        return mNewStyle;
    }

    bool equals(const ErlTerm &t) const;

    /**
     * Get the string representation of the REF. Erlang REFs are printed
     * as #Ref<node.id>
     * @return the string representation of the REF.
     **/
    std::string toString(const VariableBinding *binding = 0) const;

    IMPL_TYPE_SUPPORT(ErlRef, ERL_REF);

private:
    ErlRef(const ErlRef &t) {}

protected:
    std::string mNode;
    unsigned int mIds[3];
    unsigned int mCreation;
    bool mNewStyle;

};

} //namespace type
} //namespace epi

#endif // _ERLREF_HPP


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

#ifndef _ERLLIST_HPP
#define _ERLLIST_HPP

#include "ErlTerm.hpp"
#include "ErlTermPtr.hpp"

namespace epi {
namespace type {

/**
 * Representation of Erlang lists.
 * This is the base class for two specific classes:
 *   - Empty lists, with no elements.
 *   - Cons Lists with one *or more* elements and a tail.
 *
 * The tail can be:
 *   - A empty list, so the list is a proper list. If the tail is a not empty
 *     list, you can access the elements of the tail directly with elementAt()
 *     in the base list.
 *   - Other type of term.
 *
 * The arity of the list is the number of elements that it contains plus
 * the arity of the tail list (if it is a proper list).
 *
 * A list is closed (state VALID) if a tail is defined.
 * If a tail is closed no more elements can be added and the tail can't be
 * changed.
 *
 * An empty list is a special list with NO elements and NO tail. It's used
 * as final tail in proper lists.
 */
class ErlList: public ErlTerm {
public:

    inline ErlList(): ErlTerm() {}

    /**
     * Get the list arity, the number of elements in the list.
     */
    virtual unsigned int arity() const = 0;

    /**
     * Get one the element by position (between 0 and arity-1)
     * @returns a pointer to the element. The pointer is
     *  at least referenced by this list.
     * @throws EpiBadArgument if index is out of range
     * @throws EpiEmptyList if the list is empty
     * @throws EpiInvalidTerm if the list is not closed  (is invalid)
     */
    virtual ErlTerm* elementAt(unsigned int index) const
            throw(EpiInvalidTerm, EpiBadArgument, EpiEmptyList) = 0;

    /**
     * Get nth tail. If index is less than arity-1, the returned
     * pointer is a new ErlConsList containing references
     * to the elements after index position, and the pointer is
     * zero referenced (should be deleted).
     * If position is = arity-1, the last tail is returned, and
     * is referenced at least by this list.
     * @param index Tail position to get
     * @return A pointer to a new ErlTerm with the tail at given position.
     * @throw EpiInvalidTerm if list is not initialized
     * @throw EpiBadArgument if position >= arity
     * @throw EpiEmptyList if list is empty.
     */
    virtual ErlTerm* tail(unsigned int index) const
            throw(EpiInvalidTerm, EpiBadArgument, EpiEmptyList) = 0;

    virtual bool equals(const ErlTerm &t) const;

	IMPL_TYPE_SUPPORT(ErlList, ERL_LIST);

protected:
	virtual bool internalMatch(VariableBinding* binding, ErlTerm* pattern)
		throw (EpiVariableUnbound);


};

} //namespace type
} //namespace epi

#endif // _ERLLIST_HPP

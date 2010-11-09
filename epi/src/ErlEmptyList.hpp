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

#ifndef _ERLEMPTYLIST_HPP
#define _ERLEMPTYLIST_HPP

#include "ErlList.hpp"

namespace epi {
namespace type {

/**
 * Representation of a erlang empty list. It have no concrete state.
 */
class ErlEmptyList: public ErlList {
public:

    /**
     * Create an empty list.
     */
    inline ErlEmptyList() {
        mInitialized = true;
    }


    /*
     * Is valid
     */
    inline bool isValid() const {
        return true;
    }

    inline unsigned int arity() const {
        return 0;
    }

    inline ErlTerm* elementAt(unsigned int index) const
            throw(EpiEmptyList)
    {
        throw EpiEmptyList("List is empty");
    }

    inline ErlTerm* tail(unsigned int index) const
            throw(EpiEmptyList)
    {
        throw EpiEmptyList("List is empty");
    }

    inline std::string toString(const VariableBinding *binding = 0) const {
        return "[]";
    }

	IMPL_TYPE_SUPPORT2(ErlEmptyList, ERL_EMPTY_LIST, ERL_LIST);
private:
    ErlEmptyList(const ErlEmptyList &t) {}

protected:
};

} //namespace type
} //namespace epi

#endif // _ERLEMPTYLIST_HPP

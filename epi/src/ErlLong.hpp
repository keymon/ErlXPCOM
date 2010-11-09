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

#ifndef _ERLLONG_HPP
#define _ERLLONG_HPP

#include "ErlTerm.hpp"

namespace epi {
namespace type {

/**
 * Provides a representation of Erlang integers
 **/
class ErlLong: public ErlTerm {

public:

    /**
     * Create an unitialized long
     **/
    inline ErlLong():ErlTerm() {}

    /**
     * Create long from the given value.
     * @param value Value for the new long
     **/
    inline ErlLong(const long value): mValue(value) {
        mInitialized=true;
    }

    /**
     * Init this long with the given value.
     * @param value Value for the long
     * @throws EpiAlreadyInitialized if the double is already initialized
     */
    void init(const long long value)
            throw(EpiAlreadyInitialized);

    /**
     * Get the actual value contained in this term.
     **/
    long long longValue() const throw(EpiInvalidTerm);

    bool equals(const ErlTerm &t) const;

    std::string toString(const VariableBinding *binding = 0) const;

    IMPL_TYPE_SUPPORT(ErlLong, ERL_LONG);


private:
    ErlLong (const ErlLong &t) {}
    inline ~ErlLong() {}

protected:
    long long mValue;

};

} //namespace type
} //namespace epi


#endif // _ERLLONG_HPP

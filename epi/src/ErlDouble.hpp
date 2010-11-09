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

#ifndef _ERLDOUBLE_HPP
#define _ERLDOUBLE_HPP

#include "ErlTerm.hpp"

namespace epi {
namespace type {

/**
 * Provides a representation of Erlang floats and doubles
 **/
class ErlDouble: public ErlTerm {

public:

    /**
     * Create an unitialized double
     **/
    inline ErlDouble():ErlTerm() {};

    /**
     * Create a double from the given value.
     * @param value Value for the new double
     **/
    inline ErlDouble(const double value):ErlTerm(), mValue(value) {
        mInitialized = true;
    }

    /**
     * Init this double with the given value.
     *
     * @param value Value for the double
     * @throws EpiAlreadyInitialized if the double is already initialized
     */
    void init(const double value)
            throw(EpiAlreadyInitialized);


    /**
     * Get the actual value contained in this term.
     **/
    double doubleValue() const
            throw(EpiInvalidTerm);

    bool equals(const ErlTerm &t) const;

    std::string toString(const VariableBinding *binding = 0) const;

    IMPL_TYPE_SUPPORT(ErlDouble, ERL_DOUBLE);

private:
    ErlDouble (const ErlDouble &t) {}
    inline ~ErlDouble() { }

protected:
    double mValue;

};

} //namespace type
} //namespace epi


#endif // _ERLDOUBLE_HPP

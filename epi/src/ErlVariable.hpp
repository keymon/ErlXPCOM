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

#ifndef __ERLVARIABLE_HPP
#define __ERLVARIABLE_HPP

#include "ErlTerm.hpp"

namespace epi {
namespace type {


/**
 * Provides a representation of an Variable.
 * A variable has a name (string). The name can be uppercase or not.
 * Always equals to false.
 * The essencial difference is in match method.
 * If you use '_' as variable, it will allways succes in matching, but
 * it will not be bind.
 **/
class ErlVariable: public ErlTerm {

public:
    /**
     * Create a new anonymous variable
     */
    inline ErlVariable(): ErlTerm(), mName("_") {
        mInitialized = true;
    }
    /**
     * Create a new variable. If you use "_" as name, it will be an
     * anonymous variable
     */
    inline ErlVariable( std::string name ): ErlTerm(), mName(name) {
        mInitialized = true;
    }

    inline std::string getName( ) {
        return mName;
    }

    inline bool equals(const ErlTerm &t) const {
        return false;
    }

    std::string toString(const VariableBinding *binding = 0) const;

    ErlVariable* searchUnbound(const VariableBinding* binding);

    ErlTerm* subst(const VariableBinding* binding)
            throw (EpiInvalidTerm, EpiVariableUnbound);

    IMPL_TYPE_SUPPORT(ErlVariable, ERL_VARIABLE);

private:

    std::string mName;

protected:

    bool internalMatch(VariableBinding* binding, ErlTerm* pattern)
            throw (EpiVariableUnbound);

};

} // type
} // epi


#endif

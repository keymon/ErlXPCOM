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


#include <iostream>
#include <sstream>
#include <memory>

#include "ErlTerm.hpp"
#include "ErlTermPtr.hpp"
#include "VariableBinding.hpp"

using namespace epi::error;
using namespace epi::type;
/*
std::string StringOfTermType(const TermType t) {
    std::string erl_type_names[] =
    { "ErlAtom", "ErlLong", "ErlDouble", "ErlString", "ErlRef",
    "ErlPort", "ErlPid", "ErlBinary", "ErlTuple", "ErlList",
    "ErlEmptyList", "ErlConsList" };
    return erl_type_names[t];
}
*/

bool ErlTerm::isValid() const {
    return mInitialized;
}


// By default, return himself
ErlTerm* ErlTerm::subst(const VariableBinding* binding)
        throw (EpiInvalidTerm, EpiVariableUnbound)
{
    if (!isValid())
        throw EpiInvalidTerm("Element is not initialized");

    return this;
}

bool ErlTerm::match(ErlTerm* pattern, VariableBinding* binding)
        throw (EpiVariableUnbound)
{

    // Protect the given binding. Just change it if the match success
    VariableBinding* dirtyBinding =
        binding? new VariableBinding(*binding):
                 new VariableBinding();
	
	// If the given term is a variable, match with it
	bool success;
    if (pattern->instanceOf(ERL_VARIABLE)) {
		success = pattern->internalMatch(dirtyBinding, this);
	} else {
		success = this->internalMatch(dirtyBinding, pattern);
    }

    if (success && binding) {
        binding->merge(dirtyBinding);
    }
	
    delete dirtyBinding;
    return success;
}

bool ErlTerm::internalMatch(VariableBinding* binding, ErlTerm* pattern)
        throw (EpiVariableUnbound)
{
    Dout(dc::erlang, "ErlTerm::internalMatch()");
    // default behaviour.
    if (!this->isValid()) {
        return false;
    }
    if (pattern->instanceOf(ERL_VARIABLE)) {
        Dout(dc::erlang, "Pattern parameter is a variable, conmute");
        return pattern->internalMatch(binding, this);
    }
    return equals(*pattern);
}


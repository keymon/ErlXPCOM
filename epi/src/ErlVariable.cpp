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


#include "ErlVariable.hpp"
#include "VariableBinding.hpp"

using namespace epi::type;

std::string ErlVariable::toString(const VariableBinding *binding) const {
    if (mName == "_") {
        return mName;
    }
    ErlTerm* term = binding? binding->search(mName): 0;
    if (term) {
        return term->toString();
    } else {
        return mName;
    }

}

ErlVariable* ErlVariable::searchUnbound(const VariableBinding* binding) {
    if (mName == "_") {
        return 0;
    }
    ErlTerm* term = binding? binding->search(mName): 0;
    if (term) {
        return 0;
    } else {
        return this;
    }
}

ErlTerm* ErlVariable::subst(const VariableBinding* binding)
        throw (EpiInvalidTerm, EpiVariableUnbound) {
    if (mName == "_") {
        throw EpiVariableUnbound("_");
    }
    ErlTerm* term = binding? binding->search(mName): 0;
    if (term) {
        return term;
    } else {
        throw EpiVariableUnbound(mName);
    }
}


bool ErlVariable::internalMatch(VariableBinding* binding, ErlTerm* pattern)
        throw(EpiVariableUnbound)
{
    Dout(dc::erlang, "Matching variable '"<< mName << "' with " << pattern->toString());
    // If is anonymous, return true :)
    if (mName == "_") {
        return true;
    }

    // first check if this variable is bound
    ErlTerm* boundValue = binding? binding->search(mName): 0;
    if (boundValue) {
        Dout(dc::erlang, "Variable is bound to '"<< boundValue->toString(binding) <<"'");
        // perforn the matching with the value
        return boundValue->internalMatch(binding, pattern);
    } else {
        // bind the variable
        if (binding != 0)  {
            Dout_continue(dc::erlang, _continue, "Failed.", "Variable is unbound, binding. ");
            binding->bind(mName, pattern->subst(binding));
            Dout_finish(_continue, "'" << mName << "' bound to '" << pattern->toString(binding) << "'");
        }
        return true;
    }
}


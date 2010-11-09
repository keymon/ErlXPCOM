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
#include <string.h>

#include "ErlAtom.hpp"
#include "EpiBuffer.hpp"

using namespace epi::error;
using namespace epi::type;

void ErlAtom::init(const char *atom, bool copy)
        throw (EpiBadArgument,
               EpiAlreadyInitialized)
{
    if (isValid()) {
        throw EpiAlreadyInitialized("Atom already initialized");
    }
	unsigned int size = strlen(atom);
	
    if (size > MAX_ATOM_LENGTH) {
        std::ostringstream oss;
        oss << "Atom must not exceed " << MAX_ATOM_LENGTH << " characters";
        throw EpiBadArgument(oss.str());
    } else if (size == 0) {
        throw EpiBadArgument("Atom must be non-empty");
    }

    if (copy) {
		mAtom = new char[size+1];
		strncpy((char*)mAtom, atom, size+1);
	} else {
		mAtom =  atom;
	}
		
    mInitialized = true;

}

bool ErlAtom::equals(const ErlTerm &t) const {
    if (!t.instanceOf(ERL_ATOM))
        return false;

    if (!this->isValid() || !t.isValid())
        return false;

    ErlAtom *_t = (ErlAtom*) &t;
    return strcmp(mAtom, _t->mAtom) == 0;
}

const char* ErlAtom::atomValue() const
        throw(EpiInvalidTerm)
{
    if (!isValid()) {
        throw EpiInvalidTerm("Atom is not initialized");
    }
    return mAtom;
}


/* FIXME: return escaped string */
std::string ErlAtom::toString(const VariableBinding *binding) const {
    if (!isValid())
        return "*** INVALID ATOM ***";
    return mAtom;
}

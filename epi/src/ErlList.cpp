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


#include "ErlList.hpp"

using namespace epi::error;
using namespace epi::type;

bool ErlList::equals(const ErlTerm &t) const {
    // Check if the term is the same class
    if (!this->isValid() || !t.isValid())
        return false;
    if (!t.instanceOf(ERL_LIST))
        return false;
	
    const ErlList* _t = (ErlList *) &t;
	
    if (_t->arity() != this->arity()) {
        return false;
    }
	
	if (this->arity()>0) {
		for (unsigned int i = 0; i < this->arity(); i++) {
			ErlTermPtr<> a(this->elementAt(i));
			ErlTermPtr<> b(_t->elementAt(i));
			if (!a->equals(*b.get()))
				return false;
		}
		/* Check the tail */
		ErlTermPtr<> a(this->tail(arity()-1));
		ErlTermPtr<> b(_t->tail(arity()-1));
		if (!a->equals(*b.get()))
			return false;
	}
	return true;
}

bool ErlList::internalMatch(VariableBinding* binding, ErlTerm* pattern)
        throw (EpiVariableUnbound) {

    if (!this->isValid() || !pattern->isValid())
        return false;

    if (!pattern->instanceOf(ERL_LIST))
        return false;

    const ErlList* _t = (ErlList *) pattern;

	// We will compare the elements one by one from 0 to smaller arity.
	// Then compare tails.

	// Get the smaller arity
	unsigned int arity = _t->arity() < this->arity()? 
							_t->arity(): this->arity();

	// Is one is empty list, the other should too
	if (arity == 0) {
		return this->arity() == _t->arity();
	}

	for (unsigned int i = 0; i < arity; i++) {
		ErlTermPtr<> a(this->elementAt(i));
		ErlTermPtr<> b(_t->elementAt(i));
		if (!a->internalMatch(binding, b.get()))
			return false;
	}
	/* Check the tail */
	ErlTermPtr<> a(this->tail(arity-1));
	ErlTermPtr<> b(_t->tail(arity-1));
	// match the tails. Try to match a with b and viceversa since 
	// internalMatch is not commutative
	if (!a->internalMatch(binding, b.get()))
		if (!b->internalMatch(binding, a.get()))
			return false;

	return true;
}

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

#include "ErlConsList.hpp"
#include "EpiBuffer.hpp"

using namespace epi::error;
using namespace epi::type;

// Unitialized list constructor
ErlConsList::ErlConsList():mElementVector(0) {
}

// Unitialized list constructor
ErlConsList::ErlConsList(unsigned int estimated_size):mElementVector(0) {
    mElementVector.reserve(estimated_size);
}

// Array initialized  constructor
ErlConsList::ErlConsList(ErlTerm *elems[], unsigned int arity)
        throw(EpiBadArgument)
{
    init(elems, arity);
}

ErlConsList::ErlConsList(ErlTerm *elem)
        throw(EpiBadArgument)
{
    ErlTerm *elems[1] = {elem};
    init(elems, 1);
}

ErlConsList::ErlConsList(ErlTerm *elem1, ErlTerm *elem2)
        throw(EpiBadArgument)
{
    ErlTerm *elems[2] = {elem1, elem2};
    init(elems, 2);
}

ErlConsList::ErlConsList(ErlTerm *elem1, ErlTerm *elem2, ErlTerm *elem3)
        throw(EpiBadArgument)
{
    ErlTerm *elems[3] = {elem1, elem2, elem3};
    init(elems, 3);

}

ErlConsList::ErlConsList(ErlTerm *elem1, ErlTerm *elem2,
                         ErlTerm *elem3, ErlTerm *elem4)
        throw(EpiBadArgument)
{
    ErlTerm *elems[4] = {elem1, elem2, elem3, elem4};
    init(elems, 4);

}

void ErlConsList::init(ErlTerm *elems[], unsigned int arity) {
    mElementVector.reserve(arity);

    for(unsigned int i=0; i<arity; i++) {
        addElement(elems[i]);
    }
    close();
}

///
ErlConsList* ErlConsList::addElement(ErlTerm* elem)
        throw(EpiBadArgument, EpiAlreadyInitialized)
{
    Dout(dc::erlang|flush_cf,
         "["<<this<<"]" << "ErlConsList:AddElement(" <<
         (elem? elem->toString(): std::string("null"))<<")");

    if (!elem || !elem->isValid()) {
        throw EpiBadArgument("Element is invalid");
    }

    if (isValid()) {
        throw EpiAlreadyInitialized("list is closed");
    }

    mElementVector.push_back(elem);
	
	return this;
}

void ErlConsList::close(ErlTerm *elem)
        throw(EpiBadArgument, EpiAlreadyInitialized)
{
    Dout(dc::erlang,"["<<this<<"]" <<
         "ErlConsList:close(" <<
         (elem? elem->toString(): std::string("null"))<<")");

    if (!elem || !elem->isValid()) {
        throw EpiBadArgument("Element is invalid");
    }
    // If the given element is an list, add all elements.
    if (elem->instanceOf(ERL_CONS_LIST)) {
        ErlConsList *cons = (ErlConsList*) elem;
        for (unsigned int i=0; i<cons->arity(); i++) {
            addElement(cons->elementAt(i));
        }
        close(cons->tail(cons->arity()));
    } else {
        addElement(elem);
        mInitialized = true;
    }
}

ErlTerm* ErlConsList::tail(unsigned int index) const
        throw(EpiInvalidTerm, EpiEmptyList, EpiBadArgument)
{
    if (!isValid()) {
        throw EpiInvalidTerm("List is invalid");
    }
    if (index >= arity()) {
        throw EpiBadArgument("Index grater than arity");
    }
    Dout(dc::erlang, "["<<this<<"]"<< "Getting tail "<< index << " arity=" << arity());
    if (index < arity()-1) {
        // If is not the last tail, return a new ErlConsList
        ErlConsList *newList = new ErlConsList(arity()-index+1);
        for (unsigned int i = index+1; i<mElementVector.size(); i++) {
            newList->mElementVector.push_back(mElementVector[i]);
        }
        newList->mInitialized = mInitialized;
        return newList;
    }
    // else, return a copy of last element.
    else {
        return mElementVector.back().get();
    }
}

ErlTerm* ErlConsList::elementAt(unsigned int index) const
        throw(EpiInvalidTerm, EpiBadArgument, EpiEmptyList)
{
    if (!isValid()) {
        throw EpiInvalidTerm("List is invalid");
    }
    if (index < 0 || index >= arity()) {
        throw EpiBadArgument("Index out of range");
    }

    return mElementVector[index].get();
}

//bool ErlConsList::equals(const ErlTerm &t) const {
//    // Check if the term is the same class
//    if (!t.instanceOf(ERL_CONS_LIST))
//        return false;
//    if (!this->isValid() || !t.isValid())
//        return false;
//
//    const ErlConsList* _t = (ErlConsList *) &t;
//    if (mElementVector != _t->mElementVector) {
//        return false;
//    }
//    return true;
//}
//
//bool ErlConsList::internalMatch(VariableBinding* binding, ErlTerm* pattern)
//        throw (EpiVariableUnbound) {
//
//    if (pattern->instanceOf(ERL_VARIABLE)) {
//        Dout(dc::erlang, "Pattern parameter is a variable, conmute");
//        return pattern->internalMatch(binding, this);
//    }
//
//    if (!this->isValid() || !pattern->isValid())
//        return false;
//
//    if (!pattern->instanceOf(ERL_CONS_LIST))
//        return false;
//
//    Dout_continue(dc::erlang, _continue, " failed.",
//    		"ErlConsList::internalMatch(): "<<
//         toString(binding) << " = "<< pattern->toString(binding) );
//
//    const ErlConsList* _t = (ErlConsList *) pattern;
//
//    /* Match the smaller with the bigger */
//    if (arity() != _t->arity()) {
//        Dout_finish(_continue, "arity is not equal");
//        return false;
//    }
//    /* Now this list is the smaller. Match each element except the tail */
//    for (unsigned int i=0; i<arity(); i++) {
//        Dout_continued( "Matching element " << i << ": " <<
//                mElementVector[i]->toString(binding)<< " = " <<
//                _t->mElementVector[i]->toString(binding));
//        if(!mElementVector[i]->internalMatch(binding, _t->mElementVector[i].get())) {
//            return false;
//        }
//    }
//
//    /* at the last, match the tail */
//    // Use ErlTermPtr to ensure deletion
//    ErlTermPtr<ErlTerm> patternTail = _t->tail(this->arity()-1);
//    Dout_continued( "Matching tails " <<
//            mElementVector.back()->toString(binding) << " = " <<
//            patternTail->toString(binding) << ". ");
//    if (mElementVector.back()->internalMatch(binding, patternTail.get())) {
//        Dout_finish(_continue, "Ok");
//        return true;
//    } else {
//        return false;
//    }
//}

std::string ErlConsList::toString(const VariableBinding* binding) const {

    if (!isValid())
        return "** INVALID LIST **";

    std::ostringstream oss;
    oss << "[";

    for (unsigned int i=0; i<mElementVector.size()-1; i++) {
        if (i>0) {
            oss << ",";
        }
        oss << mElementVector[i]->toString(binding);
    }

    // If the last tail is not an EmptyList, print
    if (!mElementVector.back()->instanceOf(ERL_EMPTY_LIST)) {
        oss << "|" << mElementVector.back()->toString(binding);
    }
    oss << "]";

    return oss.str();
}

ErlVariable* ErlConsList::searchUnbound(const VariableBinding* binding) {
    for (unsigned int i=0; i<mElementVector.size(); i++) {
        ErlVariable *unbound = mElementVector[i]->searchUnbound(binding);
        if (unbound) {
            return unbound;
        }
    }
    return 0;
}

ErlTerm* ErlConsList::subst(const VariableBinding* binding)
        throw (EpiInvalidTerm, EpiVariableUnbound)
{
    Dout_continue(dc::erlang, _continue, " Failed.", 
    		"["<<this<<"]"<< " ErlConsList::subst(): ");
    ErlTermPtr<ErlConsList> newList = new ErlConsList();
    // We check if any contained term changes.
    bool change = false;
    int size = mElementVector.size();
    for (int i=0; i<size; i++) {
        ErlTerm *newElem = mElementVector[i]->subst(binding);
        // check if the pointer is different
        if (newElem == mElementVector[i].get()) {
            change = true;
        }
        if (i!=size-1) {
            newList->addElement(newElem);
        } else {
            newList->close(newElem);
        }
    }

    // If change, return the new tuple
    if (change) {
	    Dout_finish(_continue, "Returning a new list with different content");
        return newList.drop();
    } else {
	    Dout_finish(_continue, "Returning the same list (no substitution)");
        return this;
    }
}


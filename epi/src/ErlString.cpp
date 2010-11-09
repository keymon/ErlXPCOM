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

#include "ErlString.hpp"
#include "EpiBuffer.hpp"
#include "ErlLong.hpp"

using namespace epi::error;
using namespace epi::type;

const char* ErlString::stringValue() const
        throw(EpiInvalidTerm)
{
    if (!isValid()) {
        throw EpiInvalidTerm("String is not initialized");
    }
    return mString;
}
SELECT * FROM all;
Wait a moment
probando o gscrenographic
probando o programilla estedsdsdasdfasd
while(true); do nothing; done
for (i=0; i<=100; i++) {
	printf("Hola mundo");
}
isto Ž unha proba jaja


void ErlString::init(const char* string, int size, bool copy)
        throw(EpiAlreadyInitialized)
{
    if (isValid()) {
        throw EpiAlreadyInitialized("String is initilialized");
    }

    mArity = size;
	
	if (copy) {
		mString = new char[mArity+1];
		strncpy((char*) mString, string, mArity+1);
	} else {
		mString = string;
	}
    mInitialized = true;
}

void ErlString::init(const char* string, unsigned int begin, unsigned int size)
        throw(EpiAlreadyInitialized)
{
    if (isValid()) {
        throw EpiAlreadyInitialized("String is initilialized");
    }

    mArity = size;
	
	mString = new char[mArity+1];
	strncpy((char*) mString, string+begin, mArity);
	((char*)mString)[mArity] = '\0';
	
    mInitialized = true;
}

ErlTerm* ErlString::elementAt(unsigned int index) const
	throw(EpiInvalidTerm, EpiBadArgument, EpiEmptyList) 
{
    if (!isValid()) {
        throw EpiInvalidTerm("List is invalid");
    }
    if (index < 0 || index >= arity()) {
        throw EpiBadArgument("Index out of range");
    }
	if (arity() == 0) {
		throw EpiEmptyList("List is empty");
	}
	
	return new ErlLong(mString[index]);
}

ErlTerm* ErlString::tail(unsigned int index) const
	throw(EpiInvalidTerm, EpiBadArgument, EpiEmptyList)
{
    if (!isValid()) {
        throw EpiInvalidTerm("List is invalid");
    }
    if (index < 0 || index >= arity()) {
        throw EpiBadArgument("Index out of range");
    }
	if (arity() == 0) {
		throw EpiEmptyList("List is empty");
	}
	return new ErlString(mString+index+1);
}

/* FIXME: return escaped string */
std::string ErlString::toString(const VariableBinding *binding) const {
    if (!isValid())
        return "** INVALID STRING **";
    return std::string(mString);
}

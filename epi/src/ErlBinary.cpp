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

 // Main config file

#include <iostream>
#include <sstream>
#include <memory>
#include <string.h>

#include "ErlBinary.hpp"
#include "EpiBuffer.hpp"

using namespace epi::error;
using namespace epi::type;

void ErlBinary::init(const void *data, const unsigned int size,
                const bool copy, const bool del)
        throw(EpiAlreadyInitialized)
{

    if (isValid()) {
        throw EpiAlreadyInitialized("Binary is initilialized");
    }

    // Copy the data if necesary
    mSize=size;
    char *tmpData;
    if (copy) {
        tmpData = new char[size];
        memcpy(tmpData, data, size);
    } else {
        tmpData = (char*) data;
    }

    mData = tmpData;
    mDelete = del;

    mInitialized = true;

}

bool ErlBinary::equals(const ErlTerm &t) const {
    if (!t.instanceOf(ERL_BINARY))
        return false;

    if (!this->isValid() || !t.isValid())
        return false;


    ErlBinary *_t = (ErlBinary*) &t;
    return this->size() == _t->size() &&
            memcmp(this->binaryData(), _t->binaryData(), this->size())==0;
}

std::string ErlBinary::toString(const VariableBinding *binding) const {
    if (!isValid())
        return "** INVALID BINARY **";

    std::ostringstream oss;
    oss << "#Bin<";
    const char *data = (const char *) this->binaryData();
    unsigned int size = this->size();
    for(unsigned int i=0; i<size; i++) {
        int v = data[i];
        oss << v;
        if (i < size-1) {
            oss << ',';
        }
    }
    oss << ">";
    return oss.str();
}

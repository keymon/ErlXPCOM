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

#include "ErlLong.hpp"
#include "EpiBuffer.hpp"

using namespace epi::error;
using namespace epi::type;

void ErlLong::init(const long long value)
        throw(EpiAlreadyInitialized)
{
    if (isValid()) {
        throw EpiAlreadyInitialized("Long is initilialized");
    }

    mValue = value;
    mInitialized = true;
}

long long ErlLong::longValue() const
        throw(EpiInvalidTerm)
{
    if (!isValid()) {
        throw EpiInvalidTerm("Double is not initialized");
    }
    return mValue;
}



bool ErlLong::equals(const ErlTerm &t) const {
    if (!t.instanceOf(ERL_LONG))
        return false;

    if (!this->isValid() || !t.isValid())
        return false;

    ErlLong *_t = (ErlLong*) &t;
    return mValue == _t->mValue;
}

std::string ErlLong::toString(const VariableBinding *binding) const {
    if (!isValid())
        return "** INVALID LONG **";
    std::ostringstream oss;
    oss << mValue;
    return oss.str();
}

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

#include "ErlPort.hpp"
#include "EpiBuffer.hpp"

using namespace epi::error;
using namespace epi::type;

void ErlPort::init(const std::string node, const int id,
                   const int creation)
       throw(EpiBadArgument, EpiAlreadyInitialized)
{
    if (isValid()) {
        throw EpiAlreadyInitialized("Port is initilialized");
    }

    if (node.length() > MAX_NODE_LENGTH) {
        std::ostringstream oss;
        oss << "Node name must not exceed " <<
                MAX_NODE_LENGTH << " characters";
        throw EpiBadArgument(oss.str());
    } else if (node.length() == 0) {
        throw EpiBadArgument("nodename must be non-empty");
    }

    mNode = node;
    mId = id & 0x3ffff;
    mCreation = creation & 0x03;
    mInitialized = true;

}

bool ErlPort::equals(const ErlTerm &t) const {
    if (!t.instanceOf(ERL_PORT))
        return false;

    if (!this->isValid() || !t.isValid())
        return false;

    ErlPort *_t = (ErlPort *) &t;

    return (mNode == _t->mNode) &&
            (mId == _t->mId);
}

std::string ErlPort::toString(const VariableBinding *binding) const {
    if (!isValid())
        return "** INVALID PORT **";

    std::ostringstream oss;
    oss << "#Port<" << mNode << "."
            << mId << "."
            << mCreation << ">";
    return oss.str();
}


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

#include "ErlRef.hpp"
#include "EpiBuffer.hpp"

using namespace epi::error;
using namespace epi::type;

void ErlRef::init(const std::string node, const unsigned int ids[],
                  const unsigned  int creation, const bool newStyle)
        throw(EpiBadArgument, EpiAlreadyInitialized)
{
    if (isValid()) {
        throw EpiAlreadyInitialized("Ref is initilialized");
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
    mNewStyle = newStyle;

    mIds[0] = ids[0] & 0x3ffff;
    if (mNewStyle) {
        mIds[1] = ids[1];
        mIds[2] = ids[2];
    }
    mCreation = creation & 0x03;
    mInitialized = true;
}

bool ErlRef::equals(const ErlTerm &t) const {
    if (!t.instanceOf(ERL_REF))
        return false;

    if (!this->isValid() || !t.isValid())
        return false;

    ErlRef *_t = (ErlRef *) &t;

    if (_t->mNewStyle != mNewStyle)
        return false;

    return (mNode == _t->mNode) &&
            (mIds[0] == _t->mIds[0]) &&
            (   !mNewStyle ||
                ((mIds[1] == _t->mIds[1]) &&
                 (mIds[2] == _t->mIds[2]))
                );
}

std::string ErlRef::toString(const VariableBinding *binding) const {
    if (!isValid())
        return "** INVALID REF **";

    std::ostringstream oss;
    if (mNewStyle) {
        oss << "#Ref<" << mNode << "."
                << mIds[0] << "."
                << mIds[1] << "."
                << mIds[2] << "."
                << mCreation << ">";
    } else {
        oss << "#Ref<" << mNode << "."
                << mIds[0] << "."
                << mCreation << ">";
    }

    return oss.str();
}

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

#include "ErlPid.hpp"
#include "EpiBuffer.hpp"

using namespace epi::error;
using namespace epi::type;


void ErlPid::init(std::string node, int id, int serial, int creation)
        throw(EpiBadArgument, EpiAlreadyInitialized)
{
    if (isValid()) {
        throw EpiAlreadyInitialized("Pid is initilialized");
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
    mId = id & 0x3ffff; // 15bits
    mSerial = serial & 0x1fff ;  // 13 bits
    mCreation = creation & 0x03; // 2 bits
    mInitialized = true;

}


bool ErlPid::equals(const ErlTerm &t) const {
    if (!t.instanceOf(ERL_PID))
        return false;

    if (!this->isValid() || !t.isValid())
        return false;

    ErlPid *_t = (ErlPid *) &t;

    return (mNode == _t->mNode) &&
           (mId == _t->mId) &&
           (mSerial == _t->mSerial);
}

std::string ErlPid::toString(const VariableBinding *binding) const {
    if (!isValid())
        return "** INVALID PID **";

    std::ostringstream oss;
    oss << "#Pid<" << mNode << "." << mId << "." << mSerial << ">";
    return oss.str();
}

bool epi::type::operator<(const ErlPid &t1, const ErlPid &t2) {

	if (t1.node() < t2.node()) {
		return true;
	} 
	if (t1.node() > t2.node()) {
		return false;
	}
	if (t1.id() < t2.id()) {
		return true;
	} 
	if (t1.id() < t2.id()) {
		return false;
	} 
	if (t1.serial() < t2.serial()) {
		return true;
	} 
	if (t1.serial() > t2.serial()) {
		return false;
	} 
	if (t1.creation() < t2.creation()) {
		return true;
	} 
	if (t1.creation() > t2.creation()) {
		return false;
	} 
	return false;
}


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

#include "EpiNode.hpp"
#include "EpiBuffer.hpp"
#include "EpiConnection.hpp"
#include "EpiUtil.hpp"

using namespace epi::error;
using namespace epi::type;
using namespace epi::node;
using namespace epi::util;


bool epi::node::isSameHost(const std::string node1,
                           const std::string node2,
                           const std::string host)
{
    std::string fullNodeName1;
    std::string fullNodeName2;

    std::string::size_type pos;
    pos = node1.find ("@",0);

    if (pos == std::string::npos) {
        fullNodeName1 = node1;
    } else {
        fullNodeName1 = node1+"@"+host;
    }

    pos = node2.find ("@",0);

    if (pos == std::string::npos) {
        fullNodeName2 = node2;
    } else {
        fullNodeName2 = node2+"@"+host;
    }

    return fullNodeName1 == fullNodeName2;

}

//////////////////////////////////////////////////////////////////////////
// AbstractNode
AbstractNode::AbstractNode()
{
}

AbstractNode::AbstractNode(const std::string aNodeName,
                           const std::string aCookie)
        throw (EpiBadArgument):
        mCookie(aCookie)
{
    initNodeName(aNodeName);
}

void AbstractNode::initNodeName(const std::string aNodeName)
        throw (EpiBadArgument)
{

    std::string::size_type pos = aNodeName.find ("@",0);

    if (pos == std::string::npos) {
        mAliveName = aNodeName;
        mHostName = smLocalhost;
    } else {
        mAliveName = aNodeName.substr(0, pos);
        mHostName = aNodeName.substr(pos+1, aNodeName.size());
    }

    mNodeName = mAliveName+'@'+mHostName;

    if (mNodeName.length() > MAX_NODE_LENGTH) {
        throw EpiBadArgument("Node name too big");
    }
}

// FIXME: get the cookie from ~/.cookie
std::string AbstractNode::smDefaultCookie = "no-cookie";
// FIXME: get localhost name from a property
std::string AbstractNode::smLocalhost = "localhost";


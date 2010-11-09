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


#include "EITransport.hpp"
#include "ErlangTransportManager.hpp"

using namespace epi::node;
using namespace epi::error;

const char* ErlangTransportManager::defaultProtocol = "ei";
ErlangTransportManager *ErlangTransportManager::mErlangTransportManagerInstance = 0;

ErlangTransportManager::ErlangTransportManager() {
    mFactoryMap["ei"] = new epi::ei::EITransportFactory();
}


void ErlangTransportManager::registerProtocol(const std::string protocol,
                                              ErlangTransportFactory *factory)
{
    if (instance().mFactoryMap.count(protocol) != 0) {
        delete instance().mFactoryMap[protocol];
    }
    instance().mFactoryMap[protocol] = factory;
}

ErlangTransport *ErlangTransportManager::
        createErlangTransport(std::string nodeid, std::string cookie)
        throw (EpiUnknownProtocol, EpiException)
{

    std::string protocol;
    std::string nodename;

    // Find first the protocol
    std::string::size_type pos = nodeid.find (":",0);

    if (pos == std::string::npos) {
        protocol = ErlangTransportManager::defaultProtocol;
        nodename = nodeid;
    } else {
        protocol = nodeid.substr(0, pos);
        nodename = nodeid.substr(pos+1, nodeid.size());
    }

    if (instance().mFactoryMap.count(protocol) == 0) {
        throw EpiUnknownProtocol(protocol);
    }
    ErlangTransportFactory *factory =
            instance().mFactoryMap[protocol];

    if (!factory) {
        throw EpiUnknownProtocol(protocol);
    }

    return factory->createErlangTransport(nodename, cookie);

}



ErlangTransportManager &ErlangTransportManager::instance() {
    if (!mErlangTransportManagerInstance) {
        mErlangTransportManagerInstance = new ErlangTransportManager();
    }
    return *mErlangTransportManagerInstance;
}

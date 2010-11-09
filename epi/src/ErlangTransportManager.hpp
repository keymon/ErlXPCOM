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
#ifndef __ERLANGTRANSPORTMANAGER_HPP
#define __ERLANGTRANSPORTMANAGER_HPP

#include <string>
#include <map>

#include "ErlangTransport.hpp"
#include "ErlangTransportFactory.hpp"

namespace epi {
namespace node {

using namespace epi::error;

/**
 * With ErlangTransportManager you can get an instance of an
 * ErlangInterface using an identifier with the following sintax:
 *
 * "protocol:nodename@hostname:port"
 *
 * This class will extract the "protocol" part and search
 * the ErlangTransportFactory associated to it. It will call
 * the ErlangTransportFactory::createErlangTransport method
 * with the "nodename@hostname:port" part.
 *
 * This class is a singleton.
 */
class ErlangTransportManager {
    typedef std::map<std::string, ErlangTransportFactory*> factorymap;
public:

    /**
     * Default protocol: EI
     */
    static const char* defaultProtocol;

    /**
     * Register a factory to a protocol id.
     * @param protocol a string to identify the protocol
     * @param factory the factory to use. Owership is transfered. It will not
     * be deleted util the program exits.
     */
    static void registerProtocol(const std::string protocol,
                                 ErlangTransportFactory *factory);

    /**
     * Return an instance of the new erlang transport. It will search
     * the appropiate factory and call the createErlangTransport method
     * @param nodeid string with the node name with the protocol part:
     *  "protocol:nodename@hostname:port"
     * @param cookie cookie to use
     * @throw EpiUnknowProtocol if the protocol is unkown.
     * @throw EpiException if there is an error.
     */
    static ErlangTransport *
            createErlangTransport(std::string nodeid, std::string aCookie)
            throw (EpiUnknownProtocol, EpiException);


private:
    static ErlangTransportManager *mErlangTransportManagerInstance;

    factorymap mFactoryMap;

    /**
     * get singleton instance
     */
    static ErlangTransportManager &instance();

    ErlangTransportManager();
};

}
}

#endif //__ERLANGTRANSPORTMANAGER_HPP

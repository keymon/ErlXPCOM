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

#ifndef __ERLANGTRANSPORTFACTORY_HPP
#define __ERLANGTRANSPORTFACTORY_HPP

#include "EpiException.hpp"
#include "ErlangTransport.hpp"

namespace epi {
namespace node {

using namespace epi::error;

/**
 * Implement this class to provide a factory of an erlang transport
 */
class ErlangTransportFactory {
public:
    /**
     * Return an instance of the new erlang transport
     * @param nodename string with the node name without the protocol part
     * @param cookie Cookie to use
     * @throw EpiException if there is an error.
     */
    virtual ErlangTransport *createErlangTransport(std::string nodename, std::string aCookie)
            throw (EpiException) = 0;
    inline virtual ~ErlangTransportFactory() {}
};

}
}

#endif //__ERLANGTRANSPORTFACTORY_HPP

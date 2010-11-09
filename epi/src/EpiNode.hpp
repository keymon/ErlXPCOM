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

#ifndef _EPINODE_H
#define _EPINODE_H

#include <string>

#include "ErlTypes.hpp"
#include "EpiError.hpp"
#include "Socket.hpp"

namespace epi {
namespace node {

using namespace epi::error;
using namespace epi::type;

class Connection;

/**
 * Check if two node names are equal.
 * It will compare the node names with or without the hostname.
 *
 * @param node1 first hostname to compare
 * @param node2 second hostname to compare
 * @param host host name to use for alive
 */
bool isSameHost(const std::string node1,
                const std::string node2,
                const std::string host);

/**
 * Contains information of a node: Host name, alive name,
 * node name (host+alive) and cookie.
 */
class AbstractNode {
public:

    AbstractNode();
    AbstractNode(const std::string aHost, const std::string aCookie)
            throw (EpiBadArgument);

    /**
     * Get node name
     */
    inline std::string getNodeName() const {
        return mNodeName;
    }

    /**
     * Get alive name
    */
    inline std::string getAliveName() const {
        return mAliveName;
    }

    /**
     * get host name
     */
    inline std::string getHostName() const {
        return mHostName;
    }

    /**
     * get cookie
     */
    inline std::string getCookie() const {
        return mCookie;
    }

    /**
     * Set the cookie
     */
    inline void setCookie(std::string cookie) {
        mCookie = cookie;
    }


private:
protected:
    std::string mNodeName;
    std::string mAliveName;
    std::string mHostName;
    std::string mCookie;

    /**
     * Init the nodename setting the alive and host name
     */
    void initNodeName(const std::string aNodeName)
            throw (EpiBadArgument);

    static std::string smDefaultCookie; // Default cookie
    static std::string smLocalhost;     // localhost name

};

/**
 * Information of Remote node
 */
class PeerNode: public AbstractNode {
public:
    // Actually the cookie is the same for all peers.
    // ei_interface have no way to use different cookies with diferent
    // connections :(
    inline PeerNode(std::string aNodeName): AbstractNode(aNodeName, "") {}
}
;


} // node
} // epi


#endif

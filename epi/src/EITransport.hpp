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

#ifndef _EITRANSPORT_HPP
#define _EITRANSPORT_HPP

#include <ei.h>

#include "EpiConnection.hpp"
#include "ErlangTransport.hpp"
#include "ErlangTransportFactory.hpp"
#include "EpiBuffer.hpp"

namespace epi {
namespace ei {

using namespace epi::error;
using namespace epi::node;

class EITransport;

/**
 * Factory for EITransport
 */
class EITransportFactory:  public ErlangTransportFactory {
public:
    virtual ErlangTransport *
            createErlangTransport(std::string nodename, std::string aCookie)
            throw (EpiException);
    inline virtual ~EITransportFactory() {}
};



/**
 * ErlangTransport that uses the EI library for comunication
 * and encoding.
 */
class EITransport: public ErlangTransport {
public:

    /**
     * Create a new EI transport
     * @param aNodeName node name
     * @param aAliveName alive name for this node
     * @param aHostName host name for this node
     * @param aCookie cookie to use
     * @param aPort TCP port to use
     * @throws EpiBadArgument if node name is too long
     * @throws EpiConnectionException if there is a network problem
     */
    EITransport(const std::string aNodeName,
                const std::string aAliveName,
                const std::string aHostName,
                const std::string aCookie,
                const int aPort = 0
               )
            throw (EpiBadArgument, EpiConnectionException);

    virtual ~EITransport();

    virtual Connection* connect(const std::string node)
            throw(EpiConnectionException);

    virtual Connection* connect(const std::string node, const std::string cookie)
            throw(EpiConnectionException);

    virtual Connection* accept(long timeout = 0)
            throw(EpiConnectionException);

    virtual Connection* accept(const std::string cookie, long timeout = 0)
            throw(EpiConnectionException);

    virtual std::string getNodeName();

    /**
     * Publish the node port in the local name server epmd.
     * Set the socket to listen.
     */
    virtual void publishPort() throw (EpiConnectionException);

    /**
     * Unpublish the node port in the local name server epmd.
     * Useful, for example, when epmd has not detected the failure of a node
     */
    virtual void unPublishPort() throw (EpiConnectionException);


protected:

    std::string mNodeName;
    std::string mAliveName;
    std::string mHostName;
    std::string mCookie;

    /** Ei library C-Node */
    ei_cnode *ec;

    int mPort;

    // Init the node
    void init(int aPort)
            throw (EpiConnectionException);

    /*
     * This flag indicates if the node is listening for incoming connections
     */
    bool mListening;

    // Check if the node is listening for incoming connections
    inline bool isListening() const
    {
        return mListening;
    }

    // Set the node to listen the socket
    void setListening()
            throw(EpiNetworkException);

    Socket mSocket;

    Connection* do_connect(const ei_cnode *other_ec, const std::string node)
            throw(EpiConnectionException);
    Connection* do_accept(ei_cnode *other_ec, long timeout = 0)
            throw(EpiConnectionException);


};



} // node
} // ei


#endif

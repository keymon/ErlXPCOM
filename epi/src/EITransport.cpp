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



#include <string>
#include <sstream>

#include "EITransport.hpp"
#include "EIConnection.hpp"
#include "EpiUtil.hpp"

using namespace epi::node;
using namespace epi::error;
using namespace epi::type;
using namespace epi::ei;

ErlangTransport *
        EITransportFactory::createErlangTransport(std::string nodename, std::string aCookie)
        throw (EpiException)
{

    std::string alivename;
    std::string subnodename;
    std::string hostname;
    int port;

    std::string::size_type pos = nodename.find (":",0);

    if (pos == std::string::npos) {
        port = 0;
        subnodename = nodename;
    } else {
        subnodename = nodename.substr(0, pos);
        std::istringstream stream(nodename.substr(pos+1, nodename.size()));
          stream >> port;
          if (port < 1024 || port > 65535) {
            port = 0;
          }
    }

    pos = subnodename.find ("@",0);

    if (pos == std::string::npos) {
        alivename = subnodename;
        hostname = "defaulthost";
    } else {
        alivename = subnodename.substr(0, pos);
        hostname = subnodename.substr(pos+1, subnodename.size());
    }

    return new epi::ei::EITransport(alivename+"@"+hostname, alivename,
                                    hostname, aCookie, port);

}


EITransport::EITransport( const std::string aNodeName,
                const std::string aAliveName,
                const std::string aHostName,
                const std::string aCookie,
                const int aPort)
    throw( EpiBadArgument, EpiConnectionException ):
                mNodeName(aNodeName), mAliveName(aAliveName),
                mHostName(aHostName), mCookie(aCookie)
{
    init(aPort);
}

void EITransport::init(int aPort)
        throw (EpiConnectionException)
{
    // Open and listen that port
    Dout_continue(dc::connect, _continue, " failed.",
                  "Opening listen port: ");

    mSocket.create();
    mSocket.bind(aPort);
    mListening = false;

    if (!mSocket.is_valid()) {
        // FIXME, more explicit error
        Dout_finish(_continue, " Unable to open port");
        throw EpiNetworkException("Unable to open port");
    }

    mPort = mSocket.getLocalPort();
    Dout_continued( mPort << ".");

     // Init ei library
    ec = new ei_cnode;

    // I use ei_connect_xinit, ei_connect_init does not work to me :-/
    Dout_continued( " Init EI:");
    int ei_res =
            ei_connect_xinit(ec,
                             mHostName.c_str(),
                             mAliveName.c_str(),
                             mNodeName.c_str(),
                             // FIXME: In ei_interface ¡¡ipaddr is NOT USED!!
                             // DIOXXXXXXXXXXX!!!! }:-/
                             NULL,
                             mCookie.c_str(),
                             // FIXME: Please! how does creation work!?!?!
                             0);

    if (ei_res < 0) {
        // FIXME, more explicit error
        Dout_finish(_continue, " Error init EI-library for node "<< mNodeName);
        throw EpiEIException("Error init EI-library");
    }

    Dout_finish(_continue, ".");
}

EITransport::~ EITransport( )
{
}

Connection * EITransport::connect( const std::string node )
        throw( EpiConnectionException )
{
    // Use default cookie
    return do_connect(ec, node);
}

Connection * EITransport::connect( const std::string node,
                                   const std::string cookie )
        throw( EpiConnectionException )
{
    // Change cookie
    ei_cnode *new_ec = epi::util::EiCNodeChangeCookie(ec, cookie);
    Connection *connection= do_connect(ec, node);
    delete new_ec;
    return connection;
}

Connection * EITransport::accept( long timeout )
        throw( EpiConnectionException )
{
    // Use default cookie
    return do_accept(ec, timeout);
}

Connection * EITransport::accept( const std::string cookie, long timeout )
        throw( EpiConnectionException )
{
    // Change cookie
    std::auto_ptr<ei_cnode> new_ec(
    		epi::util::EiCNodeChangeCookie(ec, cookie));
    
    Connection *connection = do_accept(new_ec.get(), timeout);
    return connection;
}

/*
 * Perform connection using this ei_cnode
 */
Connection* EITransport::do_connect(const ei_cnode *other_ec, const std::string node)
        throw(EpiConnectionException)
{
    Dout_continue(dc::connect, _continue, " failed.",
                  "EITransport::do_connect(" << node << "): ");

    int newSock = ei_connect((ei_cnode *) other_ec, (char *) node.c_str());

    if (newSock<0) {
        switch (erl_errno) {
            case EHOSTUNREACH:
                Dout_finish(_continue, " Failed: Unreachable.");
                throw EpiNetworkException("Can not connect: host is unreachable", erl_errno);
                break;
            case EIO:
                Dout_finish(_continue, " Failed: Network error.");
                throw EpiNetworkException("Can not connect: network error", erl_errno);
                break;
            case ECONNREFUSED:
                Dout_finish(_continue, " Failed: Connection refused.");
                throw EpiNetworkException("Can not connect: no body in other side", erl_errno);
                break;
            default:
                Dout_finish(_continue, " Failed: Cannot connect.");
                throw EpiNetworkException("Can not connect", erl_errno);
                break;
        }
    }


    // Create the connection
    Connection *connection = new EIConnection(new PeerNode(node),
                                            other_ec->ei_connect_cookie,
                                            new Socket(newSock));

    Dout_finish(_continue, "connected.");

    return connection;
}


Connection* EITransport::do_accept(ei_cnode *other_ec, long timeout)
        throw(EpiConnectionException)
{

    ErlConnect erlConnect;

    Dout_continue(dc::connect, _continue, " failed.",
                  "EITransport::accept(): ");

    setListening();

	int newSock;
	
	newSock = ei_accept_tmo(other_ec, 
	                    mSocket.getSystemSocket(), 
					   &erlConnect, 
					   timeout);

    if (newSock < 0) {
    		// EI does not set erl_errno = ETIMEDOUT if there is a timeout,
    		// it sets erl_errno to 0 :-?
    		if (erl_errno == ETIMEDOUT || erl_errno == 0) {
			Dout_finish(_continue, " timeout.");
    			return 0;
    		}
        // FIXME, return more explicit error
        throw EpiEIException("Error accepting connection", erl_errno);
    }

    // Create the connection
    Connection *connection = new EIConnection(new PeerNode(erlConnect.nodename),
                                            other_ec->ei_connect_cookie,
                                            new Socket(newSock));

    Dout_finish(_continue, "accepted for " << erlConnect.nodename);

    return connection;
}

void EITransport::publishPort()
        throw (EpiConnectionException)
{

    int ei_res;
    Dout_continue(dc::connect, _continue, " failed.",
                  "Publishing port "<< mPort << ": ");

    setListening();
    ei_res = ei_publish(ec, mPort);
    if (ei_res < 0) {
        throw EpiEIException("Error publishing port", erl_errno);
    }

    Dout_finish(_continue, " ok.");

}

void EITransport::unPublishPort()
        throw (EpiConnectionException)
{
    int ei_res;

    Dout_continue(dc::connect, _continue, " failed.",
                  "UnPublishing port:");

    ei_res = ei_unpublish(ec);

    if (ei_res < 0) {
        throw EpiEIException("Error unpublishing port", erl_errno);
    }

    Dout_finish(_continue, " ok.");
}

void EITransport::setListening()
        throw(EpiNetworkException)
{
    // Set the socket to listen
    if (!isListening()) {
        Dout_continue(dc::connect, _continue, " failed.", 
        		"Setting socket to listen: ");
        if (!mSocket.listen()) {
            throw EpiNetworkException("Error opening port for listen");
        }

        mListening = true;
        Dout_finish(_continue, "ok");
    }
}

std::string EITransport::getNodeName() {
    return mNodeName;
}

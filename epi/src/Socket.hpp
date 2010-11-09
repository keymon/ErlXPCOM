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

/*
 Based on code from Rob Tougher.

 Copyright © 2002, Rob Tougher.
 Copying license http://www.linuxgazette.com/copying.html
 Published in Issue 74 of Linux Gazette, January 2002
 - http://www.linuxgazette.com/issue74/tougher.html

 */

#ifndef Socket_class
#define Socket_class


#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <unistd.h>
#include <arpa/inet.h>

#include <string>


const int MAXHOSTNAME = 200;
const int MAXCONNECTIONS = 5;
const int MAXRECV = 500;

class Socket
{
public:
	 Socket();
     Socket(int aSock);
     virtual ~Socket();

	 // Server initialization
	 bool create();
	 bool bind ( const int port );
	 bool listen() const;
	 bool accept ( Socket& ) const;

	 // Client initialization
	 bool connect ( const std::string host, const int port );

	 // Data Transimission
	 bool send ( const std::string ) const;
	 int recv ( std::string& ) const;


     void close ();

	 void set_non_blocking ( const bool );

	 inline bool is_valid() const { return m_sock != -1; }

	 inline int getLocalPort() const { return ntohs(m_addr.sin_port); }

	 inline int getSystemSocket() const { return m_sock; }

private:

	 int m_sock;
	 sockaddr_in m_addr;


};


#endif

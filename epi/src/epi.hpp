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

#ifndef _EPI_HPP
#define _EPI_HPP

/**
 * @mainpage
 * Erlang Plus Interface Library (EPI)
 * (C) 2005 Héctor Rivas Gándara <keymon@gmail.com>
 *
 * @version 0.0.1 2005-02-25 Initial
 *
 * The Erlang Plus Interface library (EPI) is a tool set of C++ classes to
 * easily build applications in C++ what comunicates with Erlang.
 *
 * The intention of the library is to cover the holes in EI library offering:
 *
 *  - An object oriented implementation
 *  - A simple API
 *  - Extensibility
 *  - Abstraction of comunication mechanism
 *  - Simplified management of incoming messages (mailboxes)
 *
 * EPI is realased under the LGPL license. For details please see the file
 * "COPYING" distributed with EPI.
 *
 * EPI works in a similar manner than jinterface the library for java
 * http://www.erlang.se/doc/doc-5.0.1/lib/jinterface-1.2/doc/.
 *
 * It have some differences:
 *
 *  - All comunication is done throw MailBoxes, no matter if you use
 *      self or auto managed nodes.
 *  - Epi abstracts the comunication mechanism, and you can extend it with new
 *       mechanism (using ErlangTransport interface and ErlangTransportFactory)
 *
 * NOTE: EPI is a alfa version and is probably full of bugs.
 *
 * Example of use:
 *
 * @code
 * 
 *
 * #include <iostream>
 * #include <memory>
 * #include <string>
 *
 * #include "epi.hpp"
 *
 * // Use namespaces
 * using namespace epi::type;
 * using namespace epi::error;
 * using namespace epi::node;
 *
 * int main () {
 * const std::string LOCALNODE = "pepito@KeySys.Ceibe.org";
 * const std::string REMOTENODE = "pepita@KeySys.Ceibe.org";
 * const std::string COOKIE = "one_cookie";
 *
 *    try {
 *         // Create the node
 *         AutoNode node(LOCALNODE, COOKIE);
 *
 *         // Get a mailbox. The node has the pointer owership!!!
 *         MailBox *mailbox = node.createMailBox();
 *
 *		   // Create the tuple {self(), hello}
 *         ErlTermPtr<ErlTuple> tuple(new ErlTuple(2));
 *	       tuple->initElement(mailbox->self())-0>initElement(new ErlAtom("hello"));
 *
 *         // Send the term to a server in the remote node
 *         mailbox->send(REMOTENODE, "reply_server", tuple.get());
 *
 *         // Receive the response
 *         ErlTermPtr<> received(mailbox->receive());
 *
 *         // Print it
 *         std::cout << "Received response: " <<
 *                 received->toString() << std::endl;
 *    } catch (EpiException &e) {
 *         std::cout << "Exception catched: " << e.getMessage() << std::endl;
 *         return 1;
 *    }
 *    return 0;
 * }
 * @endcode
 *
 * The corresponding erlang node:
 *
 * @code
 * -module(reply_server).
 * -export([start/0, loop/0]).
 *
 * start() ->
 *     Pid=spawn(reply_server, loop, []),
 *     register(reply_server, Pid),
 *     Pid.
 *
 * loop() ->
 *     receive
 *     {Pid, Msg} ->
 *        io:format("Received: ~w from ~w~n", [Msg, Pid]),
 *        Pid!Msg;
 *     X ->
 *         io:format("Received: ~w~n", [X])
 *     end,
 *     loop().
 * @endcode
 *
 * Bird's eye view:
 *
 *  - To manage connections automaticlyuse epi::node::AutoNode,
 *    use epi::node::LocalNode otherwise
 *  - You can explore the mailbox queue content using the
 *    interface QueueGuard from the GenericQueue.hpp file
 *    and the associated method in MailBox class.
 *    Some useful guards are provided:
 *      - PatternMatchingGuard: It searchs a message that matches
 *          a pattern (an ErlTerm with variables).
 *      - CommandQueueGuard: Uses the Command+Decorator patterns
 *          to attach an command to be executed when the guard matches.
 *      - MatchingCommandGuard: Like the CommandQueueGuard, but
 *          with pattern matching
 *      - ComposedGuard: Uses the composite pattern to compose guards
 *  - Is highly recomended the use of std::auto_ptr and
 *      epi::type::ErlTermPtr.
 *
 */

#ifndef CWDEBUG
#include "nodebug.h"
#endif

#include "ErlTypes.hpp"
#include "EpiException.hpp"
#include "EpiConnection.hpp"
#include "EpiBuffer.hpp"
#include "EpiNode.hpp"
#include "EpiLocalNode.hpp"
#include "EpiAutoNode.hpp"
#include "EpiBuffer.hpp"
#include "EpiInputBuffer.hpp"
#include "EpiOutputBuffer.hpp"
#include "EpiUtil.hpp"
#include "EpiMailBox.hpp"
#include "EpiMessage.hpp"
#include "PatternMatchingGuard.hpp"
#include "MatchingCommandGuard.hpp"
#include "MatchingCommand.hpp"
#include "ComposedGuard.hpp"

#endif // _EPI_HPP


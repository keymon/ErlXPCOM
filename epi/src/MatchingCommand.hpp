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

#ifndef __MATCHINGCOMMAND_HPP
#define __MATCHINGCOMMAND_HPP

#include "ErlTypes.hpp"

namespace epi {
namespace node {

using namespace epi::type;

/**
 * Interface to implement commands to be executed with MatchingCommandGuard.
 * The user must implement the method execute and de destructor.
 * The destructor must be virtual.
 */
class MatchingCommand {
public:
    /**
     * FIXME: Exceptions does not cross QueueGuard class
     * Execute the command.
     * @param term Term from the incoming message
     * @param binding VariableBinding with the bound variables in the pattern
     * matching
     */
    virtual void execute(ErlTerm* term, VariableBinding *binding)
            throw (EpiException) = 0;
    virtual inline ~MatchingCommand() {}
};

} // namespace node
} // namespace epi

#endif // __MATCHINGCOMMAND_HPP

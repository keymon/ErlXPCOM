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

#ifndef __MATCHINGCOMMANDGUARD_HPP
#define __MATCHINGCOMMANDGUARD_HPP

#include <memory>

#include "GenericQueue.hpp"
#include "EpiMailBox.hpp"
#include "EpiMessage.hpp"
#include "MatchingCommand.hpp"
#include "ErlTypes.hpp"

namespace epi {
namespace node {

using namespace epi::type;

/**
 * Pattern Matching guard with command asociated. This guard is
 * equivalent to PatternMatchingGuard but has a MatchingCommand
 * associated that will be called when a message matches the
 * given pattern.
 */
class MatchingCommandGuard: public MailBoxGuard {
public:

    /**
     * Create a new MatchingCommandGuard.
     * @param pattern pattern to match
     * @param command Command to execute. Owership is transfered!!!
     */
    inline MatchingCommandGuard(ErlTerm *pattern, MatchingCommand *command):
        mPattern(pattern), mCommand(command)
    {}

    virtual bool match(ErlangMessage* msg) throw (EpiException);

    virtual inline ~MatchingCommandGuard() {}

private:
    ErlTermPtr<ErlTerm> mPattern;
    std::auto_ptr<MatchingCommand> mCommand;

};

} // namespace node
} // namespace epi

#endif // __MATCHINGCOMMAND_HPP

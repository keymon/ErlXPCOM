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

#ifndef __COMPOSEDGUARD_HPP
#define __COMPOSEDGUARD_HPP

#include <memory>
#include <list>

#include "GenericQueue.hpp"
#include "EpiMailBox.hpp"
#include "EpiMessage.hpp"
#include "MatchingCommand.hpp"
#include "ErlTypes.hpp"

namespace epi {
namespace node {

using namespace epi::type;

/**
 * Composed MailBoxGuard. This class allows define a secuence of
 * MailBoxGuards to be checked with the messages of a mailbox.
 */
class ComposedGuard: public MailBoxGuard {
    typedef std::list<MailBoxGuard*> guardlist;
public:

    inline ComposedGuard(): mGuardList() {}

    /**
     * Add a guard to this composed guard. Owership is not transfered.
     */
    void addGuard(MailBoxGuard *guard);

    virtual bool match(ErlangMessage* msg) throw (EpiException);

    virtual ~ComposedGuard();

private:
    guardlist mGuardList;

};

}
}

#endif //__COMPOSEDGUARD_HPP

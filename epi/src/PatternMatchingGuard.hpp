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

#ifndef __PATTERNMATCHINGGUARD_HPP
#define __PATTERNMATCHINGGUARD_HPP

#include "EpiMailBox.hpp"
#include "EpiMessage.hpp"
#include "ErlTypes.hpp"

namespace epi {
namespace node {

using namespace epi::type;

/**
 * Pattern Matching guard. Use this guard to search a message
 * containing a term that matches a given pattern (a term with variables).
 * You can provide a binding.
 */
class PatternMatchingGuard: public MailBoxGuard {
public:

    inline PatternMatchingGuard(ErlTerm *pattern, VariableBinding *binding = 0):
        mPattern(pattern), mBinding(binding) {
    }

    /**
     * callback with the pattern matching of the guard.
     */
    virtual bool match(ErlangMessage* msg) throw (EpiException);

    virtual inline ~PatternMatchingGuard() {}
private:
    ErlTermPtr<ErlTerm> mPattern;
    VariableBinding *mBinding;

};

} // namespace node
} // namespace epi

#endif // __PATTERNMATCHINGGUARD_HPP

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

#ifndef _EPIBUFFER_H
#define _EPIBUFFER_H

#include "EpiError.hpp"

// Forward declaration of erlang types
namespace epi {
namespace type {
    class ErlAtom;
    class ErlBinary;
    class ErlConsList;
    class ErlDouble;
    class ErlEmptyList;
    class ErlList;
    class ErlLong;
    class ErlPid;
    class ErlPort;
    class ErlRef;
    class ErlString;
    class ErlTerm;
    class ErlTuple;
    class VariableBinding;
} // namespace epi
} // namespace types


namespace epi {
namespace node {

using namespace epi::error;
using namespace epi::type;


/**
 * Abstract buffer.
 */
class Buffer {

public:
    /**
     * Reset the buffer.
     */
    virtual void reset() = 0;

     /**
      * Reset the internal index
      */
    virtual void resetIndex() = 0;

};

} // node
} // epi
#endif

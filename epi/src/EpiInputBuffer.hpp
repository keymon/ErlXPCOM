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

#ifndef _EPIINPUTBUFFER_H
#define _EPIINPUTBUFFER_H

#include "EpiBuffer.hpp"


namespace epi {
namespace type {
    class ErlTerm;
} // namespace epi
} // namespace types

namespace epi {
namespace node {
/**
 * Input buffer
 */
class InputBuffer: public Buffer {
public:
    inline virtual ~InputBuffer() {};
    /**
     * Deserialize the next term in the buffer
     * @returns a pointer to a new ErlTerm. 0 if there are no terms
     * to decode
     */
    virtual ErlTerm* readTerm() throw(EpiDecodeException) = 0;

};

} // node
} // epi

#endif

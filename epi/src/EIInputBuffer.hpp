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

#ifndef _EIINPUTBUFFER_H
#define _EIINPUTBUFFER_H


#include "EIBuffer.hpp"
#include "EpiInputBuffer.hpp"

namespace epi {
namespace ei {

using namespace epi::node;
using namespace epi::type;
using namespace epi::error;
class EIOutputBuffer;

/**
 * EI library Input buffer
 */
class EIInputBuffer: public epi::node::InputBuffer, public EIBuffer {
    friend class EIOutputBuffer;
public:

    EIInputBuffer(const bool with_version=true);

    virtual ~EIInputBuffer();

    virtual ErlTerm* readTerm() throw(EpiDecodeException);

    virtual void reset();

    virtual void resetIndex();

protected:
    EIInputBuffer(ei_x_buff &buffer, const bool with_version);

    int mDecodeIndex;

    int *getDecodeIndex();
};

}
}

#endif

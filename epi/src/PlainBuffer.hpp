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

#ifndef _PLAINBUFFER_H
#define _PLAINBUFFER_H

#include <vector>
#include "ErlTerm.hpp"
#include "ErlTermPtr.hpp"
#include "EpiInputBuffer.hpp"
#include "EpiOutputBuffer.hpp"


namespace epi {
namespace node {

using namespace epi::type;
using namespace epi::error;

/**
 * This buffer does not have encoded terms, simple stores a
 * references to them
 */
class PlainBuffer: public InputBuffer, public OutputBuffer  {
    typedef std::vector<ErlTermPtr<ErlTerm> > erlterm_list;

public:
    PlainBuffer();
    PlainBuffer(const PlainBuffer &from);
    virtual ~PlainBuffer();

    virtual ErlTerm* readTerm() throw(EpiDecodeException);
    virtual void reset();
    virtual void resetIndex();

    virtual void writeTerm(ErlTerm *t, const VariableBinding *binding = 0)
            throw(EpiInvalidTerm, EpiEncodeException, EpiVariableUnbound);
    virtual InputBuffer *getInputBuffer();

protected:
    unsigned int mDecodeIndex;
    erlterm_list mTermList;

};

} // node
}


#endif

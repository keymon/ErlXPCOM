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



#include "PlainBuffer.hpp"

using namespace epi::error;
using namespace epi::type;
using namespace epi::node;

PlainBuffer::PlainBuffer(): mDecodeIndex(0), mTermList()
{
}

PlainBuffer::PlainBuffer(const PlainBuffer &from): mDecodeIndex(0), mTermList(from.mTermList)
{
}

ErlTerm* PlainBuffer::readTerm() throw(EpiDecodeException)
{
    if (mDecodeIndex == mTermList.size()) {
        return 0;
    }
    return mTermList[mDecodeIndex++].get();
}

void PlainBuffer::reset() {
    mTermList.clear();
    mDecodeIndex = 0;
}

void PlainBuffer::resetIndex() {
    mDecodeIndex = 0;
}

PlainBuffer::~PlainBuffer()
{
}

void PlainBuffer::writeTerm(ErlTerm *t, const VariableBinding *binding)
        throw(EpiInvalidTerm, EpiEncodeException, EpiVariableUnbound)
{
    if (t == 0) {
        throw EpiInvalidTerm("Null pointer");
    }
    // We "encode" a term without variables, using subst
    mTermList.push_back(t->subst(binding));
}

InputBuffer *PlainBuffer::getInputBuffer() {
    // return  a copy of this buffer
    return new PlainBuffer(*this);
}




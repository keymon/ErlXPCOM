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



#include "EIBuffer.hpp"

using namespace epi::ei;


EIBuffer::EIBuffer(const bool with_version): mWithVersion(with_version) {
    if (with_version) {
        ei_x_new_with_version(&mBuffer);
    } else {
        ei_x_new(&mBuffer);
    }
}

EIBuffer::EIBuffer(ei_x_buff &buffer, const bool with_version):
        mBuffer(buffer), mWithVersion(with_version)
{
    do_resetIndex();
}

EIBuffer::~EIBuffer() {
    ei_x_free(&mBuffer);
}

void EIBuffer::do_reset() {
    ei_x_free(&mBuffer);
    if (mWithVersion) {
        ei_x_new_with_version(&mBuffer);
    } else {
        ei_x_new(&mBuffer);
    }
}

void EIBuffer::do_resetIndex() {
    mBuffer.index = 0;
    // skip version number if necesary
    if (mWithVersion) {
        int version;
        ei_decode_version(mBuffer.buff, &mBuffer.index, &version);
    }
}

ei_x_buff* EIBuffer::getBuffer() { return &mBuffer; }

char * EIBuffer::getInternalBuffer() {
    return mBuffer.buff;
}

int * EIBuffer::getInternalIndex() {
    return &(mBuffer.index);
}

int * EIBuffer::getInternalBufferSize() {
    return &(mBuffer.buffsz);
}

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
#ifndef _EIBUFFER_H
#define _EIBUFFER_H

#include "ei.h"

namespace epi {
namespace ei {

class EIConnection;
class EIMessageAcceptor;

/**
 * Implementation of Buffer using EI library
 * It stores a dinamic ei buffer (ei_x_buff)
 */
class EIBuffer {
    friend class EIConnection;
    friend class EIMessageAcceptor;
public:

    virtual ~EIBuffer();

    /**
     * Reset the buffer.
     */
    virtual void do_reset();

     /**
      * Reset the internal index
      */
    virtual void do_resetIndex();

protected:
    /**
     * Create a new buffer
     * @param with_version Append or not the magic version number.
     *  Default: true
     */
    EIBuffer(const bool with_version);

    /**
     * Create a new buffer from other buffer.
     */
    EIBuffer(ei_x_buff &buffer, const bool with_version);

    ei_x_buff mBuffer;
    bool mWithVersion;

    /**
     * Return the pointer to buffer
     */
    virtual ei_x_buff* getBuffer();

    /**
     * Get the char* buffer from inner ei_x_buff
     */
    virtual char * getInternalBuffer();

    /**
     * Get the pointer to the index of inner ei_x_buff
     */
    virtual int * getInternalIndex();

    /**
     * Get the pointer to the size of inner ei_x_buff
     */
    virtual int * getInternalBufferSize();

};

} // ei
} // epi
#endif

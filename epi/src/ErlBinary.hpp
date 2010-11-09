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

#ifndef _ERLBINARY_HPP
#define _ERLBINARY_HPP

#include "ErlTerm.hpp"

namespace epi {
namespace type {

/**
 * Provides a representation of Erlang Binary
 **/
class ErlBinary: public ErlTerm {

public:

    /**
     * Create an unitialized binary
     **/
    inline ErlBinary(): ErlTerm(), mData((char *) 0) {}

    /**
     * Create a binary from the given data.
     * Data if not copied, data will be deleted on destruction.
     * Data is shared between all cloned binarys
     * @param data Pointer to data.
     * @param size binary size in bytes
     * @param copy copy the data or not. Default=true
     * @param copy delete data on destruction or not. Default=true
     **/
    inline ErlBinary(const void *data, const unsigned int size,
              const bool copy=true, const bool del = true):
            ErlTerm(), mData((char *) 0) {
        try {
            init(data, size, copy, del);
        } catch (EpiException &e) {
        }
    }


    /**
     * Init this binary with the given value.
     * @param data Pointer to data
     * @param size binary size in bytes
     * @param copy copy the data or not. Default=true
     * @throws EpiAlreadyInitialized if the binary is already initialized
     */
    void init(const void *data, const unsigned int size,
         const bool copy=true, const bool del=true)
            throw(EpiAlreadyInitialized);

    /**
     * Get the actual data contained in this term. ¡¡DO NOT DELETE!!
     */
    inline const void * binaryData() const
            throw(EpiInvalidTerm)
    {
        if (!isValid()) {
            throw EpiInvalidTerm("Binary is not initialized");
        }
        return mData;
    }

    /**
     * Get the size of the data (in bytes)
     */
    inline unsigned int size() const
            throw(EpiInvalidTerm)
    {
        if (!isValid()) {
            throw EpiInvalidTerm("Binary is not initialized");
        }
        return mSize;
    }

    bool equals(const ErlTerm &t) const;

    std::string toString(const VariableBinding *binding = 0) const;

    IMPL_TYPE_SUPPORT(ErlBinary, ERL_BINARY);

private:
    ErlBinary (const ErlBinary &t) {}

    inline ~ErlBinary() {
        if (mDelete)
            delete[] mData;
    }


protected:
    char *mData;
    bool mDelete;
    unsigned int mSize;

};

} //namespace type
} //namespace epi


#endif // _ERLBINARY_HPP

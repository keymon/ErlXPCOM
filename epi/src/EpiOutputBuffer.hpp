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

#ifndef _EPIOUTPUTBUFFER_H
#define _EPIOUTPUTBUFFER_H

#include "EpiBuffer.hpp"
#include "EpiOutputBuffer.hpp"


namespace epi {
namespace type {
    class ErlTerm;
} // namespace epi
} // namespace types

namespace epi {
namespace node {

class InputBuffer;

/**
 * Output buffer
 */
class OutputBuffer: public Buffer {
public:
    inline virtual ~OutputBuffer() {};

     /**
      * Write a term in this buffer
      * Will fail if the term is not valid.
      * @param t ErlTerm to write
      * @param binding Binding to be use with variables. It can be null
      * @throw EpiInvalidTerm If the term is invalid
      * @throw EpiEncodeException If there is an encoding error
      * @throw EpiVariableUnbound if there is an unbound variable
      */
     virtual void writeTerm(ErlTerm *t, const VariableBinding *binding = 0)
             throw(EpiInvalidTerm, EpiEncodeException, EpiVariableUnbound) = 0;


     /**
      * Returns a pointer to a new InputBuffer with the data
      * of this OutputBuffer.
      * The state of the OutputBuffer after this call is not defined, and
      * should be deleted.
      */
     virtual InputBuffer *getInputBuffer() = 0;

};

} // node
} // epi

#endif

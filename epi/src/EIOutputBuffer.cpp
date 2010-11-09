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



#include "EIOutputBuffer.hpp"
#include "EIInputBuffer.hpp"
#include "ErlTypes.hpp"
#include "EpiUtil.hpp"

using namespace epi::node;
using namespace epi::type;
using namespace epi::error;
using namespace epi::util;
using namespace epi::ei;

EIOutputBuffer::EIOutputBuffer(const bool with_version):
        EIBuffer(with_version)
{
}

EIOutputBuffer::~EIOutputBuffer()
{
}

void EIOutputBuffer::reset() {
    do_reset();
}

void EIOutputBuffer::resetIndex() {
    do_resetIndex();
}

void EIOutputBuffer::writeTerm(ErlTerm *t, const VariableBinding *binding)
        throw(EpiInvalidTerm, EpiEncodeException, EpiVariableUnbound)
{
    if (t) {
        Dout_continue(dc::connect, _continue, " failed.",
                      "InputBuffer::WriteTerm(" << t->toString() << "):");
        int ei_res;
        switch(t->termType()) {
        case ERL_ATOM:
            ei_res = ei_x_encode_atom(this->getBuffer(),
                                      ((ErlAtom*) t)->atomValue());
            if (ei_res < 0) {
                throw EpiEIEncodeException("EI atom encoding failed", ei_res);
            }
            break;
        case ERL_LONG:
            ei_res = ei_x_encode_longlong(this->getBuffer(),
                                      ((ErlLong*) t)->longValue());

            if (ei_res < 0) {
                throw EpiEIEncodeException("EI long encoding failed", ei_res);
            }
            break;

        case ERL_DOUBLE:
            ei_res = ei_x_encode_double(this->getBuffer(),
                                        ((ErlDouble*) t)->doubleValue());

            if (ei_res < 0) {
                throw EpiEIEncodeException("EI double encoding failed", ei_res);
            }
            break;
        case ERL_STRING:
            ei_res = ei_x_encode_string(this->getBuffer(),
                                        ((ErlString*) t)->stringValue());

            if (ei_res < 0) {
                throw EpiEIEncodeException("EI string encoding failed", ei_res);
            }
            break;
        case ERL_REF:
            if (1==1) {
                erlang_ref *ref;
                if ((ref = ErlRef2EI((ErlRef*) t)) == 0) {
                    throw EpiInvalidTerm("Reference not initialized");
                }

                ei_res = ei_x_encode_ref(this->getBuffer(), ref);
                delete ref;

                if (ei_res < 0) {
                    throw EpiEIEncodeException("EI ref encoding failed", ei_res);
                }
            }
            break;

        case ERL_PORT:
            if (1==1) {
                erlang_port *port;
                if ((port = ErlPort2EI((ErlPort*) t)) == 0) {
                    throw EpiInvalidTerm("Reference not initialized");
                }

                ei_res = ei_x_encode_port(this->getBuffer(), port);
                delete port;
                if (ei_res < 0) {
                    throw EpiEIEncodeException("EI port encoding failed", ei_res);
                }
            }
            break;

        case ERL_PID:
            if (1==1) {
                erlang_pid *pid;
                if ((pid = ErlPid2EI((ErlPid*) t)) == 0) {
                    throw EpiInvalidTerm("Reference not initialized");
                }
                ei_res = ei_x_encode_pid(this->getBuffer(), pid);
                delete pid;

                if (ei_res < 0) {
                    throw EpiEIEncodeException("EI pid encoding failed", ei_res);
                }
            }

            break;
        case ERL_BINARY:
            ei_res = ei_x_encode_binary(this->getBuffer(),
                                            ((ErlBinary*) t)->binaryData(),
                                            ((ErlBinary*) t)->size());

            if (ei_res < 0) {
                throw EpiEIEncodeException("EI binary encoding failed", ei_res);
            }
            break;

        case ERL_TUPLE:
            if (1==1) {
                ErlTuple *tuple = (ErlTuple*) t;

                ei_res = ei_x_encode_tuple_header(this->getBuffer(), tuple->arity());

                if (ei_res < 0) {
                    throw EpiEIEncodeException("EI tuple encoding failed", ei_res);
                }

                for (unsigned int i = 0; i < tuple->arity(); i++) {
                    this->writeTerm(tuple->elementAt(i), binding);
                }
            }
            break;
        case ERL_EMPTY_LIST:
            ei_res = ei_x_encode_empty_list(this->getBuffer());

            if (ei_res < 0) {
                throw EpiEIEncodeException("EI empty list encoding failed", ei_res);
            }
            break;
        case ERL_LIST:
        case ERL_CONS_LIST:
            if (1==1) {
                ErlConsList* list = (ErlConsList*) t;

                ei_x_encode_list_header(this->getBuffer(), list->arity());

                // Encode all elements and last tail
                for(unsigned int i=0; i<list->arity(); i++) {
                    this->writeTerm(list->elementAt(i), binding);
                }
                this->writeTerm(list->tail(list->arity()-1), binding);

            }
            break;
        case ERL_VARIABLE:
            if (1==1) {
                ErlVariable* variable = (ErlVariable*) t;
                if (variable->getName() == "_") {
                    throw EpiInvalidTerm("You can't encode an anonymous variable");
                }
                ErlTerm* term = binding? binding->search(variable->getName()): 0;
                if (term) {
                    this->writeTerm(term, binding);
                } else {
                    throw EpiVariableUnbound(variable->getName());
                }
            }

            break;
        }
        Dout_finish(_continue, " encoded.");
    } else {
        throw EpiInvalidTerm("trying to encode a null pointer");
    }
}

InputBuffer * EIOutputBuffer::getInputBuffer( )
{
    InputBuffer * inputBuffer = new EIInputBuffer(mBuffer, mWithVersion);
    if (mWithVersion) {
        ei_x_new_with_version(&mBuffer);
    } else {
        ei_x_new(&mBuffer);
    }
    return inputBuffer;

}

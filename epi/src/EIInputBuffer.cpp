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



#include "ErlTypes.hpp"
#include "EIInputBuffer.hpp"

using namespace epi::node;
using namespace epi::error;
using namespace epi::type;
using namespace epi::ei;

EIInputBuffer::EIInputBuffer(const bool with_version):
        EIBuffer(with_version)
{
    mDecodeIndex = mBuffer.index;
}

EIInputBuffer::~EIInputBuffer( )
{
}

EIInputBuffer::EIInputBuffer(ei_x_buff &buffer, const bool with_version):
        EIBuffer(buffer, with_version)
{
    // Set the decode index to same position than internal index
    mDecodeIndex = mBuffer.index;
}


void EIInputBuffer::reset() {
    do_reset();
}

void EIInputBuffer::resetIndex() {
    do_resetIndex();
}

ErlTerm* EIInputBuffer::readTerm() throw(EpiDecodeException)
{
    Dout_continue(dc::buffer, _continue, " failed.",
                  "["<<this<<"]" << "EIInputBuffer::readTerm(): ");

    // Check if is there are more elements to decode
    if (*this->getDecodeIndex() == *this->getInternalBufferSize()) {
        return 0;
    }

    // check the type of next term:
    int type, size, ei_res = 0;

    ei_res = ei_get_type(this->getInternalBuffer(),
                         this->getDecodeIndex(), &type, &size);
    if(ei_res < 0) {
        throw EpiEIDecodeException("ei_get_type failed", ei_res);
    }

    ErlTerm* returnTerm = 0;

    switch (type) {
    case ERL_ATOM_EXT:
        if (1==1) { // Fake context
            Dout_continued(  "decoding an Atom: ");
            char *atom = new char[size+1];
            ei_res = ei_decode_atom(this->getInternalBuffer(),
                                    this->getDecodeIndex(), atom);
            if (ei_res < 0) {
                throw EpiEIDecodeException("EI atom decoding failed", ei_res);
            }
            try {
                returnTerm = new ErlAtom(atom, false);
            } catch (EpiBadArgument &e) {
                throw EpiEIDecodeException(e.getMessage());
            }
        }
        break;
    case ERL_PID_EXT:
        if (1==1) {
            Dout_continued("decoding a Pid: ");
            erlang_pid pid;
            ei_res = ei_decode_pid(this->getInternalBuffer(),
                                       this->getDecodeIndex(), &pid);
            if (ei_res < 0) {
                throw EpiEIDecodeException("Error decoding pid", ei_res);
            }

            	try {
                returnTerm = new ErlPid(pid.node, pid.num, pid.serial, pid.creation);
            } catch(EpiException &e) {
                throw EpiEIDecodeException(e.getMessage());
            }
        }
        break;

    case ERL_LARGE_TUPLE_EXT:
    case ERL_SMALL_TUPLE_EXT:
        if (1==1) {
            Dout_continued( "decoding a Tuple: ");
            int arity;

            ei_res = ei_decode_tuple_header(this->getInternalBuffer(),
                                                this->getDecodeIndex(), &arity);
            if (ei_res < 0) {
                throw EpiEIDecodeException("EI tuple decoding failed", ei_res);
            }
            try {
                ErlTermPtr<ErlTuple> new_tuple(new ErlTuple(arity));
                for (int i=0; i < arity; i++) {
                    new_tuple->initElement(this->readTerm());
                }
                returnTerm = new_tuple.drop();
            } catch(EpiDecodeException &e) {
                throw e;
            } catch(EpiException &e) {
                throw EpiDecodeException(e.getMessage());
            }
        }
        break;

    case ERL_STRING_EXT:
        if (1==1) {
            Dout_continued( "decoding a String: ");
            char* string_data = new char[size+1];
            ei_res = ei_decode_string(this->getInternalBuffer(),
                                      this->getDecodeIndex(), string_data);
            if (ei_res < 0) {
                delete [] string_data;
                throw EpiEIDecodeException("EI string decoding failed", ei_res);
            }

            returnTerm = new ErlString(string_data, size, false);
        }
        break;

    case ERL_LIST_EXT:
        if (1==1) {
            Dout_continued( "decoding a List: ");
            int arity;

            ei_res = ei_decode_list_header(this->getInternalBuffer(),
                                           this->getDecodeIndex(), &arity);

            if (ei_res < 0) {
                throw EpiEIDecodeException("EI list decoding failed", ei_res);
            }

            if (arity == 0) {
                // EI docs says that this not happens... but, let's add it
                returnTerm = new ErlEmptyList();
            } else {
                ErlTermPtr<ErlConsList> new_list(new ErlConsList(arity)) ;
                try {
                    for (int i=0; i <= arity; i++) {
                    // If the decoding is success, add it (check if is the tail)
                        if (i!=arity)
                            new_list->addElement(this->readTerm());
                        else
                            new_list->close(this->readTerm());
                    }
                } catch (EpiDecodeException &e) {
                    throw e;
                } catch (EpiException &e) {
                    throw EpiDecodeException(e.getMessage());
                }

                returnTerm = new_list.drop();
            }
        }
        break;

    case ERL_NIL_EXT:
        if (1==1) {
            Dout_continued( "decoding a Empty list: ");
            int arity;
            ei_res = ei_decode_list_header(this->getInternalBuffer(),
                                       this->getDecodeIndex(), &arity);

            if(ei_res < 0 || arity != 0) {
                throw EpiEIDecodeException("EI empty list decoding failed", ei_res);
            }
            returnTerm = new ErlEmptyList();
        }
        break;

    case ERL_SMALL_BIG_EXT:
    case ERL_LARGE_BIG_EXT:
        // FIXME: Only 32 bits integers are supported
    case ERL_SMALL_INTEGER_EXT:
    case ERL_INTEGER_EXT:
        if (1==1) {
            Dout_continued( "decoding a Long: ");
            long long vlong;
            ei_res = ei_decode_longlong(this->getInternalBuffer(),
                                        this->getDecodeIndex(), &vlong);

            if (ei_res < 0) {
                throw EpiEIDecodeException("EI long decoding failed", ei_res);
            }

            returnTerm = new ErlLong(vlong);
        }
        break;

    case ERL_FLOAT_EXT:
        if (1==1) {
            Dout_continued( "decoding a Double: ");
            double vdouble;
            ei_res = ei_decode_double(this->getInternalBuffer(),
                                          this->getDecodeIndex(), &vdouble);

            if (ei_res < 0) {
                throw EpiEIDecodeException("EI double decoding failed", ei_res);
            }

            returnTerm = new ErlDouble(vdouble);
        }
        break;

    case ERL_BINARY_EXT:
        if (1==1) {
            Dout_continued( "decoding a Binary: ");

            char *data = new char[size];
            // EI lib needs a long as parameter of ei_decode_binary but returns
            // a int in ei_get_type
            long binary_size = size;
            ei_res = ei_decode_binary(this->getInternalBuffer(),
                                      this->getDecodeIndex(),
                                      data, &binary_size);
            if (ei_res < 0) {
                delete [] data;
                throw EpiEIDecodeException("EI binary decoding failed", ei_res);
            }
            returnTerm = new ErlBinary(data, size, false, true);
        }
        break;


    case ERL_NEW_REFERENCE_EXT:
        if (1==1) {
            Dout_continued( "decoding a Reference: ");
            erlang_ref ref;
            ei_res = ei_decode_ref(this->getInternalBuffer(),
                                       this->getDecodeIndex(), &ref);
            if (ei_res < 0) {
                throw EpiEIDecodeException("Error decoding", ei_res);
            }
            try {
                bool newStyle = ref.len != 1;
                returnTerm = new ErlRef(ref.node, ref.n, ref.creation, newStyle);;
            } catch(EpiException &e) {
                throw EpiEIDecodeException(e.getMessage());
            }
        }
        break;

    case ERL_PORT_EXT:
        if (1==1) {
            Dout_continued( "decoding a Port: ");
            erlang_port port;
            ei_res = ei_decode_port(this->getInternalBuffer(),
                                        this->getDecodeIndex(), &port);
            if (ei_res < 0) {
                throw EpiEIDecodeException("Error decoding", ei_res);
            }

            try {
                returnTerm = new ErlPort(port.node, port.id, port.creation);
            } catch(EpiException &e) {
                throw EpiDecodeException(e.getMessage());
            }
        }
        break;

    default:
        std::ostringstream oss;
        oss << "Unknown message content type " << type;

        Dout_continued( "Unknown message content type " << type);
        throw EpiEIDecodeException(oss.str());
        break;
    }

    Dout_finish(_continue, returnTerm->toString() << ".");
    return returnTerm;

}

int *EIInputBuffer::getDecodeIndex() {
    return &mDecodeIndex;
}

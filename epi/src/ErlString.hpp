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

#ifndef _ERLSTRING_HPP
#define _ERLSTRING_HPP

#include <string.h>
#include "ErlList.hpp"

namespace epi {
namespace type {

/**
 * Provides a representation of Erlang strings.
 **/
class ErlString: public ErlList {

public:

    /**
     * Create an unitialized (and invalid) string
     **/
    inline ErlString() {
    };

    /**
     * Create an string from the given string.
     * @param string the string to create the ErlString from. The 
	 *   size will computed from the string
 	 * @param copy Copy the given string or  not
     **/
    ErlString(const char *string, bool copy = true) {
        try{
            init(string, strlen(string), copy);
        } catch (EpiAlreadyInitialized &e) {
        }
    }

    /**
     * Create an string from the given string.
     * @param string buffer to create the ErlString from.
	 * @param size Size of the string
	 * @param copy Copy the given string or  not
     **/
    ErlString(const char *string, int size, bool copy = true) {
        try{
            init(string, size, copy);
        } catch (EpiAlreadyInitialized &e) {
        }
    }

    /**
     * Create an string from the given buffer starting
	 * at index 'begin' with size 'size'
     * @param string the string to create the ErlString from.
     * @param begin the start index.
     * @param end the size.
     **/
    ErlString(const char *string, unsigned int begin, unsigned int size) {
        try{
            init(string, begin, size);
        } catch (EpiAlreadyInitialized &e) {
        }
    }

    /**
     * Init this string with the given string.
     *
     * @param string the string to init the ErlString from.
	 * @param copy Copy the given string or  not
     * @throws EpiAlreadyInitialized if the string is already initialized
     */
    void init(const char* string, int size, bool copy=true)
            throw (EpiAlreadyInitialized);

    /**
     * Init this string with the given buffer starting
	 * at index 'begin' with size 'size'
     * @param string the string to create the ErlString from.
     * @param begin the start index.
     * @param end the size.
     **/
    void init(const char *string, unsigned int begin, unsigned int size)
            throw (EpiAlreadyInitialized);

    virtual inline unsigned int arity() const {
		return mArity;
	}

    virtual ErlTerm* elementAt(unsigned int index) const
            throw(EpiInvalidTerm, EpiBadArgument, EpiEmptyList);

    virtual ErlTerm* tail(unsigned int index) const
            throw(EpiInvalidTerm, EpiBadArgument, EpiEmptyList);

    /**
     * Get the actual string contained in this term.
     * @throws EpiInvalidTerm if the term is invalid
     **/
    const char* stringValue() const
            throw(EpiInvalidTerm);

    virtual std::string toString(const VariableBinding *binding = 0) const;

    virtual inline ~ErlString() {
    }

	IMPL_TYPE_SUPPORT2(ErlString, ERL_STRING, ERL_LIST);

private:
    ErlString (const ErlString &t) {}

protected:
    const char* mString;
	unsigned int mArity;

};

} //namespace type
} //namespace epi

#endif // _ERLSTRING_HPP

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

#ifndef _ERLTERM_H
#define _ERLTERM_H

#include <string>
#include <vector>
#include <memory> 

#include "EpiError.hpp" 
#include "ErlTermImpl.hpp"


// forward declaration of buffers
namespace epi {
namespace node {
class OutputBuffer;
class InputBuffer;
}
}

namespace epi {
namespace type {

using namespace epi::error;
using namespace epi::node;

class VariableBinding;


////////////////////////////////////////////////////////////////////////
/**
 * Term type values. Each term have a type value.
 * The type is necessary for term comparation.
 */
enum TermType {
    ERL_ATOM = 0,
    ERL_LONG,
    ERL_DOUBLE,
    ERL_STRING,
    ERL_REF,
    ERL_PORT,
    ERL_PID,
    ERL_BINARY,
    ERL_TUPLE,
    ERL_LIST,
    ERL_EMPTY_LIST,
    ERL_CONS_LIST,
    ERL_VARIABLE
};

/** Maximun sizes */
static const unsigned int MAX_HOSTNAME_LENGTH = 64;
static const unsigned int MAX_ALIVE_LENGTH = 63;
static const unsigned int MAX_COOKIE_SIZE = 512;
static const unsigned int MAX_ATOM_LENGTH = 255;
static const unsigned int MAX_NODE_LENGTH =
        MAX_ALIVE_LENGTH + MAX_HOSTNAME_LENGTH + 1;

////////////////////////////////////////////////////////////////////////
// Forward declaration of compound terms
class ErlTuple;
class ErlConsList;
class ErlVariable;

/**
 * Base class of the Erlang data type classes.
 * This class is used to represent an arbitrary Erlang term.
 *
 * A ErlTerm is always readonly, but can be partially initialized.
 * ErlTerm initialization is not thread safe!
 *
 * The user must manage ever ErlTerm* pointers.
 * All ErlTerm pointers can be shared, using reference counting.
 * Two methods are provided to manage reference counting:
 *  - addRef(): Increases the reference counting by one
 *  - release(): Decreases reference counting, and if reference counter
 *     is <= 0, deletes the object.
 * Other methods are:
 *  - drop(): only decreases the counted, and not deletes the object.
 *       This method must be used with care.
 *
 * The ErlTerm destructor is protected, so user can't delete an ErlTerm
 * directly, he must call release() method. This ensures that an ErlTerm
 * will be deleted if, and only if reference counter <= 0.
 *
 * When an ErlTerm is created, it has refcount = 0. The user should
 * addRef() and release() it when necesary. If the release method is
 * never call, the ErlTerm will never be deleted.
 *
 * A smart pointer, ErlTermPtr, that manages addRef() and  release() is provided,
 *
 * Copy ErlTerms is diallowed, so copy constructor is private.
 * Copy the pointer and use the addRef() method instead.
 *
 *
 * All subclasses implement a static cast() method, that will cast
 * any ErlTerm* to the given class. It will raise EpiBadArgument if
 * the type is not compatible, increasing and decreasing the counter
 * of the term to ensure that the term is deleted if counter = 0
 */
class ErlTerm {
    // Needed to access to encode method
    friend class ErlTuple;
    friend class ErlList;
    friend class ErlConsList;
    friend class ErlVariable;
    friend class OutputBuffer;
    friend class InputBuffer;

    typedef int refcnt;
public:
    /*
     All subclasses must:
      - define a default constructor (no arguments) that creates
          an unitialized term.
      - define method instanceOf with clases that implements
      - declare copy constructor as private
      - declare destructor (if necesary) as private
      - define method equals
      - define method ToString
    */

    inline ErlTerm(): mRefCount(0), mInitialized(false) {
        Dout(dc::erlang_memory, "["<<this<<"]" <<"new ErlTerm()");
    }

    /**
     * Increment the reference counter by 1
     * @return value of ref counter
     */
    inline refcnt addRef() {
        Dout(dc::erlang_memory, "["<<this<<"]" <<"addRef() -> refcnt=" << mRefCount+1);
        return ++mRefCount;
    }

    /**
     * Decrement the reference counter, deleting the object if necesary
     * @return value of ref counter. If counter <=0 the object have been deleted
     */
    inline refcnt release() {
        Dout(dc::erlang_memory, "["<<this<<"]" <<"release() -> refcnt= " << mRefCount-1);
        if (--mRefCount <= 0) {
            delete this;
			return 0;
        }
        return mRefCount;
    }

    /**
     * Check if this term is valid, or have a valid value.
     * By default, simply checks if it is initialized
     */
    virtual bool isValid() const;

    /**
     * Check if the object is an instance of a concrete class. This method
     * is necesary to implement comparation method without rtti.
     * @param type Class type identificator
     */
    virtual bool instanceOf(const TermType type) const = 0;

    /**
     * Get the concrete TermType for this term
     */
	virtual TermType termType() const = 0;

    /**
     * Check if two terms are equal
     */
    virtual bool equals(const ErlTerm &t) const = 0;

    /**
     * Get the string representation of this using a variable binding
     * @param binding Variable binding to use. It can be null.
     */
    virtual std::string toString(const VariableBinding *binding = 0) const = 0;

    /**
     * Perform pattern matching.
     * @param pattern Pattern (ErlTerm) to match
     * @param binding VariableBinding to use in pattern matching.
     *  This binding will be updated with new bound variables if
     *  match succes.
     * @return true if matching success or false if fails.
     */
    bool match(ErlTerm* pattern, VariableBinding* binding = 0)
            throw (EpiVariableUnbound);

    /**
     * Returns the equivalent without inner variables, using the
     * given binding to substitute them.
     * The normal behaviour is:
     *  - Simple types return themself
     *  - Compound types check any of the contained terms changed,
     *      returning themself if there is no changed
     *  - Variables substitutes.
     * So, the returned pointer can be zero counted.
     * @param binding VariableBinding to use to resolve variables.
     *  It can be 0 and no substituions will be performed, throwing
     *  EpiVariableUnbound if there is a variable.
     * @returns a ErlTerm* to the new term without ErlVariables
     * @throws EpiInvalidTerm if the term is invalid
     * @throws EpiVariableUnbound if a variable is unbound
     */
    virtual ErlTerm* subst(const VariableBinding* binding)
            throw (EpiInvalidTerm, EpiVariableUnbound);

    /**
     * This method searchs the first unbound variable into a term for
     * the given binding. It can be used to check if the term has
     * unbound variables
     * @param binding VariableBinding to check where.
     * @returns a pointer to ErlVariable if there is an unbound variable,
     *  null otherwise.
     */
    virtual ErlVariable* searchUnbound(const VariableBinding* binding) {
        return 0;
    }


    /**
     * Decrement the reference counter, but does not delete the ErlTerm.
     * To be used by ErlTermPtr is you want to return an zero referenced
     * ErlTerm.
     * If reference counter == 0, it is not decreased.
     * Use this method with care!!
     * @return value of ref counter
     */
    inline refcnt drop() {
        Dout(dc::erlang_memory, "["<<this<<"]" << "drop() refcnt=" << mRefCount-1);
        if (mRefCount == 0) {
            return 0;
        } else {
            return --mRefCount;
        }
    }


    /**
     * FIXME: Implement with a stream.
     *
     * Create an ErlTerm from an string representation. Like erl_format in
     * ErlLibrary you can use this function to
     * create Erlang terms using a format specifier and a corresponding set
     * of arguments, much in the way printf() works.
     *
     * The set of valid format specifiers is as follows:
     * <ul>
     * <li>~i - Integer
     * <li>~f - Floating point
     * <li>~a - Atom
     * <li>~s - String
     * <li>~w - Arbitrary Erlang term
     * Example:
     *   ErlTerm::format("[{name,~a},{age,~i},{data,~w}]",
     *         "madonna",
     *          21,
     *          ErlTerm::format("[{adr,~s,~i}]","E-street",42));
     *
     * @return and zero reference counted ErlTerm
     * @throws EpiParseError if the string is incorrect
     */
     static ErlTerm * format(const std::string &formatStr, ...)
               throw (EpiParseError);

private:

protected:
    refcnt mRefCount;

    bool mInitialized;

    /** Protected VIRTUAL!!! destructor. Use release() for destruction */
    inline virtual ~ErlTerm() {
        Dout(dc::erlang_memory, "["<<this<<"]" <<"  \\-delete");
    }
    /**
     * Implementation of matching process.
     * This function can change the binding if the matching fails.
	 * Be carefull if you redefine this method, consider ErlVariable classes.
     * @throws EpiVariableUnbound if an unbound variable is found
     */
    virtual bool internalMatch(VariableBinding* binding, ErlTerm* pattern)
            throw (EpiVariableUnbound);

};

/**
 * Default '==' operator, delegates in Equals method.
 */
inline bool operator==(const ErlTerm &t1, const ErlTerm &t2) {
    return t1.equals(t2);
}

/**
 * Default '!=' operator, delegates in Equals method.
 */
inline bool operator!=(const ErlTerm &t1, const ErlTerm &t2) {
    return !t1.equals(t2);
}

} //namespace type
} //namespace epi


#endif

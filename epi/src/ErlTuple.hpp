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

#ifndef _ERLTUPLE_HPP
#define _ERLTUPLE_HPP

#include "ErlTerm.hpp"
#include "ErlTermPtr.hpp"

namespace epi {
namespace type {

/**
 * Representation of Erlang tuples.
 * One tuple is created from zero or more erlang Terms.
 *
 * The arity of the tuple is the number of elements it contains.
 * Elements are indexed from 0 to (arity-1) and can be retrieved
 * individually by using the appropriate index.
 *
 * The constructors and init functions in this class receive pointers
 * to ErlTerms to init the tuple elements. This ErlTerms will be
 * reference added.
 */
class ErlTuple: public ErlTerm {
    typedef std::vector<ErlTermPtr<ErlTerm> > erlterm_vector ;
public:

    /**
     * Create unitialized tuple
     * Arity and elements have to be initialized.
     */
    inline ErlTuple(): ErlTerm(), mArityDefined(false), mElementVector(0) {
        Dout(dc::erlang, "Created unitialized Tuple at" << this);
    }

    /**
     * Create a arity initialized tuple.
     * Elements must be initialized if arity > 0
     */
    ErlTuple(unsigned int arity);

    /**
     * Create a tuple from a array of (pointer to) terms.
	 * Be carefull with this contructor, do not use code like:
	 *   new ErlTuple(new X(), new Y());
	 * that could leak if construction of Y fails. Use initElement instead.
     * @param elems array of pointer to elements to create the tuple from.
     * @throws EpiBadArgument if  any of the elements is invalid
     */
    ErlTuple(ErlTerm *elems[], unsigned int arity) throw(EpiBadArgument);

    /**
     * Create a unary tuple containing the given element.
     * @param elem a pointer to the element to create the tuple from.
     * @throws EpiBadArgument if  any of the elements is invalid
     **/
    ErlTuple(ErlTerm *elem) throw(EpiBadArgument);

    /**
     * Create a tuple with two elements.
     * @throws EpiBadArgument if  any of the elements is invalid
     **/
    ErlTuple(ErlTerm *elem1, ErlTerm *elem2) throw(EpiBadArgument);

    /**
     * Create a tuple with three elements.
	 * Be carefull with this contructor, do not use code like:
	 *   new ErlTuple(new X(), new Y());
	 * that could leak if construction of Y fails. Use initElement instead.
     * @throws EpiBadArgument if  any of the elements is invalid
     **/
    ErlTuple(ErlTerm *elem1, ErlTerm *elem2, ErlTerm *elem3)
            throw(EpiBadArgument);

    /**
     * Create a tuple with four elements. 
	 * Be carefull with this contructor, do not use code like:
	 *   new ErlTuple(new X(), new Y());
	 * that could leak if construction of Y fails. Use initElement instead.
     * @throws EpiBadArgument if  any of the elements is invalid
     **/
    ErlTuple(ErlTerm *elem1, ErlTerm *elem2, ErlTerm *elem3, ErlTerm *elem4)
            throw(EpiBadArgument);

    /**
     * Init the term with the arity.
     * After this, elements must be initialized if arity > 0
     * @param arity the size of the tuples
     * @throws EpiAlreadyInitialized if the arity is already initialized
     */
    void init(unsigned int arity)
            throw(EpiAlreadyInitialized);

    /**
     * Init the term with an array of (pointer to) terms.
     * It can fail if:
     * @param elems array of pointer to elements to create the tuple from.
     *              This array should have a size >= arity. All pointers
     *              must point to valid terms.
     * @param arity Size of the tuple.
     * @throws EpiAlreadyInitialized if the arity is already initialized
     * @throws EpiBadArgument if arity < 0 or any of the elements is invalid
     */
    void init(ErlTerm *elems[], unsigned int arity)
            throw(EpiBadArgument, EpiAlreadyInitialized);

    /**
     * Init next element of the tuple. The element will be referend
     * increased.
     * @param elem an ErlTermPtr to the pointer to add
	 * @return pointer to self. So you can do something like:
	 *	tuple->initElement(element1)->initElement(element2);
     * @throws EpiInvalidTerm if the arity is not defined
     * @throws EpiAlreadyInitialized if all the elements of the tuple are
     *      initialized.
     * @throws EpiBadArgument if the element is invalid
     */
    ErlTuple* initElement(ErlTerm* elem)
            throw(EpiInvalidTerm, EpiBadArgument, EpiAlreadyInitialized);

    /**
     * Get the tuple arity defined for this (size of the tuple).
     * @throws EpiInvalidTerm if the arity is not defined
     */
    inline unsigned int arity() const
            throw(EpiInvalidTerm)
    {
        if (!mArityDefined)
            throw EpiInvalidTerm("Arity not defined");
        return mArity;
    }

    /**
     * Get the number of elements in the tuple initialized
     */
    inline unsigned int initializedCount() const {
        return mElementVector.size();
    }

    /**
     * Get one element by position
     * @returns  a pointer to the element if success. This pointer
     *  is at least referenced by this tuple.
     * @throws EpiBadArgument if index is out of range
     * @throws EpiInvalidTerm if element at this position is uninitialized or
     *      if the tuple is not valid (arity not initialized)
     */
    ErlTerm *elementAt(unsigned int index) const
            throw(EpiInvalidTerm, EpiBadArgument);

    inline ErlTerm *clone() const {
        return new ErlTuple(*this);
    }

    bool equals(const ErlTerm &t) const;

    std::string toString(const VariableBinding *binding = 0) const;

    ErlVariable* searchUnbound(const VariableBinding* binding) const;

    ErlTerm* subst(const VariableBinding* binding)
            throw (EpiInvalidTerm, EpiVariableUnbound);

    IMPL_TYPE_SUPPORT(ErlTuple, ERL_TUPLE);

private:
    inline ErlTuple(const ErlTuple &t) {}
    inline virtual ~ErlTuple() {
        Dout(dc::erlang, "["<<this<<"] ~ErlTuple()");
    }


protected:
    unsigned int mArity;
    bool mArityDefined;

    // Use a vector of shared pointers to ErlTerms
    erlterm_vector mElementVector;

    bool internalMatch(VariableBinding* binding, ErlTerm* pattern)
            throw (EpiVariableUnbound);

};

} //namespace type
} //namespace epi

#endif // _ERLTUPLE_HPP

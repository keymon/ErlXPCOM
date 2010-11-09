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

#ifndef _ERLCONSLIST_H
#define _ERLCONSLIST_H

#include "ErlList.hpp"
#include "ErlEmptyList.hpp"
#include "ErlTermPtr.hpp"

namespace epi {
namespace type {

/**
 * Representation of a erlang list constructor (Cons)
 *
 * This representation consists in an array of (shared) pointers to
 * erlang elements and a pointer to a tail.
 *
 * You can access directly to any element using elementAt(),
 * You can get a new list with a tail using tail()
 *
 * A ErlConsList will have at least an element and an tail.
 * If tail is ErlEmptyList, this is an proper list.
 *
 * TODO: map, iter, fold methods. iterators.
 *
 */
class ErlConsList: public ErlList {
public:
    /**
     * Create a unitialized list.
     * The list should be initialized using AddElement() and Close()
     */
    ErlConsList();

    /**
     * Create a unitialized list with an estimation of size.
     * The list should be initialized using AddElement() and Close()
     * @param estimated_size Estimation of the size of the list
     */
    ErlConsList(unsigned int estimated_size);

    /**
     * Create a proper list from a array of (pointer to) terms.
     * Pointer owership is transfered to list.
	 * Be carefull with this contructor, do not use code like:
	 *   new ErlConsList(new X(), new Y());
	 * that could leak if construction of Y fails. Use initElement instead.
     * @param elems array of pointer to elements to create the list from.
     * @param arity number of elements to add to the list
     */
    ErlConsList(ErlTerm *elems[], unsigned int arity) throw(EpiBadArgument);

    /**
     * Create a list containing the given element.
     * Pointer owership is transfered to list.
     * @param elem a pointer to the element to create the list from.
     **/
    ErlConsList(ErlTerm *elem) throw(EpiBadArgument);

    /**
     * Create a list with two elements.
	 * Be carefull with this contructor, do not use code like:
	 *   new ErlConsList(new X(), new Y());
	 * that could leak if construction of Y fails. Use initElement instead.
     **/
    ErlConsList(ErlTerm *elem1, ErlTerm *elem2) throw(EpiBadArgument);

    /**
     * Create a list with three elements.
	 * Be carefull with this contructor, do not use code like:
	 *   new ErlConsList(new X(), new Y());
	 * that could leak if construction of Y fails. Use initElement instead.
     **/
    ErlConsList(ErlTerm *elem1, ErlTerm *elem2, ErlTerm *elem3)
            throw(EpiBadArgument);

    /**
     * Create a list with four elements.
	 * Be carefull with this contructor, do not use code like:
	 *   new ErlConsList(new X(), new Y());
	 * that could leak if construction of Y fails. Use initElement instead.
     **/
    ErlConsList(ErlTerm *elem1, ErlTerm *elem2, ErlTerm *elem3, ErlTerm *elem4)
            throw(EpiBadArgument);

    /**
     * Add an array of elements and close with EmptyList
     * @param elems Elements to addElement
     * @param arity number of elements to add
     */
    void init(ErlTerm *elems[], unsigned int arity);

    /**
     * Add elements to the list, before the tail, using shared terms
     * @param elem a pointer to a ErlTerm
	 * @return pointer to self. So you can do something like:
	 *	list->addElement(element1)->addElement(element2);
     * @throws EpiBadArgument if the given element is invalid
     * @throws EpiAlreadyInitialized if the list is closed
     */
    ErlConsList* addElement(ErlTerm* elem)
            throw(EpiBadArgument, EpiAlreadyInitialized);

    /**
     * Set the last tail of the list. Normaly, the last tail is
     * an empty list, becoming this list a proper list.
     * @param elem a pointer to the element to use as element.
     * @throws EpiBadArgument if the given element is invalid
     * @throws EpiAlreadyInitialized if the list is closed
     */
    void close(ErlTerm* elem)
            throw(EpiBadArgument, EpiAlreadyInitialized);

    /**
     * Close the list with an empty list.
     * @throws EpiAlreadyInitialized if the list is closed
     */
    inline void close() throw(EpiAlreadyInitialized) {
        close(new ErlEmptyList());
    }

    virtual unsigned int arity() const {
        return mElementVector.size()-1;
    }

    virtual ErlTerm* elementAt(unsigned int index) const
            throw(EpiInvalidTerm, EpiBadArgument, EpiEmptyList);

    virtual ErlTerm* tail(unsigned int index) const
            throw(EpiInvalidTerm, EpiBadArgument, EpiEmptyList);

    virtual std::string toString(const VariableBinding *binding = 0) const;

    virtual ErlVariable* searchUnbound(const VariableBinding* binding);

    virtual ErlTerm* subst(const VariableBinding* binding)
            throw (EpiInvalidTerm, EpiVariableUnbound);

    IMPL_TYPE_SUPPORT2(ErlConsList, ERL_CONS_LIST, ERL_LIST);

private:
    ErlConsList(const ErlConsList &nt) {}

    inline virtual ~ErlConsList() {
        Dout(dc::erlang, "["<<this<<"] ~ErlConsList()");
    }


protected:

    // Use a vector of shared pointers to ErlTerms
    std::vector<ErlTermPtr<ErlTerm> > mElementVector;

};

} // namespace type
} // namespace epi

#endif // _ERLCONSLIST_H

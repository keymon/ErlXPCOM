/*
***** BEGIN LICENSE BLOCK *****

This file is part of the XPInterface Component.

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

#include "lfErlConsList.hpp"
#include "lfErlTerm.hpp"
#include "lfIXPInterfaceError.h"
#include "lfXPInterfaceHelper.hpp"

NS_IMPL_ISUPPORTS3(lfErlConsList, lfIErlTerm, lfIErlList, lfIErlConsList);

lfErlConsList::lfErlConsList()
{
    mTerm = new ErlConsList();
    _lfErlTerm_impl.init(mTerm.get());
}

lfErlConsList::lfErlConsList(PRUint32 size, lfIErlTerm **terms)
        throw(EpiBadArgument*)
{
    mTerm = new ErlConsList();
    _lfErlTerm_impl.init(mTerm.get());
    for (unsigned int i = 0; i<size; i++) {
        AddElement(terms[i]);
    }
    mTerm->close(new ErlEmptyList());
}

lfErlConsList::lfErlConsList(ErlConsList* aList)
{
    mTerm = aList;
    _lfErlTerm_impl.init(mTerm.get());
}

lfErlConsList::~lfErlConsList()
{
}

/* void addElement (in lfIErlTerm elem); */
NS_IMETHODIMP lfErlConsList::AddElement(lfIErlTerm *elem)
{
    try {
        ErlTerm *term;
        elem->GetErlTerm(&term);
        mTerm->addElement(term);
        return NS_OK;
    } catch (EpiBadArgument *e) {
        delete e;
        return NS_ERROR_INVALID_ARG;
    } catch (EpiAlreadyInitialized *e) {
        delete e;
        return NS_ERROR_ALREADY_INITIALIZED;
    }
}

/* void close (in lfIErlTerm elem); */
NS_IMETHODIMP lfErlConsList::Close(lfIErlTerm *elem)
{
    try {
        ErlTerm *term;
        elem->GetErlTerm(&term);
        mTerm->close(term);
        return NS_OK;
    } catch (EpiBadArgument *e) {
        delete e;
        return NS_ERROR_INVALID_ARG;
    } catch (EpiAlreadyInitialized *e) {
        delete e;
        return NS_ERROR_ALREADY_INITIALIZED;
    }
}

/* readonly attribute unsigned long arity; */
NS_IMETHODIMP lfErlConsList::GetArity(PRUint32 *aArity)
{
    XPCOM_VALUE_RETURN(mTerm->arity(), aArity);
}

/* lfIErlTerm elementAt (in unsigned long position); */
NS_IMETHODIMP lfErlConsList::ElementAt(PRUint32 position, lfIErlTerm **_retval)
{
    try {
        ErlTerm *term = mTerm->elementAt(position);
        NS_ADDREF(*_retval = lfXPInterfaceHelper::fromErlTerm(term));
    } catch (EpiInvalidTerm *e) {
        delete e;
        return NS_ERROR_NOT_INITIALIZED;
    } catch (EpiBadArgument *e) {
        delete e;
        return NS_ERROR_INVALID_ARG;
    } catch (EpiEmptyList *e) {
        delete e;
        return NS_ERROR_XPINTERFACE_EMPTYLIST;
    }
    return NS_OK;
}

/* lfIErlTerm tail (in unsigned long position); */
NS_IMETHODIMP lfErlConsList::Tail(PRUint32 position, lfIErlTerm **_retval)
{
    try {
        ErlTerm *term = mTerm->tail(position);
        NS_ADDREF(*_retval = lfXPInterfaceHelper::fromErlTerm(term));
    } catch (EpiInvalidTerm *e) {
        delete e;
        return NS_ERROR_NOT_INITIALIZED;
    } catch (EpiEmptyList *e) {
        delete e;
        return NS_ERROR_XPINTERFACE_EMPTYLIST;
    }
    return NS_OK;
}



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

#include "lfErlTuple.hpp"
#include "lfErlTerm.hpp"
#include "lfXPInterfaceHelper.hpp"

NS_IMPL_ISUPPORTS2(lfErlTuple, lfIErlTerm, lfIErlTuple);

using namespace epi::error;

lfErlTuple::lfErlTuple(PRUint32 size)
{
    mTerm = new ErlTuple(size);
    _lfErlTerm_impl.init(mTerm.get());
}

lfErlTuple::lfErlTuple(ErlTuple *aTuple)
{
    mTerm = aTuple;
    _lfErlTerm_impl.init(mTerm.get());
}

lfErlTuple::lfErlTuple(PRUint32 size, lfIErlTerm **terms)
        throw(EpiBadArgument*)
{
    mTerm = new ErlTuple(size);
    _lfErlTerm_impl.init(mTerm.get());
    for (unsigned int i = 0; i<size; i++) {
        InitElement(terms[i]);
    }
}

lfErlTuple::~lfErlTuple()
{
}


/* readonly attribute unsigned long arity; */
NS_IMETHODIMP lfErlTuple::GetArity(PRUint32 *aArity)
{
    try {
        XPCOM_VALUE_RETURN(mTerm->arity(), aArity);
    } catch (EpiInvalidTerm *e) {
        delete e;
        return NS_ERROR_NOT_INITIALIZED;
    }
}

/* void initElement (in lfIErlTerm anErlTerm); */
NS_IMETHODIMP lfErlTuple::InitElement(lfIErlTerm *anErlTerm)
{
    try {
        ErlTerm *term;
        anErlTerm->GetErlTerm(&term);
        mTerm->initElement(term);
        return NS_OK;
    } catch (EpiInvalidTerm *e) {
        delete e;
        return NS_ERROR_NOT_INITIALIZED;
    } catch (EpiBadArgument *e) {
        delete e;
        return NS_ERROR_INVALID_ARG;
    } catch (EpiAlreadyInitialized *e) {
        delete e;
        return NS_ERROR_ALREADY_INITIALIZED;
    }
}

/* lfIErlTerm elementAt (in unsigned long position); */
NS_IMETHODIMP lfErlTuple::ElementAt(PRUint32 position, lfIErlTerm **_retval)
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
    }
    return NS_OK;
}


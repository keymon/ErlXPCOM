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

#include "lfErlString.hpp"
#include "lfErlTerm.hpp"
#include "lfXPInterfaceHelper.hpp"
#include "lfIXPInterfaceError.h"

NS_IMPL_ISUPPORTS2(lfErlString, lfIErlTerm, lfIErlString);

lfErlString::lfErlString(const char *value)
{
    mTerm = new ErlString(value);
    _lfErlTerm_impl.init(mTerm.get());
}

lfErlString::lfErlString(ErlString *aString) {
    mTerm = aString;
    _lfErlTerm_impl.init(mTerm.get());
}

lfErlString::~lfErlString()
{
}

/* readonly attribute string value; */
NS_IMETHODIMP lfErlString::GetValue(char * *aValue)
{
    XPCOM_STRING_RETURN(mTerm->stringValue(), aValue);
}

/* readonly attribute unsigned long arity; */
NS_IMETHODIMP lfErlString::GetArity(PRUint32 *aArity)
{
    XPCOM_VALUE_RETURN(mTerm->arity(), aArity);
}

/* lfIErlTerm elementAt (in unsigned long position); */
NS_IMETHODIMP lfErlString::ElementAt(PRUint32 position, lfIErlTerm **_retval)
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
NS_IMETHODIMP lfErlString::Tail(PRUint32 position, lfIErlTerm **_retval)
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


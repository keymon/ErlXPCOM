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

#include "lfErlEmptyList.hpp"
#include "lfErlTerm.hpp"
#include "lfIXPInterfaceError.h"

NS_IMPL_ISUPPORTS3(lfErlEmptyList, lfIErlTerm, lfIErlList, lfIErlEmptyList);

lfErlEmptyList::lfErlEmptyList()
{
    mTerm = new ErlEmptyList();
    _lfErlTerm_impl.init(mTerm.get());
}

lfErlEmptyList::~lfErlEmptyList()
{
}

/* readonly attribute unsigned long arity; */
NS_IMETHODIMP lfErlEmptyList::GetArity(PRUint32 *aArity)
{
    XPCOM_VALUE_RETURN(0, aArity);
    return NS_OK;
}

/* lfIErlTerm elementAt (in unsigned long position); */
NS_IMETHODIMP lfErlEmptyList::ElementAt(PRUint32 position, lfIErlTerm **_retval)
{
    return NS_ERROR_XPINTERFACE_EMPTYLIST;
}

/* lfIErlTerm tail (in unsigned long position); */
NS_IMETHODIMP lfErlEmptyList::Tail(PRUint32 position, lfIErlTerm **_retval)
{
    return NS_ERROR_XPINTERFACE_EMPTYLIST;
}


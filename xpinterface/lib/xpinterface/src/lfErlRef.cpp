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

#include "lfErlRef.hpp"
#include "lfErlTerm.hpp"

NS_IMPL_ISUPPORTS2(lfErlRef, lfIErlTerm, lfIErlRef);

lfErlRef::lfErlRef(const char *node, PRUint32 ids[],
                   PRUint32 creation, const bool newStyle)
{
    mTerm = new ErlRef(node, ids, creation, newStyle);
    _lfErlTerm_impl.init(mTerm.get());
}

lfErlRef::lfErlRef(ErlRef *aRef)
{
    mTerm = aRef;
    _lfErlTerm_impl.init(mTerm.get());
}

lfErlRef::~lfErlRef()
{
}

/* readonly attribute string node; */
NS_IMETHODIMP lfErlRef::GetNode(char * *aNode)
{
    XPCOM_STRING_RETURN(mTerm->node().c_str(), aNode);
}

/* readonly attribute long id0; */
NS_IMETHODIMP lfErlRef::GetId0(PRUint32 *aId0)
{
    XPCOM_VALUE_RETURN(mTerm->id(0), aId0);
}

/* readonly attribute long id1; */
NS_IMETHODIMP lfErlRef::GetId1(PRUint32 *aId1)
{
    XPCOM_VALUE_RETURN(mTerm->id(1), aId1);
}

/* readonly attribute long id2; */
NS_IMETHODIMP lfErlRef::GetId2(PRUint32 *aId2)
{
    XPCOM_VALUE_RETURN(mTerm->id(2), aId2);
}

/* readonly attribute boolean newStyle; */
NS_IMETHODIMP lfErlRef::GetNewStyle(PRBool *aNewStyle)
{
    XPCOM_VALUE_RETURN(mTerm->isNewStyle(), aNewStyle);
}

/* readonly attribute long creation; */
NS_IMETHODIMP lfErlRef::GetCreation(PRUint32 *aCreation)
{
    XPCOM_VALUE_RETURN(mTerm->creation(), aCreation);
}


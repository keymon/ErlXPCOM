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

#include "lfErlPort.hpp"
#include "lfErlTerm.hpp"

NS_IMPL_ISUPPORTS2(lfErlPort, lfIErlTerm, lfIErlPort);

lfErlPort::lfErlPort(const char *node, PRUint32 id, PRUint32 creation)
{
    mTerm = new ErlPort(node, id, creation);
    _lfErlTerm_impl.init(mTerm.get());
}

lfErlPort::lfErlPort(ErlPort *aPort)
{
    mTerm = aPort;
    _lfErlTerm_impl.init(mTerm.get());
}

lfErlPort::~lfErlPort()
{
}

/* readonly attribute string node; */
NS_IMETHODIMP lfErlPort::GetNode(char * *aNode)
{
    XPCOM_STRING_RETURN(mTerm->node().c_str(), aNode);
}

/* readonly attribute long id; */
NS_IMETHODIMP lfErlPort::GetId(PRUint32 *aId)
{
    XPCOM_VALUE_RETURN(mTerm->id(), aId);
}

/* readonly attribute long creation; */
NS_IMETHODIMP lfErlPort::GetCreation(PRUint32 *aCreation)
{
    XPCOM_VALUE_RETURN(mTerm->creation(), aCreation);
}


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

#include "lfErlDouble.hpp"
#include "lfErlTerm.hpp"

NS_IMPL_ISUPPORTS2(lfErlDouble, lfIErlTerm, lfIErlDouble);

lfErlDouble::lfErlDouble(double value)
{
    mTerm = new ErlDouble(value);
    _lfErlTerm_impl.init(mTerm.get());
}

lfErlDouble::lfErlDouble(ErlDouble *aDouble)
{
    mTerm = aDouble;
    _lfErlTerm_impl.init(mTerm.get());
}

lfErlDouble::~lfErlDouble()
{
}

/* readonly attribute double value; */
NS_IMETHODIMP lfErlDouble::GetValue(double *aValue)
{
    XPCOM_VALUE_RETURN(mTerm->doubleValue(), aValue);
}


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

#include <iostream>
#include <sstream>
#include <string>
using namespace std;

#include "lfErlAtom.hpp"
#include "lfErlTerm.hpp"
#include "xpcom_utils.hpp"

NS_IMPL_ISUPPORTS2(lfErlAtom, lfIErlTerm, lfIErlAtom);

/**
 * Construct from a string.
 */
lfErlAtom::lfErlAtom(const char *aAtomValue)
        throw (EpiBadArgument)
{
    mTerm = new ErlAtom(aAtomValue);
    _lfErlTerm_impl.init(mTerm.get());
}

lfErlAtom::lfErlAtom(ErlAtom *aAtom) {
    mTerm = aAtom;
    _lfErlTerm_impl.init(mTerm.get());
}

lfErlAtom::~lfErlAtom()
{
}

/* readonly attribute string atom; */
NS_IMETHODIMP lfErlAtom::GetValue(char * *aAtom)
{
    XPCOM_STRING_RETURN(mTerm->atomValue(), aAtom);
}


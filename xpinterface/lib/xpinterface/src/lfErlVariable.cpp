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

#include "lfErlVariable.hpp"

NS_IMPL_ISUPPORTS2(lfErlVariable, lfIErlTerm, lfIErlVariable);

/**
 * Construct from a string.
 */
lfErlVariable::lfErlVariable(const char *aVariableName)
{
    mTerm = new ErlVariable(aVariableName);
    _lfErlTerm_impl.init(mTerm.get());
}

lfErlVariable::lfErlVariable(ErlVariable *aVariable) {
    mTerm = aVariable;
    _lfErlTerm_impl.init(mTerm.get());
}

lfErlVariable::~lfErlVariable()
{
  /* destructor code */
}

/* readonly attribute string name; */
NS_IMETHODIMP lfErlVariable::GetName(char * *aName)
{
    XPCOM_STRING_RETURN(mTerm->getName().c_str(), aName);
}


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

#ifndef _LFERLSTRING_H
#define _LFERLSTRING_H

#include <epi.hpp>

#include "lfIErlString.h"
#include "lfErlTerm.hpp"

using namespace epi::type;

class lfErlString : public lfIErlString
{
public:
    NS_DECL_ISUPPORTS;
    NS_DECL_LFIERLLIST;
    NS_DECL_LFIERLSTRING;

    // Methods from lfIErlTerm
    NS_FORWARD_LFIERLTERM(_lfErlTerm_impl.);

    lfErlString(const char* value);
    lfErlString(ErlString *aString);

private:
    virtual ~lfErlString();

protected:
    /* additional members */

    // The internal representation of the term
    ErlTermPtr<ErlString> mTerm;

    // Implementation of lfErlTerm methods
    lfErlTerm_Impl _lfErlTerm_impl;
};

#endif

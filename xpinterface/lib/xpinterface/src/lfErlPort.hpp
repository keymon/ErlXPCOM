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

#ifndef _LFERLPORT_H
#define _LFERLPORT_H

#include <epi.hpp>

#include "lfIErlPort.h"
#include "lfErlTerm.hpp"

using namespace epi::type;

class lfErlPort : public lfIErlPort
{
public:
    NS_DECL_ISUPPORTS;
    NS_DECL_LFIERLPORT;

    // Methods from lfIErlTerm
    NS_FORWARD_LFIERLTERM(_lfErlTerm_impl.);

    lfErlPort(const char *node, PRUint32 id, PRUint32 creation);
    lfErlPort(ErlPort *aPort);

private:
    virtual ~lfErlPort();

protected:
    /* additional members */

    // The internal representation of the term
    ErlTermPtr<ErlPort> mTerm;

    // Implementation of lfErlTerm methods
    lfErlTerm_Impl _lfErlTerm_impl;

};

#endif

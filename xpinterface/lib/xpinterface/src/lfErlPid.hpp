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

#ifndef _LFERLPID_H
#define _LFERLPID_H

#include <epi.hpp>
using namespace epi::type;

#include "lfIErlPid.h"
#include "lfErlTerm.hpp"

class lfErlPid : public lfIErlPid
{
public:
    NS_DECL_ISUPPORTS;
    NS_DECL_LFIERLPID;

    // Methods from lfIErlTerm
    NS_FORWARD_LFIERLTERM(_lfErlTerm_impl.);

    /**
     * Construct from basic native values
     */
    lfErlPid(const char *node, PRUint32 id, PRUint32 serial, PRUint32 creation);

    /**
	 * Construct from a ErlTerm.
	 * Data IS NOT copied, but is freed in the destructor
     */
    lfErlPid(ErlPid *aPid);

private:
    virtual ~lfErlPid();

	// The ETERM value
	ErlTermPtr<ErlPid> mTerm;

     // Implementation of lfErlTerm methods
     lfErlTerm_Impl _lfErlTerm_impl;

protected:
    /* additional members */
};

#endif

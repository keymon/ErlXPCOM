/*
***** BEGIN LICENSE BLOCK *****

This file is part of the erlXPCOM (Erlang XPCOM binding) project.

Copyright (C) 2005 Hector Rivas Gandara <keymon@gmail.com>

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published replyby the Free Software Foundation; either
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
#ifndef __ERLXPCOMREMOTEMETHOD_H
#define __ERLXPCOMREMOTEMETHOD_H

#include "ErlXPCOMMethod.h"

namespace erlxpcom {

/**
 * This class containts the data necesary to call to a remote method. 
 * The class will contain the nsXTPCMiniVariant array, that will be read
 * with the marshaller visitor and populated with reply with the unmarshaller
 * visitor.
 */
class ErlXPCOMRemoteMethod: public ErlXPCOMMethod {
public:

	ErlXPCOMRemoteMethod(ErlXPCOMMethodInfo* _methodInfo, 
	                     nsXPTCMiniVariant* _variants);

	virtual ~ErlXPCOMRemoteMethod();

	virtual nsXPTCMiniVariant* getMiniVariant(int paramNumber);
	
	/**
	 * Return the variants array
	 */
	inline nsXPTCMiniVariant* getMiniVariants() {
		return variants;
	}

private:
	
	nsXPTCMiniVariant* variants;
		
};

}
#endif // __ERLXPCOMREMOTEMETHOD_H

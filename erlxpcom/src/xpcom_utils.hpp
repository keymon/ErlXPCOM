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

/**
 * This file defines some useful macros for XPCOM use.
 */
#ifndef _XPCOM_UTILS_H
#define _XPCOM_UTILS_H

#include "nsMemory.h"
#include "nscore.h"
#include "string.h"

/**
 * Clone an string to be returned by an out parameter using the nsMemory
 * class (see http://www.mozilla.org/scriptable/faq.html#i9).
 * _from must be a (const char *)
 * _retval must be a (char **)
 */
#define XPCOM_STRING_CLONE(_from, _retval) \
    *_retval = (char*) nsMemory::Clone((_from), \
 	    sizeof(char)*(strlen(_from)+1))

/**
 * Implements a method that returns a string. this clones the string
 * using nsMemory, and checking for erros returning the appropiated value
 * _from must be a (const char *)
 * _retval must be a (char **)
 */
#define XPCOM_STRING_RETURN(_from, _retval) \
	 if(!_retval) return NS_ERROR_NULL_POINTER;\
        XPCOM_STRING_CLONE(_from, _retval);\
	 return *_retval ? NS_OK : NS_ERROR_OUT_OF_MEMORY

/**
 * Implements a method that returns an simple type (int, float, etc...)
 * checking for erros and returning the appropiated value
 * _from must be a (simple_type)
 * _retval must be a (simple_type *)
 */
#define XPCOM_VALUE_RETURN(_from, _retval) \
	 if(!_retval) return NS_ERROR_NULL_POINTER;\
    *_retval=_from;\
	 return NS_OK

/**
 * Operator < for nsIID
 */
inline bool operator<(const nsIID &t1, const nsIID &t2) {
	return memcmp(&t1, &t2, sizeof(nsIID));
}



#endif

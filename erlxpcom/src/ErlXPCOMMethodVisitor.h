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
#ifndef __ERLXPCOMMETHODVISITOR_H
#define __ERLXPCOMMETHODVISITOR_H

#include <xptcall.h>
#include <xptinfo.h>
#include <prtypes.h>

#include "ErlXPCOMMethod.h"
#include "ErlXPCOMException.h"
#include "ErlXPCOMLog.h"

namespace erlxpcom {
/**
 * This is the interface that must implement a visitor of an ErlXPCOMMethod.
 * There is a callback for each XPCOM type. 
 * Each method will get all the information necesary for the given param in  
 * the ErlXPCOMParam class. 
 */
class ErlXPCOMMethodVisitor {
public:
	/** 
	 * Visit a param. 
     * @param param Pointer to the param. Can't be null
	 * @param paramDep1 Pointer to depend param 1. NULL if type has no dependencies
	 * @param paramDep2
	 */
	void do_visit(ErlXPCOMParam* param, 
				  ErlXPCOMParam* paramDep1, 
				  ErlXPCOMParam* paramDep2) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_integer_8(ErlXPCOMParam &param) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_integer_16(ErlXPCOMParam &param) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_integer_32(ErlXPCOMParam &param) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_integer_64(ErlXPCOMParam &param) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_unsigned_integer_8(ErlXPCOMParam &param) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_unsigned_integer_16(ErlXPCOMParam &param) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_unsigned_integer_32(ErlXPCOMParam &param) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_unsigned_integer_64(ErlXPCOMParam &param) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_float(ErlXPCOMParam &param) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_double(ErlXPCOMParam &param) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_bool(ErlXPCOMParam &param) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_char(ErlXPCOMParam &param) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_wchar(ErlXPCOMParam &param) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_void(ErlXPCOMParam &param) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_iid(ErlXPCOMParam &param) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_dom_str(ErlXPCOMParam &param) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_char_str(ErlXPCOMParam &param) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_wchar_str(ErlXPCOMParam &param) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_char_str_size_is(ErlXPCOMParam &param,
									ErlXPCOMParam &paramDep1,
									ErlXPCOMParam &paramDep2) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_wchar_str_size_is(ErlXPCOMParam &param,
									 ErlXPCOMParam &paramDep1,
									 ErlXPCOMParam &paramDep2) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_utf8_str(ErlXPCOMParam &param) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_cstring_str(ErlXPCOMParam &param) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_astring_str(ErlXPCOMParam &param) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_interface(ErlXPCOMParam &param) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_interface_is(ErlXPCOMParam &param,
								ErlXPCOMParam &paramDep) 
	throw (InterfaceMismatchException, InternalErrorException);
	virtual void t_array(ErlXPCOMParam &param,
						 ErlXPCOMParam &paramDep) 
	throw (InterfaceMismatchException, InternalErrorException);
	
	/**
	 * Default callback for undefined types
	 */
	virtual void t_default(ErlXPCOMParam *param,
						   ErlXPCOMParam *paramDep1, 
						   ErlXPCOMParam *paramDep2, 
						   void *otherData)
	throw (InterfaceMismatchException, InternalErrorException);
protected:

	
};


}
#endif // __ERLXPCOMMETHODVISITOR_H


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
#ifndef __ERLXPCOMMETHODUNMARSHALLER_H
#define __ERLXPCOMMETHODUNMARSHALLER_H

#include <epi.hpp>

#include "ErlXPCOMLog.h"
#include "ErlangOrb.h"

#include "ErlXPCOMMethodVisitor.h"

namespace erlxpcom {

using namespace epi::type;
using namespace epi::error;

/**
 * This class implements an ErlXPCOMMethodVisitor that will process the 
 * ErlXPCOMMethod params setting the miniVariants from the values contained in
 * an ErlList.
 * It will unmarshall the parameters.
 */
class ErlXPCOMMethodUnmarshaller: public ErlXPCOMMethodVisitor {
public:
	/**
	 * Default constructor.
	 * @param orb
	 * @param processingIn flag that indicates if the UnMarshaller will unmarshall
	 *		in params (true) or out params (false)
	 * @param erlangParams ErlList with the list of params to unmashall
	 */ 
	ErlXPCOMMethodUnmarshaller(ErlangORB* orb, bool processingIn, 
		ErlList *erlangParams);

	~ErlXPCOMMethodUnmarshaller();
	
private:
	ErlTerm* getTermForParam(ErlXPCOMParam &param) 
		throw (InterfaceMismatchException, InternalErrorException);

	/** Common implementation for t_interface and t_interface_is */
	void t_interface_common(ErlXPCOMParam &param, nsIID &iid) 	
		throw (InterfaceMismatchException, InternalErrorException);

	/** Common implementation for t_char_str, t_wchar_str, 
		t_char_str_size_is and t_wchar_str_size_is.
		Returns string length */
	int t_char_str_common(ErlXPCOMParam &param) 
		throw (InterfaceMismatchException, InternalErrorException);

	/** Common implementation for t_utf8, t_cstring */
	void t_cstring_common(ErlXPCOMParam &param) 	
		throw (InterfaceMismatchException, InternalErrorException);

	/** Common implementation for t_dom_str, t_astring */
	void t_astring_common(ErlXPCOMParam &param) 	
		throw (InterfaceMismatchException, InternalErrorException);

	
 	PRLogModuleInfo *log;

	ErlangORB* orb;

	// This flag indicates if the marshaller is processing in params or out params
	bool processingIn;

	// This flag indicates if the marshaller is processing elements of an array
	bool processingArray;

	ErlTermPtr<ErlList> erlangParams;
	// if processing an array
	ErlTermPtr<ErlList> erlangArray;

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
public:
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
	
	
};

}
#endif //__ERLXPCOMMETHODUNMARSHALLER_H

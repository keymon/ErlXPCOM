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
#ifndef __ERLXPCOMMETHODMARSHALLER_H
#define __ERLXPCOMMETHODMARSHALLER_H

#include <epi.hpp>

#include "ErlXPCOMLog.h"
#include "ErlangOrb.h"
#include "ErlXPCOMMethodVisitor.h"

namespace erlxpcom {

using namespace epi::type;
using namespace epi::error;

/**
 * This class implements an ErlXPCOMMethodVisitor that will process the 
 * ErlXPCOMMethod params building an ErlList with the erlang representation.
 * It will marshall the parameters.
 */
class ErlXPCOMMethodMarshaller: public ErlXPCOMMethodVisitor {
public:
	/**
	 * Default constructor.
	 * @param orb
	 * @param processingIn flag that indicates if the Marshaller will marshall
	 *		in params (true) or out params (false)
	 * @param outParamsNumber the number of in or out params expected without
	 *	the length and size params
	 */ 
	ErlXPCOMMethodMarshaller(ErlangORB* orb, bool processingIn, int paramsNumber);

	~ErlXPCOMMethodMarshaller();
	
	/**
	 * Get the params result of the marshalling
	 */
	ErlList* getParams() 
		throw (InterfaceMismatchException, InternalErrorException);


private:
	/**
	 * Set an erlang param
	 */
	void setParam(ErlXPCOMParam &param, ErlTerm* term);

	/**
	 * Gets the erlang representation of and nsISupports pointer. 
	 * This method will check if the object is an existing stub, an proxy and
	 * if not, will create and register a new stub for it.
	 */
	ErlTerm* XPCOMtoErlangObject(nsISupports* obj, nsIID &iid);	
	

 	PRLogModuleInfo *log;

	ErlangORB* orb;

	// This flag indicates if the marshaller is processing in params or out params
	bool processingIn;


	int paramsNumber;
	int paramsCount;
	
	// array where marshalled params will be stored 
	ErlTermPtr<> *erlangParams;

	// This flag indicates if the marshaller is processing an array
	bool processingArray;
	
	// pointer where marshalled elements of an array will be stored
	ErlTermPtr<> erlangArrayElement;

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
#endif //__ERLXPCOMMETHODMARSHALLER_H

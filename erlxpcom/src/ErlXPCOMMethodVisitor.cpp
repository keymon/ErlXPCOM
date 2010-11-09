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
#include <sstream>
#include "ErlXPCOMMethodVisitor.h"

using namespace erlxpcom;

void ErlXPCOMMethodVisitor::do_visit(ErlXPCOMParam* param, 
									 ErlXPCOMParam* paramDep1, 
									 ErlXPCOMParam* paramDep2) 
throw (InterfaceMismatchException, InternalErrorException)
{
	switch (param->tag) {
        case nsXPTType::T_I8:
			t_integer_8(*param);
			break;
		case nsXPTType::T_I16:            
			t_integer_16(*param);
			break;
		case nsXPTType::T_I32:         
			t_integer_32(*param);
			break;
        case nsXPTType::T_I64:
			t_integer_64(*param);
			break;
		case nsXPTType::T_U8:
			t_unsigned_integer_8(*param);
			break;
		case nsXPTType::T_U16:
			t_unsigned_integer_16(*param);
			break;
        case nsXPTType::T_U32:
			t_unsigned_integer_32(*param);
			break;
        case nsXPTType::T_U64:
			t_unsigned_integer_64(*param);
			break;
        case nsXPTType::T_FLOAT:
			t_float(*param);
			break;
        case nsXPTType::T_DOUBLE:
			t_double(*param);
			break;
        case nsXPTType::T_BOOL:
			t_bool(*param);
			break;
        case nsXPTType::T_CHAR:  /* fall through */
			t_char(*param);
			break;
        case nsXPTType::T_WCHAR:
			t_wchar(*param);
			break;
        case nsXPTType::T_CSTRING:
			t_cstring_str(*param);
			break;
        case nsXPTType::T_CHAR_STR:
			t_char_str(*param);
			break;
        case nsXPTType::T_PSTRING_SIZE_IS: // This will be treated like cstring.
			t_char_str_size_is(*param,
									   *paramDep1, 
									   *paramDep2);
			break;
		case nsXPTType::T_WCHAR_STR:         
			t_wchar_str(*param);
			break;
        case nsXPTType::T_PWSTRING_SIZE_IS:
			t_wchar_str_size_is(*param,
									    *paramDep1, 
									    *paramDep2);
			break;
        case nsXPTType::T_INTERFACE:
			t_interface(*param);
			break;
        case nsXPTType::T_INTERFACE_IS:   
			t_interface_is(*param, *paramDep1);
			break;
        case nsXPTType::T_IID:            
			t_iid(*param);
			break;
        case nsXPTType::T_VOID:              
			t_void(*param);
			break;
        case nsXPTType::T_DOMSTRING:         
			t_dom_str(*param);
			break;
        case nsXPTType::T_UTF8STRING:        
			t_utf8_str(*param);
			break;
        case nsXPTType::T_ASTRING:        
			t_astring_str(*param);
			break;
        case nsXPTType::T_ARRAY:             
			t_array(*param, *paramDep1);
			break;

        default:                             
			PR_LOG( ErlXPCOMLog::getLog(), PR_LOG_ERROR, 
				(" `-> parameter %i unimplemented type %hi", 
					 param->number, 
					 param->tag));
			std::ostringstream oss;
	
			oss << "Parameter " << param->number << " unimplemented type";
			throw InterfaceMismatchException(oss.str());
	}
}

void ErlXPCOMMethodVisitor::t_integer_8(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, NULL, NULL, NULL);
}
void ErlXPCOMMethodVisitor::t_integer_16(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, NULL, NULL, NULL);
}
void ErlXPCOMMethodVisitor::t_integer_32(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, NULL, NULL, NULL);
}
void ErlXPCOMMethodVisitor::t_integer_64(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, NULL, NULL, NULL);
}
void ErlXPCOMMethodVisitor::t_unsigned_integer_8(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, NULL, NULL, NULL);
}
void ErlXPCOMMethodVisitor::t_unsigned_integer_16(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, NULL, NULL, NULL);
}
void ErlXPCOMMethodVisitor::t_unsigned_integer_32(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, NULL, NULL, NULL);
}
void ErlXPCOMMethodVisitor::t_unsigned_integer_64(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, NULL, NULL, NULL);
}
void ErlXPCOMMethodVisitor::t_float(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, NULL, NULL, NULL);
}
void ErlXPCOMMethodVisitor::t_double(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, NULL, NULL, NULL);
}
void ErlXPCOMMethodVisitor::t_bool(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, NULL, NULL, NULL);
}
void ErlXPCOMMethodVisitor::t_char(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, NULL, NULL, NULL);
}
void ErlXPCOMMethodVisitor::t_wchar(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, NULL, NULL, NULL);
}
void ErlXPCOMMethodVisitor::t_void(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, NULL, NULL, NULL);
}
void ErlXPCOMMethodVisitor::t_iid(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, NULL, NULL, NULL);
}
void ErlXPCOMMethodVisitor::t_dom_str(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, NULL, NULL, NULL);
}
void ErlXPCOMMethodVisitor::t_char_str(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, NULL, NULL, NULL);
}
void ErlXPCOMMethodVisitor::t_wchar_str(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, NULL, NULL, NULL);
}
void ErlXPCOMMethodVisitor::t_char_str_size_is(ErlXPCOMParam &param,
								ErlXPCOMParam &paramDep1,
								ErlXPCOMParam &paramDep2) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, &paramDep1, &paramDep2, NULL);
}
void ErlXPCOMMethodVisitor::t_wchar_str_size_is(ErlXPCOMParam &param,
								 ErlXPCOMParam &paramDep1,
								 ErlXPCOMParam &paramDep2) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, &paramDep1, &paramDep2, NULL);
}
void ErlXPCOMMethodVisitor::t_utf8_str(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, NULL, NULL, NULL);
}
void ErlXPCOMMethodVisitor::t_cstring_str(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, NULL, NULL, NULL);
}
void ErlXPCOMMethodVisitor::t_astring_str(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, NULL, NULL, NULL);
}
void ErlXPCOMMethodVisitor::t_interface(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, NULL, NULL, NULL);
}
void ErlXPCOMMethodVisitor::t_interface_is(ErlXPCOMParam &param,
							ErlXPCOMParam &paramDep) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, &paramDep, NULL, NULL);
}
void ErlXPCOMMethodVisitor::t_array(ErlXPCOMParam &param,
					 ErlXPCOMParam &paramDep) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_default(&param, &paramDep, NULL, NULL);
}

/**
 * Default callback for undefined types
 */
void ErlXPCOMMethodVisitor::t_default(ErlXPCOMParam *param,
					   ErlXPCOMParam *paramDep1, 
					   ErlXPCOMParam *paramDep2, 
					   void *otherData)
throw (InterfaceMismatchException, InternalErrorException) 
{
	std::ostringstream oss;	
	PR_LOG(ErlXPCOMLog::getLog(), PR_LOG_ERROR, 
		("-- unknown parameter type (%i) in parameter %i", 
			param->paramInfo.GetType().TagPart(), param->number));
	oss << "Parameter " << param->number << " unknown type";
	throw InterfaceMismatchException(oss.str());
}


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

#include <nsString.h>

#include "ErlXPCOMMethodUnmarshaller.h"


using namespace erlxpcom;

ErlXPCOMMethodUnmarshaller::ErlXPCOMMethodUnmarshaller(
	ErlangORB* _orb, bool _processingIn, ErlList *_erlangParams): 
	log(ErlXPCOMLog::getLog()), 
	orb(_orb),
	processingIn(_processingIn),
	processingArray(0),
	erlangParams(_erlangParams),
	erlangArray(0)
{
}

ErlXPCOMMethodUnmarshaller::~ErlXPCOMMethodUnmarshaller() {
}

ErlTerm* ErlXPCOMMethodUnmarshaller::getTermForParam(ErlXPCOMParam &param) 
	throw (InterfaceMismatchException, InternalErrorException)
{
	int paramNumber = processingArray? 
		param.arrayElementNumber: 
			processingIn? 
				param.inNumberNoSize: param.outNumberNoSize;
	try {
		if (!processingArray) {
			ErlTerm *term = erlangParams->elementAt(paramNumber);
			return term;
		} else {
			ErlTerm *term = erlangArray->elementAt(paramNumber);
			return term;
		}
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_ERROR, 
			("ErlXPCOMMethodUnmarshaller: "
			 "trying to access to unexistent param %i in param list", 
			 paramNumber));
		throw InterfaceMismatchException("Bad number of arguments");
	} catch (EpiEmptyList &e) {
		PR_LOG( log, PR_LOG_ERROR, 
			("ErlXPCOMMethodUnmarshaller: "
			 "trying to access to unexistent param %i in param list", 
			 paramNumber));
		throw InterfaceMismatchException("Bad number of arguments");
	} catch (EpiInvalidTerm &e) {
		// The list is an invalid term!!
		PR_LOG( log, PR_LOG_ERROR, 
			("ErlXPCOMMethodUnmarshaller: "
			 "parameter list is an invalid term", 
			 paramNumber));
		throw ErlangException("Parameter list invalid", e);
	}
}

void ErlXPCOMMethodUnmarshaller::t_interface_common(ErlXPCOMParam &param, nsIID &iid) 	
	throw (InterfaceMismatchException, InternalErrorException)
{
	PRLogModuleInfo *log = ErlXPCOMLog::getLog();

	std::ostringstream oss;

	ErlTermPtr<ErlTuple> xpcomObjPattern(
		new ErlTuple(new ErlAtom("xpcom_oid"), new ErlVariable("OID")));
	ErlTermPtr<ErlTuple> erlangObjPattern(
		new ErlTuple(new ErlAtom("erlang_oid"), new ErlVariable("OID")));
	ErlTermPtr<ErlTerm> nullObjPattern(new ErlAtom("null"));

	ErlTerm *term = getTermForParam(param);
	int number = processingIn? param.inNumberNoSize: param.outNumberNoSize;

	VariableBinding binding;
	
	// Check if it is a XPCOM side object or an erlang side object 
	if (term->match(xpcomObjPattern.get(), &binding)) {
		lfOID oid;
		// get the oid
		try {
			ErlLong *along = ErlLong::cast(binding.search("OID"));
			oid = ErlXPCOMHelper::OIDFromErlang(along);
		} catch (EpiBadArgument &e) {
			PR_LOG( log, PR_LOG_DEBUG, 
				("-- Incorrect format in param %i, is not object descriptor", number));
			oss << "Incorrect format for param " << number;
			throw InterfaceMismatchException(oss.str());
		}
		// Get the stub. It MUST be registered, Erlang can't known it otherwise
		ErlXPCOMStub* stub = orb->getStub(oid);
		
		param.miniVariant->val.p = stub->getXPCOMObject();
	} else if (term->match(erlangObjPattern.get(), &binding)) {
		lfOID oid;
		ErlXPCOMProxy *proxy = 0;
		// get the oid
		try {
			ErlLong *along = ErlLong::cast(binding.search("OID"));
			oid = ErlXPCOMHelper::OIDFromErlang(along);
		} catch (EpiBadArgument &e) {
			PR_LOG( log, PR_LOG_DEBUG, 
				("-- Incorrect format in param %i, is not object descriptor", number));
			oss << "Incorrect format for param " << number;
			throw InterfaceMismatchException(oss.str());
		}
		// Search if the proxy exists
		proxy = orb->searchProxy(oid, iid);
		if (proxy) {
			// If exists, use it
			param.miniVariant->val.p = proxy;
		} else {
			// Else create and register a new one
			proxy = new ErlXPCOMProxy(oid, iid);
			PR_LOG( log, PR_LOG_DEBUG, 
				("-- registering new proxy %s", proxy->toString().c_str()));

			orb->registerProxy(proxy);
			param.miniVariant->val.p = proxy;
		}
		// WARNING allways addref the returned proxy
		NS_ADDREF(proxy);
	} else if (term->match(nullObjPattern.get())) {
		param.miniVariant->val.p = nsnull;
	} else {
		PR_LOG( log, PR_LOG_ERROR, 
			("-- parameter %i bad type, is not an interface", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}

}

int ErlXPCOMMethodUnmarshaller::t_char_str_common(ErlXPCOMParam &param) 
	throw (InterfaceMismatchException, InternalErrorException) 
{
	std::ostringstream oss;	
	
	ErlTerm *term = getTermForParam(param);
	int number = processingIn? param.inNumberNoSize: param.outNumberNoSize;

	// A simple string, extract copy it directly
	if (term->instanceOf(ERL_STRING)) {
		PR_LOG( log, PR_LOG_DEBUG, ("-- CString, cloning ErlString"));
		ErlString *_t = ErlString::cast(term);
		param.miniVariant->val.p = (void*) 
			nsMemory::Clone(_t->stringValue(), _t->arity()+1);
		return _t->arity();
	// List representation of a string 
	} else if (term->instanceOf(ERL_LIST)) {
		PR_LOG( log, PR_LOG_DEBUG, ("-- CString, cloning ErlList content"));
		ErlList *_t = (ErlList *) term;
		int strsize = _t->arity();
		char *newstr = (char*) nsMemory::Alloc((strsize+1)*sizeof(char));
		try {
			for (int i = 0; i< strsize; i++) {
				// Try to get each element 
				ErlLong *element = ErlLong::cast(_t->elementAt(i));
				// FIXME: check ranges
				newstr[i] = element->longValue();
			}
			newstr[strsize] = 0;
			param.miniVariant->val.p = (void*) newstr;
			return strsize;
		} catch (EpiBadArgument &e) {
			PR_LOG( log, PR_LOG_ERROR, 
				("-- parameter %i bad type, is not an string, an element of list is not an integer (cstring)", number));
			oss << "Parameter " << number << " incorrect type";
			throw InterfaceMismatchException(oss.str());
		}
	} else {
		PR_LOG( log, PR_LOG_ERROR, 
			("-- parameter %i bad type, is not an string (cstring)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}

void ErlXPCOMMethodUnmarshaller::t_cstring_common(ErlXPCOMParam &param) 	
	throw (InterfaceMismatchException, InternalErrorException) 
{
	std::ostringstream oss;	

	ErlTerm *term = getTermForParam(param);
	int number = processingIn? param.inNumberNoSize: param.outNumberNoSize;
	
	// Get the container 
	if (param.miniVariant->val.p == NULL) {
		param.miniVariant->val.p = new nsCString();
	}
	nsACString* acString = (nsACString*) param.miniVariant->val.p;
	
	if (term->instanceOf(ERL_STRING)) {
		PR_LOG( log, PR_LOG_DEBUG, ("-- astring, cloning ErlString"));
		ErlString *_t = (ErlString *) term;
		acString->Assign(_t->stringValue());
	// List representation of a string 
	} else if (term->instanceOf(ERL_LIST)) {
		PR_LOG( log, PR_LOG_DEBUG, ("-- wstring, cloning ErlList content"));
		ErlList *_t = (ErlList *) term;
		int strsize = _t->arity();
		PRUnichar* newstr = new PRUnichar[strsize+1];
		try {
			for (int i = 0; i< strsize; i++) {
				// Try to get each element 
				ErlLong *element = ErlLong::cast(_t->elementAt(i));
				// FIXME: check ranges
				newstr[i] = element->longValue();
			}
			newstr[strsize] = 0;
			acString->Assign(nsDependentCString((char*)newstr, strsize));
			delete[] newstr;
		} catch (EpiBadArgument &e) {
			delete[] newstr;
			PR_LOG( log, PR_LOG_DEBUG, 
				("-- parameter %i bad type, is not an string, an element of list is not an integer (cstring)", number));
			oss << "Parameter " << number << " incorrect type";
			throw InterfaceMismatchException(oss.str());
		}
	} else {
		PR_LOG( log, PR_LOG_DEBUG, 
			("-- parameter %i bad type, is not an string (wstring)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}
	

void ErlXPCOMMethodUnmarshaller::t_astring_common(ErlXPCOMParam &param) 	
	throw (InterfaceMismatchException, InternalErrorException)
{
	std::ostringstream oss;	

	ErlTerm *term = getTermForParam(param);
	int number = processingIn? param.inNumberNoSize: param.outNumberNoSize;
	
	// Get the container 
	nsAString* aString = (nsAString*) param.miniVariant->val.p;
	
	if (term->instanceOf(ERL_STRING)) {
		PR_LOG( log, PR_LOG_DEBUG, ("-- astring, cloning ErlString"));
		ErlString *_t = (ErlString *) term;
		aString->Assign(nsDependentString((PRUnichar*) _t->stringValue(), _t->arity()));
	// List representation of a string 
	} else if (term->instanceOf(ERL_LIST)) {
		PR_LOG( log, PR_LOG_DEBUG, ("-- wstring, cloning ErlList content"));
		ErlList *_t = (ErlList *) term;
		int strsize = _t->arity();
		PRUnichar* newstr = new PRUnichar[strsize+1];
		try {
			for (int i = 0; i< strsize; i++) {
				// Try to get each element 
				ErlLong *element = ErlLong::cast(_t->elementAt(i));
				// FIXME: check ranges
				newstr[i] = element->longValue();
			}
			newstr[strsize] = 0;
			aString->Assign(newstr);
			delete[] newstr;
		} catch (EpiBadArgument &e) {
			delete[] newstr;
			PR_LOG( log, PR_LOG_DEBUG, 
				("-- parameter %i bad type, is not an string, an element of list is not an integer (cstring)", number));
			oss << "Parameter " << number << " incorrect type";
			throw InterfaceMismatchException(oss.str());
		}
	} else {
		PR_LOG( log, PR_LOG_DEBUG, 
			("-- parameter %i bad type, is not an string (wstring)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Visitor callbacks |
//~~~~~~~~~~~~~~~~~~~'

void ErlXPCOMMethodUnmarshaller::t_integer_8(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	std::ostringstream oss;	
	ErlTerm *term = getTermForParam(param);
	int number = processingIn? param.inNumberNoSize: param.outNumberNoSize;
	try {
		// FIXME: Check ranges
		ErlLong *_t = ErlLong::cast(term);
		param.miniVariant->val.i8 = _t->longValue();
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_ERROR, 
				("-- parameter %i bad type, is not a integer (i8)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}
void ErlXPCOMMethodUnmarshaller::t_integer_16(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	std::ostringstream oss;	
	ErlTerm *term = getTermForParam(param);
	int number = processingIn? param.inNumberNoSize: param.outNumberNoSize;
	try {
		// FIXME: Check ranges
		ErlLong *_t = ErlLong::cast(term);
		param.miniVariant->val.i16 = _t->longValue();
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_ERROR, 
			("-- parameter %i bad type, is not a integer (i16)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}
void ErlXPCOMMethodUnmarshaller::t_integer_32(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	std::ostringstream oss;	
	ErlTerm *term = getTermForParam(param);
	int number = processingIn? param.inNumberNoSize: param.outNumberNoSize;
	try {
		// FIXME: Check ranges
		ErlLong *_t = ErlLong::cast(term);
		param.miniVariant->val.i32 = _t->longValue();
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_ERROR, 
			("-- parameter %i bad type, is not a integer (i32)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}
void ErlXPCOMMethodUnmarshaller::t_integer_64(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	std::ostringstream oss;	
	ErlTerm *term = getTermForParam(param);
	int number = processingIn? param.inNumberNoSize: param.outNumberNoSize;
	try {
		// FIXME: Check ranges. 
		// Not really implemented, 64 bits not supported
		ErlLong *_t = ErlLong::cast(term);
		param.miniVariant->val.i64 = _t->longValue();
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_ERROR, 
			("-- parameter %i bad type, is not a integer (i64)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}
void ErlXPCOMMethodUnmarshaller::t_unsigned_integer_8(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	std::ostringstream oss;	
	ErlTerm *term = getTermForParam(param);
	int number = processingIn? param.inNumberNoSize: param.outNumberNoSize;
	try {
		// FIXME: Check ranges. 
		ErlLong *_t = ErlLong::cast(term);
		param.miniVariant->val.u8 = _t->longValue();
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_ERROR, 
			("-- parameter %i bad type, is not a integer (u8)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}
void ErlXPCOMMethodUnmarshaller::t_unsigned_integer_16(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	std::ostringstream oss;	
	ErlTerm *term = getTermForParam(param);
	int number = processingIn? param.inNumberNoSize: param.outNumberNoSize;
	try {
		// FIXME: Check ranges. 
		ErlLong *_t = ErlLong::cast(term);
		param.miniVariant->val.u16 = _t->longValue();
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_ERROR, 
			("-- parameter %i bad type, is not a integer (u16)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}
void ErlXPCOMMethodUnmarshaller::t_unsigned_integer_32(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	std::ostringstream oss;	
	ErlTerm *term = getTermForParam(param);
	int number = processingIn? param.inNumberNoSize: param.outNumberNoSize;
	try {
		// FIXME: Check ranges. 
		ErlLong *_t = ErlLong::cast(term);
		param.miniVariant->val.u32 = _t->longValue();
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_ERROR, 
			("-- parameter %i bad type, is not a integer (u32)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}
void ErlXPCOMMethodUnmarshaller::t_unsigned_integer_64(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	std::ostringstream oss;	
	ErlTerm *term = getTermForParam(param);
	int number = processingIn? param.inNumberNoSize: param.outNumberNoSize;
	try {
		// FIXME: Check ranges. 
		// Not really implemented, 64 bits not supported
		ErlLong *_t = ErlLong::cast(term);
		param.miniVariant->val.u64 = _t->longValue();
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_ERROR, 
			("-- parameter %i bad type, is not a integer (u64)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}
void ErlXPCOMMethodUnmarshaller::t_float(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	std::ostringstream oss;	
	ErlTerm *term = getTermForParam(param);
	int number = processingIn? param.inNumberNoSize: param.outNumberNoSize;

	try {
		ErlDouble *_t = ErlDouble::cast(term);
		param.miniVariant->val.f = _t->doubleValue();
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_ERROR, 
			("-- parameter %i bad type, is not a double (float)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}
void ErlXPCOMMethodUnmarshaller::t_double(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	std::ostringstream oss;	
	ErlTerm *term = getTermForParam(param);
	int number = processingIn? param.inNumberNoSize: param.outNumberNoSize;

	try {
		ErlDouble *_t = ErlDouble::cast(term);
		param.miniVariant->val.d = _t->doubleValue();
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_ERROR, 
			("-- parameter %i bad type, is not a double (double)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}
void ErlXPCOMMethodUnmarshaller::t_bool(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	std::ostringstream oss;	
	ErlTerm *term = getTermForParam(param);
	int number = processingIn? param.inNumberNoSize: param.outNumberNoSize;

	try {
		ErlAtom *_t = ErlAtom::cast(term);
		if (_t->atomValue() == std::string("true")) {
			param.miniVariant->val.b = true;
		} else if (_t->atomValue() == std::string("false")) {
			param.miniVariant->val.b = false;
		} else {
			PR_LOG( log, PR_LOG_ERROR, 
				("-- parameter %i bad type, is an atom but not a bool ('true'|'false')", number));
			oss << "Parameter " << number << " incorrect type";
			throw InterfaceMismatchException(oss.str());
		}
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_ERROR, 
			("-- parameter %i bad type, is not a bool", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}
void ErlXPCOMMethodUnmarshaller::t_char(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	std::ostringstream oss;	
	ErlTerm *term = getTermForParam(param);
	int number = processingIn? param.inNumberNoSize: param.outNumberNoSize;

	try {
		// FIXME: Check ranges
		ErlLong *_t = ErlLong::cast(term);
		param.miniVariant->val.c = _t->longValue();
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_DEBUG, 
			("-- parameter %i bad type, is not a integer (char)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}
void ErlXPCOMMethodUnmarshaller::t_wchar(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	std::ostringstream oss;	
	ErlTerm *term = getTermForParam(param);
	int number = processingIn? param.inNumberNoSize: param.outNumberNoSize;

	try {
		// FIXME: Check ranges
		ErlLong *_t = ErlLong::cast(term);
		param.miniVariant->val.wc = _t->longValue();
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_DEBUG, 
			("-- parameter %i bad type, is not a integer (wchar)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}
void ErlXPCOMMethodUnmarshaller::t_void(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	// Unsupported 
	t_default(&param, NULL, NULL, NULL);
}
void ErlXPCOMMethodUnmarshaller::t_iid(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	std::ostringstream oss;	
	ErlTerm *term = getTermForParam(param);
	int number = processingIn? param.inNumberNoSize: param.outNumberNoSize;

	// FIXME: Use other thing than a string ? :?
	if (term->instanceOf(ERL_STRING)) {
		ErlString *_t = (ErlString *) term;
		nsID *iid = (nsID*) PR_Malloc(sizeof(nsID));
		if (!iid->Parse(_t->stringValue())) {
			PR_LOG( log, PR_LOG_ERROR, 
				("-- parameter %i bad type, iid parse error", number));
			oss << "Parameter " << number << " incorrect type (iid parse error)";
			throw InterfaceMismatchException(oss.str());
		}
		param.miniVariant->val.p = (void*) iid;
	} else {
		PR_LOG( log, PR_LOG_ERROR, 
			("-- parameter %i bad type, is not an string (iid)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}
void ErlXPCOMMethodUnmarshaller::t_char_str(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_char_str_common(param);
}
void ErlXPCOMMethodUnmarshaller::t_wchar_str(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	// FIXME: wchar not really supported.
	t_char_str_common(param);
}
void ErlXPCOMMethodUnmarshaller::t_char_str_size_is(ErlXPCOMParam &param,
								ErlXPCOMParam &paramDep1,
								ErlXPCOMParam &paramDep2) 
throw (InterfaceMismatchException, InternalErrorException) {
	int size = t_char_str_common(param);
	paramDep1.miniVariant->val.u32 = size;
	paramDep2.miniVariant->val.u32 = size+1;
}
void ErlXPCOMMethodUnmarshaller::t_wchar_str_size_is(ErlXPCOMParam &param,
								 ErlXPCOMParam &paramDep1,
								 ErlXPCOMParam &paramDep2) 
throw (InterfaceMismatchException, InternalErrorException) {
	// FIXME: wchar not really supported.
	int size = t_char_str_common(param);
	paramDep1.miniVariant->val.u32 = size;
	paramDep2.miniVariant->val.u32 = size+1;
}
void ErlXPCOMMethodUnmarshaller::t_cstring_str(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_cstring_common(param);
}
void ErlXPCOMMethodUnmarshaller::t_utf8_str(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_cstring_common(param);
}
void ErlXPCOMMethodUnmarshaller::t_dom_str(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	// Delegate
	t_astring_common(param);
}
void ErlXPCOMMethodUnmarshaller::t_astring_str(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_astring_common(param);
}
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void ErlXPCOMMethodUnmarshaller::t_interface(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_interface_common(param, *param.iid);
}
void ErlXPCOMMethodUnmarshaller::t_interface_is(ErlXPCOMParam &param,
							ErlXPCOMParam &paramDep) 
throw (InterfaceMismatchException, InternalErrorException) {
	t_interface_common(param, *((nsIID *) paramDep.miniVariant->val.p));
}

void ErlXPCOMMethodUnmarshaller::t_array(ErlXPCOMParam &param,
									     ErlXPCOMParam &paramDep) 
throw (InterfaceMismatchException, InternalErrorException) {

	std::ostringstream oss;	
	ErlTerm *term = getTermForParam(param);

	PR_LOG( log, PR_LOG_DEBUG, 
		(" `-> Decoding an Array in param %i", param.number));

	if (term->instanceOf(ERL_LIST)) {
		ErlList *_t = (ErlList *) term;
		// Set the size_is param
		unsigned int arraySize = _t->arity();
		paramDep.miniVariant->val.u32 = _t->arity();
		PRUint8 elementTag = param.arrayElementType.TagPart();
		PRUint32 elementSize = getSizeForTagType(elementTag);
	
		// First allocate the array vector
		param.miniVariant->val.p = PR_Malloc(elementSize * arraySize);
	
		processingArray = true;
		erlangArray.reset(_t);
		
		// Process each element
		for (unsigned int i=0; i<_t->arity(); i++) {
			PR_LOG( log, PR_LOG_DEBUG, 
				("    `-> array element %i", i));
			nsXPTCMiniVariant *elementMiniVariant = (nsXPTCMiniVariant *) 
				(((PRUint8*) param.miniVariant->val.p) + elementSize*i);
			ErlXPCOMParam elementParam(param, i, elementTag, elementMiniVariant);
			do_visit(&elementParam, NULL, NULL);
		}
		processingArray = false;
		erlangArray.reset(0);
			
	} else {
		PR_LOG( log, PR_LOG_ERROR, 
			("-- parameter %i bad type, is not an list (array)", param.number));
		oss << "Parameter " << param.number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}

}

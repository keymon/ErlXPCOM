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
#include "ErlXPCOMMethodMarshaller.h"
#include "nsString.h"
#include "nsReadableUtils.h"

using namespace erlxpcom;


ErlXPCOMMethodMarshaller::ErlXPCOMMethodMarshaller(
	ErlangORB* _orb, bool _processingIn, int _paramsNumber): 
	log(ErlXPCOMLog::getLog()), 
	orb(_orb),
	processingIn(_processingIn),
	paramsNumber(_paramsNumber), 
	paramsCount(0),
	processingArray(false),
	erlangArrayElement(0)
{
	// Allocate the term array
	erlangParams = new ErlTermPtr<>[paramsNumber];
}

ErlXPCOMMethodMarshaller::~ErlXPCOMMethodMarshaller() {
	delete[] erlangParams;
}
	
ErlList* ErlXPCOMMethodMarshaller::getParams() 
	throw (InterfaceMismatchException, InternalErrorException)
{
	if (paramsCount != paramsNumber) {
		PR_LOG(log, PR_LOG_ERROR, 
			("ErlXPCOMMethodMarshaller::getParams(): "
			 "Number of marshalled params (%i) and expected params (%i) differ", 
				paramsCount, paramsNumber));
		throw InterfaceMismatchException(
			"Not enough parameters have ben marshalled");
	}
	if (paramsNumber == 0) {
		return new ErlEmptyList();
	} else {
		try {
			ErlTermPtr<ErlConsList> list(new ErlConsList());
			for (int i=0; i<paramsNumber; i++) {
				// It can be null if is a depended element
				if (erlangParams[i].get())
					list->addElement(erlangParams[i].get());
			} 
			list->close();
			return list.drop();
		} catch (EpiException &e) {
			throw ErlangException("Failed building params list", e);
		}
	}
}

void ErlXPCOMMethodMarshaller::setParam(ErlXPCOMParam &param, ErlTerm* term) {
	if (!processingArray) {
		int number = processingIn? param.inNumberNoSize: param.outNumberNoSize;
		erlangParams[number].reset(term);
		paramsCount++;
	} else {
		erlangArrayElement.reset(term);
	}
}

ErlTerm* ErlXPCOMMethodMarshaller::XPCOMtoErlangObject(
	nsISupports* obj, nsIID &iid) 
{
	ErlXPCOMStub *stub = 0;
	ErlXPCOMProxy *proxy = 0;
	
	// Check if it is null
	if (obj == nsnull) {
		return new ErlAtom("null");
	}
	
	// check if the object is a ErlXPCOMProxy
	obj->QueryInterface(NS_GET_IID(ErlXPCOMProxy), (void **) &proxy);
	
	if (proxy) {
		PR_LOG( log, PR_LOG_DEBUG, 
			("Marshaller: object 0x%08x is a proxy OID=%i", obj, proxy->getOID()));
		// Return the proxy oid
		ErlTerm *term = 
			new ErlTuple(
				new ErlAtom("erlang_oid"), 
				ErlXPCOMHelper::OIDToErlang(proxy->getOID()));
		// Decrease the reference counter (increased in QueryInterface)
		NS_RELEASE(proxy);
		return term;
	} 
	// check if this object is registered already as stub
	stub = orb->searchStub(obj, iid);
	if (stub) {
		PR_LOG( log, PR_LOG_DEBUG, 
			("Marshaller: object 0x%08x is a stub OID=%i", obj, stub->getOID()));
		// Return the stub oid
		return new ErlTuple(
			new ErlAtom("xpcom_oid"), 
			ErlXPCOMHelper::OIDToErlang(stub->getOID()));
	} 

	// Create the new stub, attached to current thread
	stub = new ErlXPCOMStub(obj, iid, NS_CURRENT_THREAD);
	// register it
	orb->registerStub(stub);

	PR_LOG( log, PR_LOG_DEBUG, 
		("Marshaller: registered new stub: %s", stub->toString().c_str()));

	return new ErlTuple(
		new ErlAtom("xpcom_oid"), 
		ErlXPCOMHelper::OIDToErlang(stub->getOID()));
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Visitor callbacks |
//~~~~~~~~~~~~~~~~~~~'
void ErlXPCOMMethodMarshaller::t_integer_8(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	setParam(param, new ErlLong(param.miniVariant->val.i8));
}
void ErlXPCOMMethodMarshaller::t_integer_16(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	setParam(param, new ErlLong(param.miniVariant->val.i16));
}
void ErlXPCOMMethodMarshaller::t_integer_32(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	setParam(param, new ErlLong(param.miniVariant->val.i32));
}
void ErlXPCOMMethodMarshaller::t_integer_64(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	// FIXME: Unimplemented :(
	throw InternalErrorException("integer 64 not implemented");
}
void ErlXPCOMMethodMarshaller::t_unsigned_integer_8(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	setParam(param, new ErlLong(param.miniVariant->val.u8));
}
void ErlXPCOMMethodMarshaller::t_unsigned_integer_16(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	setParam(param, new ErlLong(param.miniVariant->val.u16));
}
void ErlXPCOMMethodMarshaller::t_unsigned_integer_32(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	setParam(param, new ErlLong(param.miniVariant->val.u32));
}
void ErlXPCOMMethodMarshaller::t_unsigned_integer_64(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	// FIXME: Unimplemented :(
	throw InternalErrorException("integer 64 not implemented");
}
void ErlXPCOMMethodMarshaller::t_float(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	setParam(param, new ErlDouble(param.miniVariant->val.f));
}
void ErlXPCOMMethodMarshaller::t_double(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	setParam(param, new ErlDouble(param.miniVariant->val.d));
}
void ErlXPCOMMethodMarshaller::t_bool(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	setParam(param, param.miniVariant->val.b?
						new ErlAtom("true"):
						new ErlAtom("false"));
}
void ErlXPCOMMethodMarshaller::t_char(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	setParam(param, new ErlLong(param.miniVariant->val.c));
}
void ErlXPCOMMethodMarshaller::t_wchar(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	setParam(param, new ErlLong(param.miniVariant->val.wc));
}
void ErlXPCOMMethodMarshaller::t_void(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	// Unsupported 
	t_default(&param, NULL, NULL, NULL);
}
void ErlXPCOMMethodMarshaller::t_iid(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	// FIXME: Use the string is a big Chapuza, as is said in my town ;)
	char *iidstr = ((nsIID *)param.miniVariant->val.p)->ToString();
	// strip the '{' '}' charaters
	ErlString *iid_term = 
		new ErlString(iidstr, (unsigned int) 1, (unsigned int) strlen(iidstr)-2);
	PR_Free(iidstr);
	setParam(param, iid_term);
}
void ErlXPCOMMethodMarshaller::t_char_str(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	if (param.miniVariant->val.p) {
		setParam(param, new ErlString((char *) param.miniVariant->val.p));
	} else { 
		setParam(param, new ErlAtom("null"));
	}
}
void ErlXPCOMMethodMarshaller::t_wchar_str(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	// FIXME, not really wchar
	setParam(param, new ErlString((char *) param.miniVariant->val.p));
}
void ErlXPCOMMethodMarshaller::t_char_str_size_is(ErlXPCOMParam &param,
								ErlXPCOMParam &paramDep1,
								ErlXPCOMParam &paramDep2) 
throw (InterfaceMismatchException, InternalErrorException) {
	setParam(param, 
			 new ErlString((char*) param.miniVariant->val.p, 
							(unsigned int)0, (unsigned int) paramDep2.miniVariant->val.u32));
}
void ErlXPCOMMethodMarshaller::t_wchar_str_size_is(ErlXPCOMParam &param,
								 ErlXPCOMParam &paramDep1,
								 ErlXPCOMParam &paramDep2) 
throw (InterfaceMismatchException, InternalErrorException) {
	// FIXME, not really wchar
	setParam(param, 
			 new ErlString((char *) param.miniVariant->val.p, 
							(unsigned int) 0, 
							(unsigned int) paramDep2.miniVariant->val.u32));
}
void ErlXPCOMMethodMarshaller::t_dom_str(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	nsAString *aString = (nsAString *) param.miniVariant->val.p;
	char *buffer = ToNewCString(*aString);
	setParam(param, new ErlString(buffer));
	PR_Free(buffer);
}
void ErlXPCOMMethodMarshaller::t_utf8_str(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	nsACString *acString = (nsACString *) param.miniVariant->val.p;
	PRUnichar *buffer = ToNewUnicode(*acString);
	setParam(param, new ErlString((char*) buffer));
	PR_Free(buffer);
}
void ErlXPCOMMethodMarshaller::t_cstring_str(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {

	nsACString *acString = (nsACString *) param.miniVariant->val.p;
	PRUnichar *buffer = ToNewUnicode(*acString);
	setParam(param, new ErlString((char*)buffer));
	PR_Free(buffer);
}
void ErlXPCOMMethodMarshaller::t_astring_str(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	nsAString *aString = (nsAString *) param.miniVariant->val.p;
	char *buffer = ToNewCString(*aString);
	setParam(param, new ErlString(buffer));
	PR_Free(buffer);
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void ErlXPCOMMethodMarshaller::t_interface(ErlXPCOMParam &param) 
throw (InterfaceMismatchException, InternalErrorException) {
	nsISupports *obj = (nsISupports *) param.miniVariant->val.p;
	ErlTerm* erlangObj = XPCOMtoErlangObject(obj, *param.iid);
	setParam(param, erlangObj);
}
void ErlXPCOMMethodMarshaller::t_interface_is(ErlXPCOMParam &param,
											  ErlXPCOMParam &paramDep) 
throw (InterfaceMismatchException, InternalErrorException) {
	nsIID* iid = (nsIID*) paramDep.miniVariant->val.p;
	nsISupports *obj = (nsISupports *) param.miniVariant->val.p;
	ErlTerm* erlangObj = XPCOMtoErlangObject(obj, *iid);
	setParam(param, erlangObj);
}
void ErlXPCOMMethodMarshaller::t_array(ErlXPCOMParam &param,
					 ErlXPCOMParam &paramDep) 
throw (InterfaceMismatchException, InternalErrorException) {

	PRUint32 size = paramDep.miniVariant->val.u32;
	if (size == 0) {
		setParam (param, new ErlEmptyList());
	} else {
		processingArray = true;
		
		PRUint8 elementTag = param.arrayElementType.TagPart();
		PRUint32 elementSize = getSizeForTagType(elementTag);
		try {
			ErlTermPtr<ErlConsList> list(new ErlConsList());
			for (unsigned int i = 0; i< size; i++) {
				nsXPTCMiniVariant *elementMiniVariant = (nsXPTCMiniVariant *) 
					(((PRUint8*) param.miniVariant->val.p) + elementSize*i);
				ErlXPCOMParam elementParam(param, i, elementTag, elementMiniVariant);
				do_visit(&elementParam, NULL, NULL);
				list->addElement(erlangArrayElement.get());
			} 
			list->close();
			processingArray = false;
			setParam (param, list.get());
		} catch (EpiException &e) {
			throw ErlangException("Failed building array", e);
		}
	}
}




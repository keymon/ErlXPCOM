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
#include <string>
#include <string.h>

#include <prmem.h>
#include <nsMemory.h>
#include <nsISupports.h>

#include "ErlXPCOMMarshallUnmarshall.h"
#include "ErlXPCOMLog.h"
#include "ErlXPCOMDefs.h"
#include "ErlXPCOMHelper.h"
#include "ErlangORB.h"

using namespace erlxpcom;

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void setMiniVariantsOfParam(nsXPTCMiniVariant* &miniVariant,
							nsXPTCMiniVariant* &miniVariantDep1,
							nsXPTCMiniVariant* &miniVariantDep2, 
							const int paramNumber, 
							const nsXPTParamInfo &paramInfo,
							nsIInterfaceInfo *interfaceInfo, 
							const PRUint16 methodIndex,
							const nsXPTMethodInfo* methodInfo,
							nsXPTCMiniVariant** params) 
	throw (InterfaceMismatchException);

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ErlXPCOMMarshallerUnmarshaller::ErlXPCOMMarshallerUnmarshaller(
	ErlangORB* _orb,
	nsIInterfaceInfo* _interfaceInfo, 
	const nsXPTMethodInfo* _methodInfo, 
	PRUint16 _methodIndex):
		log(ErlXPCOMLog::getLog()), 
		orb(_orb), 
		interfaceInfo(_interfaceInfo), 
		methodInfo(_methodInfo),
		methodIndex(_methodIndex)
{
}

/*
 * Inits a variant, allocating real data if the param is inout
 */
void initInVariant(nsXPTCVariant &variant, 
                 nsXPTCMiniVariant &miniVariant, 
				 nsXPTParamInfo &paramInfo,
				 bool isAllocated = false) 
{
	if (paramInfo.IsOut()) {
		nsXPTCMiniVariant* realMiniVariant = (nsXPTCMiniVariant*)
			PR_Malloc(sizeof(nsXPTCMiniVariant));
		*realMiniVariant = miniVariant;
		miniVariant.val.p = realMiniVariant;
		variant.Init(miniVariant, paramInfo.GetType(), 
			paramInfo.flags | nsXPTCVariant::PTR_IS_DATA);
	} else {
		variant.Init(miniVariant, paramInfo.GetType(), paramInfo.flags);
	}
	if (isAllocated) variant.SetValIsAllocated();			
}
 
nsXPTCVariant* ErlXPCOMMarshallerUnmarshaller::unmarshallInParams(ErlList* inParams) 
	throw (InterfaceMismatchException, InternalErrorException) 
{
	PR_LOG( log, PR_LOG_DEBUG, 
		("ErlXPCOMMarshallerUnmarshaller::unmarshallInParams():"));
	// Create 
	int paramcount = methodInfo->GetParamCount();
	nsXPTCVariant* variants = 
		(nsXPTCVariant *)  PR_Malloc(sizeof(nsXPTCVariant)*paramcount);
	// FIXME: use a dinamic array for this
	bool dependedArgs[255];
	unsigned int listindex = 0;
	try {
	    // First, get the depended arguments (size_is...) to ignore them 
		for (int i=0; i< paramcount; i++) dependedArgs[i] = false;
		for (int i=0; i< paramcount; i++) {
			nsXPTParamInfo paramInfo = methodInfo->GetParam(i);
			nsXPTType type = paramInfo.GetType();

			if (type.TagPart() == nsXPTType::T_PSTRING_SIZE_IS ||
				type.TagPart() == nsXPTType::T_PWSTRING_SIZE_IS) {
				PRUint8 argnum, argnum2;
				nsresult rv;
				rv = interfaceInfo->GetSizeIsArgNumberForParam(methodIndex, 
					&paramInfo, 0, &argnum);
				// If failed, is not size_is :)
				if (NS_SUCCEEDED(rv)) {
					dependedArgs[argnum] = true;
				}
				// If failed, is not length_is :)
				rv = interfaceInfo->GetLengthIsArgNumberForParam(methodIndex, 
					&paramInfo, 0, &argnum2);
				if (NS_SUCCEEDED(rv)) {
					dependedArgs[argnum2] = true;
				}
			}
		}
		// Decode the params. 
		for (int i=0; i< paramcount; i++) {

			nsXPTCMiniVariant miniVariant;
			nsXPTCMiniVariant miniVariantDep1; // depended arg 1
			nsXPTCMiniVariant miniVariantDep2; // depended arg 2
			bool isAllocated;
			
			// ... get param info for this variant...
			nsXPTParamInfo paramInfo = methodInfo->GetParam(i);
			
			// ... For each In parameter...
			if (paramInfo.IsIn()) {
				// Ignore depended in args 
				if (dependedArgs[i]) continue;
				
				// ... get the term...
				ErlTerm *term = inParams->elementAt(listindex++);
				// ... decode it ...
				Erlang2xptcMiniVariant(term, 
					miniVariant, miniVariantDep1, miniVariantDep2, 
					paramInfo, isAllocated, listindex);
				// ... init the Variant...
				initInVariant(variants[i], miniVariant, paramInfo, isAllocated);
				
				// ... init the dependent params (in string only) if necesary (and possible)
				nsXPTType type = paramInfo.GetType();
				if (type.TagPart() == nsXPTType::T_PSTRING_SIZE_IS ||
					type.TagPart() == nsXPTType::T_PWSTRING_SIZE_IS) {
					PRUint8 argnum;
					nsresult rv = interfaceInfo->GetSizeIsArgNumberForParam(
						methodIndex, &paramInfo, 0, &argnum);
					if (NS_SUCCEEDED(rv)) {
						nsXPTParamInfo paramInfoDep = 
							methodInfo->GetParam(argnum);
						initInVariant(variants[argnum], miniVariantDep1, paramInfoDep);

						variants[argnum].Init(miniVariantDep1, 
											  paramInfoDep.GetType(), 
											  paramInfoDep.flags);
					} 
					rv = interfaceInfo->GetLengthIsArgNumberForParam(
						methodIndex, &paramInfo, 0, &argnum);
					if (NS_SUCCEEDED(rv)) {
						nsXPTParamInfo paramInfoDep = 
							methodInfo->GetParam(argnum);
						initInVariant(variants[argnum], miniVariantDep2, paramInfoDep);
					}
				}
			// If is out parameter, alloc the internal minivariant
			} else if (paramInfo.IsOut()) {
				miniVariant.val.p = (nsXPTCMiniVariant*)
						PR_Malloc(sizeof(nsXPTCMiniVariant));
				// It's necesary indicate that the ptr in variant
				// is the real data or InvokeMethod will use a null pointer
				// Of cuorse, that is not documented! :-/
				variants[i].Init(miniVariant, paramInfo.GetType(), 
					paramInfo.flags | nsXPTCVariant::PTR_IS_DATA);
			}
		}
		// Check if there are more arguments
		if (listindex < inParams->arity()) {
			PR_LOG( log, PR_LOG_DEBUG, 
				("-- Warning, too much arguments: %i provided, %i used", 
					inParams->arity(), listindex));
		}
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_DEBUG, 
			("-- unmarshallInParams(): Insuficient arguments: \"%s\"", e.what()));
		PR_Free(variants);
		throw InterfaceMismatchException("Insuficient number of arguments");
	} catch (std::exception &e) {
		PR_LOG( log, PR_LOG_DEBUG, 
			("-- unmarshallInParams(): Exception catched: \"%s\"", e.what()));
		PR_Free(variants);
		throw e;
	}
	return variants;
}

ErlList* ErlXPCOMMarshallerUnmarshaller::marshallOutParams(nsXPTCVariant* params)
	throw (InterfaceMismatchException, InternalErrorException) 
{
	PR_LOG( log, PR_LOG_DEBUG, 
		("ErlXPCOMMarshallerUnmarshaller::marshallOutParams():"));
	int paramcount = methodInfo->GetParamCount();

	int outParamsCounter = 0; // ... count the out params ... 
	ErlTermPtr<ErlConsList> outParams(new ErlConsList()); // where write the encoded terms
	try {
		// ... encode the out params. 
		for (int i=0; i< paramcount; i++) {
		
			nsXPTParamInfo paramInfo = methodInfo->GetParam(i);
			// If it is an out parameter
			if (paramInfo.IsOut()) {
				outParamsCounter++;
				
				nsXPTCMiniVariant *miniVariant = 
					(nsXPTCMiniVariant*) params[i].ptr;
				nsXPTCMiniVariant *miniVariantDep1 = miniVariant;
				nsXPTCMiniVariant *miniVariantDep2 = miniVariant;
	
				// Check if it is dependent, and recover index of 
				// depend args if necesary. 
				nsXPTType type = paramInfo.GetType();
				switch(type.TagPart()) {
				case nsXPTType::T_PSTRING_SIZE_IS:
				case nsXPTType::T_PWSTRING_SIZE_IS:
					if (1==1) {
					nsresult rv;
					PRUint8 argnum;
					rv = interfaceInfo->GetSizeIsArgNumberForParam(
						methodIndex, &paramInfo, 0, &argnum);
					if (NS_SUCCEEDED(rv)) {
						// check if is out parameter
						nsXPTParamInfo paramInfoDep = methodInfo->GetParam(argnum);
						miniVariantDep1 = paramInfoDep.IsOut()?
							(nsXPTCMiniVariant*) params[argnum].ptr :
							&params[argnum];
					} else {
						PR_LOG( log, PR_LOG_DEBUG, 
							("-- failure getting size_is for param %i", i));
						throw XPCOMException("Failure getting interface info", rv);
					}
					rv = interfaceInfo->GetLengthIsArgNumberForParam(
						methodIndex, &paramInfo, 0, &argnum);
					if (NS_SUCCEEDED(rv)) {
						// check if is out parameter
						nsXPTParamInfo paramInfoDep = methodInfo->GetParam(argnum);
						miniVariantDep2 = paramInfoDep.IsOut()?
							(nsXPTCMiniVariant*) params[argnum].ptr :
							&params[argnum];
					} else {
						PR_LOG( log, PR_LOG_DEBUG, 
							("-- failure getting length_is for param %i", i));
						throw XPCOMException("Failure getting interface info", rv);
					}
					}
				break;
				case nsXPTType::T_INTERFACE_IS:
					if (1==1) {
					PRUint8 argnum;
					nsresult rv = 
						interfaceInfo->GetInterfaceIsArgNumberForParam(methodIndex, 
							&paramInfo, &argnum);
					if (NS_SUCCEEDED(rv)) {
						// check if is out parameter
						nsXPTParamInfo paramInfoDep = methodInfo->GetParam(argnum);
						miniVariantDep1 = paramInfoDep.IsOut()?
							(nsXPTCMiniVariant*) params[argnum].ptr :
							&params[argnum];
					} else {
						PR_LOG( log, PR_LOG_DEBUG, 
							("-- failure getting IID for param %i", i));
						throw XPCOMException("Failure getting interface info", rv);
					}
					}
					default:
						break;
				}
			
				
				// ... encode the param 
				ErlTerm *outParam = 
					xptcMiniVariant2Erlang(*miniVariant, 
						*miniVariantDep1, *miniVariantDep2, 
						paramInfo, i);
				outParams->addElement(outParam);
			}
		}
	} catch (std::exception &e) {
		PR_LOG( log, PR_LOG_DEBUG, 
			("-- marshallOutParams(): Exception catched: \"%s\"", e.what()));
		throw e;
	}
	// If no out params, return an empty list
	if (outParamsCounter == 0) {
		return new ErlEmptyList();
	} else {
		outParams->close();
		return outParams.drop();
	}

}
	
ErlList* ErlXPCOMMarshallerUnmarshaller::marshallInParams(nsXPTCMiniVariant* params)
	throw (InterfaceMismatchException) 
{
	PR_LOG( log, PR_LOG_DEBUG, 
		("ErlXPCOMMarshallerUnmarshaller::marshallInParams():"));
	int paramcount = methodInfo->GetParamCount();

	int inParamsCounter = 0;
	ErlTermPtr<ErlConsList> inParams(new ErlConsList()); // where write the encoded terms

	// Create an array of pointers to parameters, for use with 
	// setMiniVariantsOfParam 
	nsXPTCMiniVariant* paramArray[paramcount];
	for (int i=0; i<paramcount; i++) {
		paramArray[i] = &(params[i]);
	}
	
	try {
		// ... encode the in params. 
		for (int i=0; i< paramcount; i++) {
			nsXPTParamInfo paramInfo = methodInfo->GetParam(i);
			if (paramInfo.IsIn()) {
				nsXPTCMiniVariant *miniVariant;
				nsXPTCMiniVariant *miniVariantDep1;
				nsXPTCMiniVariant *miniVariantDep2;
				
				// Get the variants 
				setMiniVariantsOfParam(miniVariant, miniVariantDep1, miniVariantDep2, 
									   i, paramInfo, interfaceInfo, methodIndex,
									   methodInfo, paramArray);
				// ... encode the param 
				ErlTerm *inParam = 
					xptcMiniVariant2Erlang(*miniVariant, 
										   *miniVariantDep1, 
										   *miniVariantDep2, 
											paramInfo, i);
				inParams->addElement(inParam);
				inParamsCounter++;
			}
		}

	} catch (std::exception &e) {
		PR_LOG( log, PR_LOG_DEBUG, 
			("-- marshallInParams(): Exception catched: \"%s\"", e.what()));
		throw e;
	}
	// If no in params, return an empty list
	if (inParamsCounter == 0) {
		return new ErlEmptyList();
	} else {
		inParams->close();
		return inParams.drop();
	}
}

void ErlXPCOMMarshallerUnmarshaller::unmarshallOutParams(nsXPTCMiniVariant* params, 
														 ErlList* outParams)
	throw (InterfaceMismatchException) 
{
//	PR_LOG( log, PR_LOG_DEBUG, 
//		("ErlXPCOMMarshallerUnmarshaller::unmarshallOutParams():"));
//
//
//	int paramcount = methodInfo->GetParamCount();
//
//	// Create an array of pointers to parameters, for use with 
//	// setMiniVariantsOfParam 
//	nsXPTCMiniVariant* paramArray[paramcount];
//	for (int i=0; i<paramcount; i++) {
//		paramArray[i] = &(params[i]);
//	}
//
//	bool dependedArgs[paramcount];
//	unsigned int listindex = 0;
//	try {
//	    // First, get the depended arguments (size_is...) to ignore them 
//		for (int i=0; i< paramcount; i++) dependedArgs[i] = false;
//		for (int i=0; i< paramcount; i++) {
//			nsXPTParamInfo paramInfo = methodInfo->GetParam(i);
//			nsXPTType type = paramInfo.GetType();
//
//			if (type.TagPart() == nsXPTType::T_PSTRING_SIZE_IS ||
//				type.TagPart() == nsXPTType::T_PWSTRING_SIZE_IS) {
//				PRUint8 argnum, argnum2;
//				nsresult rv;
//				rv = interfaceInfo->GetSizeIsArgNumberForParam(methodIndex, 
//					&paramInfo, 0, &argnum);
//				// If failed, is not size_is :)
//				if (NS_SUCCEEDED(rv)) {
//					dependedArgs[argnum] = true;
//				}
//				// If failed, is not length_is :)
//				rv = interfaceInfo->GetLengthIsArgNumberForParam(methodIndex, 
//					&paramInfo, 0, &argnum2);
//				if (NS_SUCCEEDED(rv)) {
//					dependedArgs[argnum2] = true;
//				}
//			}
//		}
//		// Decode the params. 
//		for (int i=0; i< paramcount; i++) {
//
//			nsXPTCMiniVariant miniVariant;
//			nsXPTCMiniVariant miniVariantDep1; // depended arg 1
//			nsXPTCMiniVariant miniVariantDep2; // depended arg 2
//			bool isAllocated;
//			
//			// ... get param info for this variant...
//			nsXPTParamInfo paramInfo = methodInfo->GetParam(i);
//			
//			// ... For each In parameter...
//			if (paramInfo.IsIn()) {
//				// Ignore depended in args 
//				if (dependedArgs[i]) continue;
//				
//				// ... get the term...
//				ErlTerm *term = inParams->elementAt(listindex++);
//				// ... decode it ...
//				Erlang2xptcMiniVariant(term, 
//					miniVariant, miniVariantDep1, miniVariantDep2, 
//					paramInfo, isAllocated, listindex);
//				// ... init the Variant...
//				initInVariant(variants[i], miniVariant, paramInfo, isAllocated);
//				
//				// ... init the dependent params (in string only) if necesary (and possible)
//				nsXPTType type = paramInfo.GetType();
//				if (type.TagPart() == nsXPTType::T_PSTRING_SIZE_IS ||
//					type.TagPart() == nsXPTType::T_PWSTRING_SIZE_IS) {
//					PRUint8 argnum;
//					nsresult rv = interfaceInfo->GetSizeIsArgNumberForParam(
//						methodIndex, &paramInfo, 0, &argnum);
//					if (NS_SUCCEEDED(rv)) {
//						nsXPTParamInfo paramInfoDep = 
//							methodInfo->GetParam(argnum);
//						initInVariant(variants[argnum], miniVariantDep1, paramInfoDep);
//
//						variants[argnum].Init(miniVariantDep1, 
//											  paramInfoDep.GetType(), 
//											  paramInfoDep.flags);
//					} 
//					rv = interfaceInfo->GetLengthIsArgNumberForParam(
//						methodIndex, &paramInfo, 0, &argnum);
//					if (NS_SUCCEEDED(rv)) {
//						nsXPTParamInfo paramInfoDep = 
//							methodInfo->GetParam(argnum);
//						initInVariant(variants[argnum], miniVariantDep2, paramInfoDep);
//					}
//				}
//			// If is out parameter, alloc the internal minivariant
//			} else if (paramInfo.IsOut()) {
//				miniVariant.val.p = (nsXPTCMiniVariant*)
//						PR_Malloc(sizeof(nsXPTCMiniVariant));
//				// It's necesary indicate that the ptr in variant
//				// is the real data or InvokeMethod will use a null pointer
//				// Of cuorse, that is not documented! :-/
//				variants[i].Init(miniVariant, paramInfo.GetType(), 
//					paramInfo.flags | nsXPTCVariant::PTR_IS_DATA);
//			}
//		}
//		// Check if there are more arguments
//		if (listindex < inParams->arity()) {
//			PR_LOG( log, PR_LOG_DEBUG, 
//				("-- Warning, too much arguments: %i provided, %i used", 
//					inParams->arity(), listindex));
//		}
//	} catch (EpiBadArgument &e) {
//		PR_LOG( log, PR_LOG_DEBUG, 
//			("-- unmarshallInParams(): Insuficient arguments: \"%s\"", e.what()));
//		PR_Free(variants);
//		throw InterfaceMismatchException("Insuficient number of arguments");
//	} catch (std::exception &e) {
//		PR_LOG( log, PR_LOG_DEBUG, 
//			("-- unmarshallInParams(): Exception catched: \"%s\"", e.what()));
//		PR_Free(variants);
//		throw e;
//	}
//	return variants;
}

/**
 * This function will check if this param is in or out, setting the minivariant
 * to the param or the minivariant pointed by the param for each case.
 */
void setMiniVariant(nsXPTCMiniVariant* &miniVariant, 
					nsXPTCMiniVariant &param, 
					const nsXPTParamInfo &paramInfo) 
{
	miniVariant = (nsXPTCMiniVariant*) (
		paramInfo.IsOut()?
				param.val.p?
					((nsXPTCMiniVariant*) param.val.p)->val.p:
					NULL:
			&param
		);
}

/*
 * Sets the pointers of variants for the given argument number
 */
void setMiniVariantsOfParam(nsXPTCMiniVariant* &miniVariant,
							nsXPTCMiniVariant* &miniVariantDep1,
							nsXPTCMiniVariant* &miniVariantDep2, 
							const int paramNumber, 
							const nsXPTParamInfo &paramInfo,
							nsIInterfaceInfo *interfaceInfo, 
							const PRUint16 methodIndex,
							const nsXPTMethodInfo* methodInfo,
							nsXPTCMiniVariant** params) 
	throw (InterfaceMismatchException) 
{
	nsresult rv;
	PRUint8 argnum;
	nsXPTType type = paramInfo.GetType();

	setMiniVariant(miniVariant, *params[paramNumber], paramInfo);
	
	switch(type.TagPart()) {
		case nsXPTType::T_PSTRING_SIZE_IS:
		case nsXPTType::T_PWSTRING_SIZE_IS:
			rv = interfaceInfo->GetSizeIsArgNumberForParam(
						methodIndex, &paramInfo, 0, &argnum);
			if (NS_SUCCEEDED(rv)) {
				// check if is out parameter
				nsXPTParamInfo paramInfoDep = methodInfo->GetParam(argnum);
				setMiniVariant(miniVariantDep1, *params[argnum], paramInfoDep);
			} else {
				PR_LOG(ErlXPCOMLog::getLog(), PR_LOG_DEBUG, 
					("-- failure getting size_is for param %i", paramNumber));
				throw XPCOMException("Failure getting interface info", rv);
			}
			rv = interfaceInfo->GetLengthIsArgNumberForParam(
				methodIndex, &paramInfo, 0, &argnum);
			if (NS_SUCCEEDED(rv)) {
				// check if is out parameter
				nsXPTParamInfo paramInfoDep = methodInfo->GetParam(argnum);
				setMiniVariant(miniVariantDep2, *params[argnum], paramInfoDep);
			} else {
				PR_LOG(ErlXPCOMLog::getLog(), PR_LOG_DEBUG, 
					("-- failure getting length_is for param %i", paramNumber));
				throw XPCOMException("Failure getting interface info", rv);
			}
		break;
		case nsXPTType::T_INTERFACE_IS:
			rv = interfaceInfo->GetInterfaceIsArgNumberForParam(methodIndex, 
					&paramInfo, &argnum);
			if (NS_SUCCEEDED(rv)) {
				// check if is out parameter
				nsXPTParamInfo paramInfoDep = methodInfo->GetParam(argnum);
				setMiniVariant(miniVariantDep1, *params[argnum], paramInfoDep);
			} else {
				PR_LOG(ErlXPCOMLog::getLog(), PR_LOG_DEBUG, 
					("-- failure getting IID for param %i", paramNumber));
				throw XPCOMException("Failure getting interface info", rv);
			}
		default:
			break;
	}
}

ErlTerm* ErlXPCOMMarshallerUnmarshaller::XPCOMtoErlangObject(
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
			("-- object 0x%08x is a proxy", obj));
		// Decrease the reference counter (increased in QueryInterface)
		NS_RELEASE(proxy);
		// Return the proxy oid
		return new ErlTuple(
			new ErlAtom("erlang_oid"), 
			ErlXPCOMHelper::OIDToErlang(proxy->getOID()));
	} 
	// check if this object is registered already as stub
	stub = orb->searchStub(obj, iid);
	if (stub) {
		PR_LOG( log, PR_LOG_DEBUG, 
			("-- object 0x%08x is a stub", obj));
		// Return the stub oid
		return new ErlTuple(
			new ErlAtom("xpcom_oid"), 
			ErlXPCOMHelper::OIDToErlang(stub->getOID()));
	} 

	// Create the new stub, attached to current thread
	PR_LOG( log, PR_LOG_DEBUG, 
		("-- registering new stub for object 0x%08x", obj));

	stub = new ErlXPCOMStub(obj, iid, NS_CURRENT_THREAD);
	// register it
	orb->registerStub(stub);

	return new ErlTuple(
		new ErlAtom("xpcom_oid"), 
		ErlXPCOMHelper::OIDToErlang(stub->getOID()));
}


void ErlXPCOMMarshallerUnmarshaller::freeXPTCVariant(nsXPTCVariant* params)
{
	PR_LOG(log, PR_LOG_DEBUG,
		("ErlXPCOMMarshallerUnmarshaller::freeXPTCVariant():"));
	// For each param
	int paramcount = methodInfo->GetParamCount();
	for (int i=0; i<paramcount; i++) {
		// Get the info for this param
		nsXPTParamInfo paramInfo = methodInfo->GetParam(i);
		// Check if is an out parameter. 
		if (paramInfo.IsOut()) {
			// delete real data string, iids... all in val.p except the interfaces 
			switch(paramInfo.GetType().TagPart()) {
				case nsXPTType::T_VOID:              /* fall through */
				case nsXPTType::T_IID:               /* fall through */
				case nsXPTType::T_DOMSTRING:         /* fall through */
				case nsXPTType::T_CHAR_STR:          /* fall through */
				case nsXPTType::T_WCHAR_STR:         /* fall through */
				case nsXPTType::T_ARRAY:             /* fall through */
				case nsXPTType::T_PSTRING_SIZE_IS:   /* fall through */
				case nsXPTType::T_PWSTRING_SIZE_IS:  /* fall through */
				case nsXPTType::T_UTF8STRING:        /* fall through */
				case nsXPTType::T_CSTRING:           /* fall through */   				
					if (1==1) {
					nsXPTCMiniVariant &variant = *((nsXPTCMiniVariant *) params[i].val.p);
					PR_LOG( log, PR_LOG_DEBUG, 
						("-- freeing real data of out parameter pointer in variant 0x%08x", 
							variant.val.p));
					PR_Free(variant.val.p);
					}
					/* fall through */
				default:
				// ... and delete the pointer 
					PR_LOG( log, PR_LOG_DEBUG, 
						("-- freeing variant in out parameter in 0x%08x", 
							params[i].ptr));
					PR_Free(params[i].ptr);
					break;
			}
		} else {
			// check if it was allocated and must be freed
			if (params[i].IsValAllocated()) {
				// Is not out, delete the 
				PR_LOG( log, PR_LOG_DEBUG, ("-- freeing allocated pointer in 'in' variant 0x%08x", 
					params[i].ptr));
				PR_Free(params[i].ptr);
			} 
		}
	}
	PR_LOG( log, PR_LOG_DEBUG, ("-- freeing variant array 0x%08x", params));
	PR_Free(params);	
}


/* 
 * Returns the Erlang representation of an XPCOM nsXPTCVariant object 
 * @param miniVariant nsXPTCMiniVariant to process
 * @param number parameter number in this method (for error notification)
 * @throw InterfaceMismatchException if can't be converted by type mistmach
 * @throw InternalErrorException if there is an error while encoding params,
 *		for example if there is a fail getting interface info.
 */
ErlTerm* ErlXPCOMMarshallerUnmarshaller::xptcMiniVariant2Erlang(
		const nsXPTCMiniVariant &miniVariant, 
	 	const nsXPTCMiniVariant &miniVariantDep1, 
	 	const nsXPTCMiniVariant &miniVariantDep2, 
		const nsXPTParamInfo &paramInfo,
		const int number) 
	throw (InterfaceMismatchException, InternalErrorException)
{
	std::ostringstream oss;
	// for T_INTERFACE
	nsIID *iid = 0;
	nsresult rv; 

	// Check the variant type
	switch (paramInfo.GetType().TagPart()) {
        case nsXPTType::T_I8:
			return new ErlLong(miniVariant.val.i8);  
			break;
		case nsXPTType::T_I16:            
			return new ErlLong(miniVariant.val.i16);  
			break;
		case nsXPTType::T_I32:         
			return new ErlLong(miniVariant.val.i32);  
			break;
        case nsXPTType::T_I64:
			return new ErlLong(miniVariant.val.i64);  
			break;
		case nsXPTType::T_U8:
			return new ErlLong(miniVariant.val.u8);  
			break;
		case nsXPTType::T_U16:
			return new ErlLong(miniVariant.val.u16);  
			break;
        case nsXPTType::T_U32:
			return new ErlLong(miniVariant.val.u32);  
			break;
        case nsXPTType::T_U64:
			// FIXME
			PR_LOG( log, PR_LOG_DEBUG, 
				("-- Parameter %i is type U64, that is not implemented", number));
			oss << "Parameter " << number << " is type U64, that is not implemented";
			throw InterfaceMismatchException(oss.str());
			break;
        case nsXPTType::T_FLOAT:
			return new ErlDouble(miniVariant.val.f);  
			break;
        case nsXPTType::T_DOUBLE:
			return new ErlDouble(miniVariant.val.d);
		    break;
        case nsXPTType::T_BOOL:
			if (miniVariant.val.b) {
				return new ErlAtom("true");
			} else {
				return new ErlAtom("false");
			}
			break;
        case nsXPTType::T_CHAR:
			return new ErlLong(miniVariant.val.c);  
			break;
        case nsXPTType::T_WCHAR:
			return new ErlLong(miniVariant.val.wc);  
			break;
        case nsXPTType::T_IID:
			if (1==1) {
				// FIXME: Use the string is a big Chapuza, as they say in my town ;)
				char *iidstr = ((nsIID *)miniVariant.val.p)->ToString();
				// string the '{' '}' charaters
				ErlString *iid_term = 
					new ErlString(std::string(iidstr, 1, strlen(iidstr)-2));
				PR_Free(iidstr);
				return iid_term;
			}
			break;
		
        case nsXPTType::T_VOID:
			return 0;
			break;

        case nsXPTType::T_CHAR_STR:
        case nsXPTType::T_CSTRING:
			return new ErlString((char *) miniVariant.val.p);
			break;

        case nsXPTType::T_PSTRING_SIZE_IS:
			return new ErlString(std::string((char *) miniVariant.val.p, 
				0, miniVariantDep2.val.u32));
			break;

		case nsXPTType::T_WCHAR_STR:        
			if (1==1) {
				PRUnichar* p = (PRUnichar*) miniVariant.val.p;
				if (*p == 0) {
					return new ErlEmptyList();
				} else {
					ErlConsList *list = new ErlConsList();
					while (*p) {
						list->addElement(new ErlLong(*p));
						p++;
					}
					return list;
				}
			}
			break;
		

        case nsXPTType::T_PWSTRING_SIZE_IS: 
			if (miniVariantDep2.val.u32 == 0) return new ErlEmptyList();
			else {
				ErlConsList *list = new ErlConsList();
				for (unsigned int i = 0; i <= miniVariantDep2.val.u32; i++) {
					list->addElement(
						new ErlLong(((PRUnichar*) miniVariant.val.p)[i]));
				}
				return list;
			}
			break;
			
        case nsXPTType::T_INTERFACE:
			// Get the iid, from type (T_INTERFACE) ... 
			if (NS_FAILED(rv = 
					interfaceInfo->GetIIDForParam(methodIndex, &paramInfo, &iid))) {
				PR_LOG( log, PR_LOG_DEBUG, 
					("-- failure getting IID for param %i", number));
				throw XPCOMException("Failure getting interface info", rv);
			}
		case nsXPTType::T_INTERFACE_IS:      /* fall through */
			if(1==1) {
				// Get the IID from dependent parameter if necesary
				if (iid == 0) {
					iid = (nsIID*) miniVariantDep1.val.p;
				}
				
				nsISupports *obj = (nsISupports *) miniVariant.val.p;

				ErlTerm* erlangObj = XPCOMtoErlangObject(obj, *iid);
				return erlangObj;
			} // if (1==1)
			break;
		
        case nsXPTType::T_DOMSTRING:         /* fall through */
        case nsXPTType::T_UTF8STRING:        /* fall through */
        case nsXPTType::T_ARRAY:             /* fall through */
        default:                             
			PR_LOG( log, PR_LOG_DEBUG, 
				("-- Parameter %i is type %i, that is not implemented", number, 
					paramInfo.GetType().TagPart()));
			std::ostringstream oss;
			oss << "Parameter " << number << " has an unimplemented type";
			throw InterfaceMismatchException(oss.str());
			break;
	}
}


////////////////////////////////////////////////////////////////////////////////
void erlang_to_i8(ErlTerm* term, nsXPTCMiniVariant &miniVariant, const int number) 
	throw (InterfaceMismatchException)
{ 
	PRLogModuleInfo *log = ErlXPCOMLog::getLog();

	std::ostringstream oss;	

	try {
		ErlLong *_t = ErlLong::cast(term);
//				if (_t->longValue() < -2^7 || _t->longValue() >= 2^7) {
//					PR_LOG( log, PR_LOG_DEBUG, 
//						("-- parameter %i out of range i8", number));
//					oss << "Parameter " << number << " out of range";
//					throw InterfaceMismatchException(oss.str());
//				}
		miniVariant.val.i8 = _t->longValue();
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_DEBUG, 
			("-- parameter %i bad type, is not a integer (i8)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}

////////////////////////////////////////////////////////////////////////////////
void erlang_to_i16(ErlTerm* term, nsXPTCMiniVariant &miniVariant, const int number) 
	throw (InterfaceMismatchException)
{
	PRLogModuleInfo *log = ErlXPCOMLog::getLog();

	std::ostringstream oss;	

	try {
		ErlLong *_t = ErlLong::cast(term);
//				if (_t->longValue() < -2^15 || _t->longValue() >= 2^15) {
//					PR_LOG( log, PR_LOG_DEBUG, 
//						("-- parameter %i out of range i16", number));
//					oss << "Parameter " << number << " out of range";
//					throw InterfaceMismatchException(oss.str());
//				}
		miniVariant.val.i16 = _t->longValue();
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_DEBUG, 
			("-- parameter %i bad type, is not a integer (i16)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}

////////////////////////////////////////////////////////////////////////////////
void erlang_to_i32(ErlTerm* term, nsXPTCMiniVariant &miniVariant, const int number) 
	throw (InterfaceMismatchException)
{
	PRLogModuleInfo *log = ErlXPCOMLog::getLog();

	std::ostringstream oss;	

	try {
		ErlLong *_t = ErlLong::cast(term);
//				if (_t->longValue() < -2^31 || _t->longValue() >= 2^31) {
//					PR_LOG( log, PR_LOG_DEBUG, 
//						("-- parameter %i out of range i32", number));
//					oss << "Parameter " << number << " out of range";
//					throw InterfaceMismatchException(oss.str());
//				}
		miniVariant.val.i32 = _t->longValue();
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_DEBUG, 
			("-- parameter %i bad type, is not a integer (i32)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}
////////////////////////////////////////////////////////////////////////////////
// FIXME: Implement for 64 bits
void erlang_to_i64(ErlTerm* term, nsXPTCMiniVariant &miniVariant, const int number) 
	throw (InterfaceMismatchException)
{
	PRLogModuleInfo *log = ErlXPCOMLog::getLog();

	std::ostringstream oss;	

	try {
		ErlLong *_t = ErlLong::cast(term);
//				if (_t->longValue() < -2^31 || _t->longValue() >= 2^31) {
//					PR_LOG( log, PR_LOG_DEBUG, 
//						("-- parameter %i out of range i32", number));
//					oss << "Parameter " << number << " out of range";
//					throw InterfaceMismatchException(oss.str());
//				}
		miniVariant.val.i64 = _t->longValue();
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_DEBUG, 
			("-- parameter %i bad type, is not a integer (i64)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}
////////////////////////////////////////////////////////////////////////////////
void erlang_to_u8(ErlTerm* term, nsXPTCMiniVariant &miniVariant, const int number) 
	throw (InterfaceMismatchException)
{
	PRLogModuleInfo *log = ErlXPCOMLog::getLog();

	std::ostringstream oss;	

	try {
		ErlLong *_t = ErlLong::cast(term);
//				if (_t->longValue() < 0 || _t->longValue() >= (2^8)) {
//					PR_LOG( log, PR_LOG_DEBUG, 
//						("-- parameter %i out of range u8 (%i)", number, _t->longValue()));
//					oss << "Parameter " << number << " out of range";
//					throw InterfaceMismatchException(oss.str());
//				}
		miniVariant.val.u8 = _t->longValue();
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_DEBUG, 
			("-- parameter %i bad type, is not a integer (u8)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
	
}

////////////////////////////////////////////////////////////////////////////////
void erlang_to_u16(ErlTerm* term, nsXPTCMiniVariant &miniVariant, const int number) 
	throw (InterfaceMismatchException)
{
	PRLogModuleInfo *log = ErlXPCOMLog::getLog();

	std::ostringstream oss;	

	try {
		ErlLong *_t = ErlLong::cast(term);
//				if (_t->longValue() < 0 || _t->longValue() >= (2^16)) {
//					PR_LOG( log, PR_LOG_DEBUG, 
//						("-- parameter %i out of range u16", number));
//					oss << "Parameter " << number << " out of range";
//					throw InterfaceMismatchException(oss.str());
//				}

		miniVariant.val.u16 = _t->longValue();
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_DEBUG, 
			("-- parameter %i bad type, is not a integer (u16)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}

////////////////////////////////////////////////////////////////////////////////
void erlang_to_u32(ErlTerm* term, nsXPTCMiniVariant &miniVariant, const int number) 
	throw (InterfaceMismatchException)
{
	PRLogModuleInfo *log = ErlXPCOMLog::getLog();

	std::ostringstream oss;	

	try {
		ErlLong *_t = ErlLong::cast(term);
//				if (_t->longValue() < 0 || _t->longValue() >= (2^32)) {
//					PR_LOG( log, PR_LOG_DEBUG, 
//						("-- parameter %i out of range u32", number));
//					oss << "Parameter " << number << " out of range";
//					throw InterfaceMismatchException(oss.str());
//				}
		miniVariant.val.u32 = _t->longValue();
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_DEBUG, 
			("-- parameter %i bad type, is not a integer (u32)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}

////////////////////////////////////////////////////////////////////////////////
// FIXME: implement to accept really 64 bits
void erlang_to_u64(ErlTerm* term, nsXPTCMiniVariant &miniVariant, const int number) 
	throw (InterfaceMismatchException)
{
	PRLogModuleInfo *log = ErlXPCOMLog::getLog();

	std::ostringstream oss;	

	try {
		ErlLong *_t = ErlLong::cast(term);
//				if (_t->longValue() < 0 || _t->longValue() >= (2^32)) {
//					PR_LOG( log, PR_LOG_DEBUG, 
//						("-- parameter %i out of range u32", number));
//					oss << "Parameter " << number << " out of range";
//					throw InterfaceMismatchException(oss.str());
//				}
		miniVariant.val.u64 = _t->longValue();
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_DEBUG, 
			("-- parameter %i bad type, is not a integer (u64)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}


////////////////////////////////////////////////////////////////////////////////
void erlang_to_float(ErlTerm* term, nsXPTCMiniVariant &miniVariant, const int number) 
	throw (InterfaceMismatchException)
{
	PRLogModuleInfo *log = ErlXPCOMLog::getLog();

	std::ostringstream oss;	

	try {
		ErlDouble *_t = ErlDouble::cast(term);
		miniVariant.val.f = _t->doubleValue();
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_DEBUG, 
			("-- parameter %i bad type, is not a double (float)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}

////////////////////////////////////////////////////////////////////////////////
void erlang_to_double(ErlTerm* term, nsXPTCMiniVariant &miniVariant, const int number) 
	throw (InterfaceMismatchException)
{
	PRLogModuleInfo *log = ErlXPCOMLog::getLog();

	std::ostringstream oss;	

	try {
		ErlDouble *_t = ErlDouble::cast(term);
		miniVariant.val.d = _t->doubleValue();
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_DEBUG, 
			("-- parameter %i bad type, is not a double (double)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}
////////////////////////////////////////////////////////////////////////////////
void erlang_to_bool(ErlTerm* term, nsXPTCMiniVariant &miniVariant, const int number) 
	throw (InterfaceMismatchException)
{
	PRLogModuleInfo *log = ErlXPCOMLog::getLog();

	std::ostringstream oss;	

	try {
		ErlAtom *_t = ErlAtom::cast(term);
		if (_t->atomValue() == std::string("true")) {
			miniVariant.val.b = true;
		} else if (_t->atomValue() == std::string("false")) {
			miniVariant.val.b = false;
		} else {
			PR_LOG( log, PR_LOG_DEBUG, 
				("-- parameter %i bad type, is an atom but not a bool ('true'|'false')", number));
			oss << "Parameter " << number << " incorrect type";
			throw InterfaceMismatchException(oss.str());
		}
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_DEBUG, 
			("-- parameter %i bad type, is not a bool", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}
////////////////////////////////////////////////////////////////////////////////
void erlang_to_char(ErlTerm* term, nsXPTCMiniVariant &miniVariant, const int number) 
	throw (InterfaceMismatchException)
{
	PRLogModuleInfo *log = ErlXPCOMLog::getLog();

	std::ostringstream oss;	

	try {
		ErlLong *_t = ErlLong::cast(term);
		// FIXME: Check ranges
		/*
		if (_t->longValue() < 0 || _t->longValue() >= 2^8) {
			PR_LOG( log, PR_LOG_DEBUG, 
				("-- parameter %i out of range u8", number));
			oss << "Parameter " << number << " out of range";
			throw InterfaceMismatchException(oss.str());
		}
		*/
		miniVariant.val.c = _t->longValue();
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_DEBUG, 
			("-- parameter %i bad type, is not a integer (char)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}

////////////////////////////////////////////////////////////////////////////////
void erlang_to_wchar(ErlTerm* term, nsXPTCMiniVariant &miniVariant, const int number) 
	throw (InterfaceMismatchException)
{
	PRLogModuleInfo *log = ErlXPCOMLog::getLog();

	std::ostringstream oss;	

	try {
		ErlLong *_t = ErlLong::cast(term);
		// FIXME: Check ranges
		/*
		if (_t->longValue() < 0 || _t->longValue() >= 2^8) {
			PR_LOG( log, PR_LOG_DEBUG, 
				("-- parameter %i out of range u8", number));
			oss << "Parameter " << number << " out of range";
			throw InterfaceMismatchException(oss.str());
		}
		*/
		miniVariant.val.wc = _t->longValue();
	} catch (EpiBadArgument &e) {
		PR_LOG( log, PR_LOG_DEBUG, 
			("-- parameter %i bad type, is not a integer (char)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}

////////////////////////////////////////////////////////////////////////////////
void erlang_to_string(ErlTerm* term, 
	nsXPTCMiniVariant &miniVariant, 
	nsXPTCMiniVariant &miniVariantDep1, 
	nsXPTCMiniVariant &miniVariantDep2, 
	const int number) 
	throw (InterfaceMismatchException)
{
	PRLogModuleInfo *log = ErlXPCOMLog::getLog();

	std::ostringstream oss;	

	//        case nsXPTType::T_CSTRING:
	//        case nsXPTType::T_CHAR_STR:
	//        case nsXPTType::T_PSTRING_SIZE_IS: // This will be treated like cstring.
	// A simple string, extract copy it directly
	if (term->instanceOf(ERL_STRING)) {
		PR_LOG( log, PR_LOG_DEBUG, ("-- CString, cloning ErlString"));
		ErlString *_t = ErlString::cast(term);
		miniVariant.val.p = (void*) 
			nsMemory::Clone(_t->stringValue().c_str(), _t->stringValue().size());
		// init depended variants
		miniVariantDep1.val.u32 = _t->stringValue().size();
		miniVariantDep2.val.u32 = _t->stringValue().size();
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
			miniVariant.val.p = (void*) newstr;
		} catch (EpiBadArgument &e) {
			PR_LOG( log, PR_LOG_DEBUG, 
				("-- parameter %i bad type, is not an string, an element of list is not an integer (cstring)", number));
			oss << "Parameter " << number << " incorrect type";
			throw InterfaceMismatchException(oss.str());
		}
	} else {
		PR_LOG( log, PR_LOG_DEBUG, 
			("-- parameter %i bad type, is not an string (cstring)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}			

////////////////////////////////////////////////////////////////////////////////
void erlang_to_wstring(
	ErlTerm* term, 
	nsXPTCMiniVariant &miniVariant, 
	nsXPTCMiniVariant &miniVariantDep1, 
	nsXPTCMiniVariant &miniVariantDep2, 
	const int number) 
	throw (InterfaceMismatchException)
{
	PRLogModuleInfo *log = ErlXPCOMLog::getLog();

	std::ostringstream oss;	

//        case nsXPTType::T_DOMSTRING:         
//		case nsXPTType::T_WCHAR_STR:         
//        case nsXPTType::T_UTF8STRING:        
//        case nsXPTType::T_PWSTRING_SIZE_IS:
    // This will be treated like cstring.
	// A simple string, extract copy it directly
	if (term->instanceOf(ERL_STRING)) {
		PR_LOG( log, PR_LOG_DEBUG, ("-- wstring, cloning ErlString"));
		ErlString *_t = (ErlString *) term;
		int strsize = _t->stringValue().size();
		const char *srcstr = _t->stringValue().c_str();
		PRUnichar* newstr = (PRUnichar*) 
			nsMemory::Alloc(strsize*sizeof(PRUnichar)+1);
		for (int i = 0; i< strsize; i++) {
			newstr[i] = srcstr[i];
		}
		newstr[strsize] = 0;
		miniVariant.val.p = newstr;
		// init depended variants
		miniVariantDep1.val.u32 = strsize;
		miniVariantDep2.val.u32 = strsize;
	// List representation of a string 
	} else if (term->instanceOf(ERL_LIST)) {
		PR_LOG( log, PR_LOG_DEBUG, ("-- wstring, cloning ErlList content"));
		ErlList *_t = (ErlList *) term;
		int strsize = _t->arity();
		PRUnichar* newstr = (PRUnichar*) 
			nsMemory::Alloc(strsize*sizeof(PRUnichar)+1);
		try {
			for (int i = 0; i< strsize; i++) {
				// Try to get each element 
				ErlLong *element = ErlLong::cast(_t->elementAt(i));
				// FIXME: check ranges
				newstr[i] = element->longValue();
			}
			newstr[strsize] = 0;
			miniVariant.val.p = newstr;
			// init depended variants
			miniVariantDep1.val.u32 = strsize;
			miniVariantDep2.val.u32 = strsize;
		} catch (EpiBadArgument &e) {
			nsMemory::Free(newstr);
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


////////////////////////////////////////////////////////////////////////////////
void erlang_to_iid(ErlTerm* term, 
	nsXPTCMiniVariant &miniVariant, 
	const int number) 
	throw (InterfaceMismatchException)
{
	PRLogModuleInfo *log = ErlXPCOMLog::getLog();

	std::ostringstream oss;	

	// FIXME: Use other thing than a string ? :?
	if (term->instanceOf(ERL_STRING)) {
		ErlString *_t = (ErlString *) term;
		nsID *iid = (nsID*) PR_Malloc(sizeof(nsID));
		if (!iid->Parse(_t->stringValue().c_str())) {
			PR_LOG( log, PR_LOG_DEBUG, 
				("-- parameter %i bad type, iid parse error", number));
			oss << "Parameter " << number << " incorrect type (iid parse error)";
			throw InterfaceMismatchException(oss.str());
		}
		miniVariant.val.p = (void*) iid;
	} else {
		PR_LOG( log, PR_LOG_DEBUG, 
			("-- parameter %i bad type, is not an string (cstring)", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}
}

/////////////////////////////////////////////////////////////////////////////
void erlang_to_interface(ErlangORB *orb,
	nsIInterfaceInfo *interfaceInfo,
	const nsXPTParamInfo &paramInfo,
	ErlTerm *term, 
	nsXPTCMiniVariant &miniVariant, 
	nsXPTCMiniVariant &miniVariantDep1, 
	nsXPTCMiniVariant &miniVariantDep2, 
	PRUint16 methodIndex, 
	int number) 	
	throw (InterfaceMismatchException, InternalErrorException)
{
	PRLogModuleInfo *log = ErlXPCOMLog::getLog();

	std::ostringstream oss;
	
	ErlTermPtr<ErlTuple> xpcomObjPattern(
		new ErlTuple(new ErlAtom("xpcom_oid"), new ErlVariable("OID")));
	ErlTermPtr<ErlTuple> erlangObjPattern(
		new ErlTuple(new ErlAtom("erlang_oid"), new ErlVariable("OID")));
	ErlTermPtr<ErlTerm> nullObjPattern(new ErlAtom("null"));
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
		
		miniVariant.val.p = stub->getXPCOMObject();
	} else if (term->match(erlangObjPattern.get(), &binding)) {
		lfOID oid;
		nsIID *iid = 0;
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
		// get the iid 
		switch (paramInfo.GetType().TagPart()) {
		case nsXPTType::T_INTERFACE:
			nsresult rv;
			// Get the iid, from type (T_INTERFACE) ... 
			if (NS_FAILED(rv = 
					interfaceInfo->GetIIDForParam(methodIndex, &paramInfo, &iid))) {
				PR_LOG( log, PR_LOG_DEBUG, 
					("-- failure getting IID for param %i", number));
				throw XPCOMException("Failure getting interface info", rv);
			}
		case nsXPTType::T_INTERFACE_IS:      /* fall through */
			if (iid == 0) {
				iid = (nsIID*) miniVariantDep1.val.p;
			}
		}
		// Search if the proxy exists
		proxy = orb->searchProxy(oid, *iid);
		if (proxy) {
			// If exists, use it
			miniVariant.val.p = proxy;
		} else {
			// Else create and register a new one
			PR_LOG( log, PR_LOG_DEBUG, 
				("-- registering new proxy for oid=%i and iid=%s", 
				 oid, iid->ToString()));
			proxy = new ErlXPCOMProxy(oid, *iid);
			orb->registerProxy(proxy);
			miniVariant.val.p = proxy;
		}
	} else if (term->match(nullObjPattern.get())) {
		miniVariant.val.p = nsnull;
	} else {
		PR_LOG( log, PR_LOG_DEBUG, 
			("-- parameter %i bad type, is not an interface", number));
		oss << "Parameter " << number << " incorrect type";
		throw InterfaceMismatchException(oss.str());
	}

}



/////////////////////////////////////////////////////////////////////////////
/* 
 * Gets the XPCOM nsXPTCVariant from its erlang representation.
 * @param term Erlang term from where extract the info
 * @param miniVariant mini variant to where write the info
 * @param miniVariant
 * @param type nsXPTType info for this parameter
 * @param number parameter number (for error notification)
 * @param isAllocated return if the Variant contains data that must be freed
 * @throws 
 */
void ErlXPCOMMarshallerUnmarshaller::Erlang2xptcMiniVariant(
		ErlTerm* term, 
	 	nsXPTCMiniVariant &miniVariant, 
	 	nsXPTCMiniVariant &miniVariantDep1, 
	 	nsXPTCMiniVariant &miniVariantDep2, 
		const nsXPTParamInfo &paramInfo,
		bool &isAllocated,
		const int number) 
{
	std::ostringstream oss;
	
	isAllocated = false; // by default is not allocated
	
	// Check the variant type
	switch (paramInfo.GetType().TagPart()) {
        case nsXPTType::T_I8:
			erlang_to_i8(term, miniVariant, number);
			break;
		case nsXPTType::T_I16:            
			erlang_to_i16(term, miniVariant, number);
			break;
		case nsXPTType::T_I32:         
			erlang_to_i32(term, miniVariant, number);
			break;
        case nsXPTType::T_I64:
			// FIXME: 64 bits not really supported by ErlLong limitations
			erlang_to_i64(term, miniVariant, number);
			break;
		case nsXPTType::T_U8:
			erlang_to_u8(term, miniVariant, number);
			break;
		case nsXPTType::T_U16:
			erlang_to_u16(term, miniVariant, number);
			break;
        case nsXPTType::T_U32:
			erlang_to_u32(term, miniVariant, number);
			break;
        case nsXPTType::T_U64:
			// FIXME: 64 bits not really supported by ErlLong limitations
			erlang_to_u64(term, miniVariant, number);
			break;
        case nsXPTType::T_FLOAT:
			erlang_to_float(term, miniVariant, number);
			break;
        case nsXPTType::T_DOUBLE:
			erlang_to_double(term, miniVariant, number);
			break;
        case nsXPTType::T_BOOL:
			erlang_to_bool(term, miniVariant, number);
			break;
        case nsXPTType::T_CHAR:  /* fall through */
			erlang_to_char(term, miniVariant, number);
			break;
        case nsXPTType::T_WCHAR:
			erlang_to_wchar(term, miniVariant, number);
			break;
        case nsXPTType::T_CSTRING:
        case nsXPTType::T_CHAR_STR:
        case nsXPTType::T_PSTRING_SIZE_IS: // This will be treated like cstring.
			erlang_to_string(term, miniVariant, miniVariantDep1, miniVariantDep2, number);
			isAllocated = true;
			break;
		case nsXPTType::T_WCHAR_STR:         
        case nsXPTType::T_PWSTRING_SIZE_IS:
			erlang_to_wstring(term, miniVariant, miniVariantDep1, miniVariantDep2, number);
			isAllocated = true;
			break;
        case nsXPTType::T_INTERFACE:         /* fall through */
        case nsXPTType::T_INTERFACE_IS:      /* fall through */
			erlang_to_interface(orb, interfaceInfo, paramInfo, term, 
								miniVariant, miniVariantDep1, miniVariantDep2, 
								methodIndex, number);
			break; 
        case nsXPTType::T_IID:               /* fall through */
			erlang_to_iid(term, miniVariant, number);	
			isAllocated = true;
			break;
        case nsXPTType::T_VOID:              /* fall through */
        case nsXPTType::T_DOMSTRING:         
        case nsXPTType::T_UTF8STRING:        
        case nsXPTType::T_ARRAY:             /* fall through */

        default:                             
			PR_LOG( log, PR_LOG_DEBUG, 
				("-- parameter %i unimplemented type %hi", number, paramInfo.GetType().TagPart()));
			oss << "Parameter " << number << " unimplemented type";
			throw InterfaceMismatchException(oss.str());
	}

}


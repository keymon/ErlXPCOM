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

#include "ErlXPCOMLocalMethod.h"

using namespace erlxpcom;

ErlXPCOMLocalMethod::ErlXPCOMLocalMethod(ErlXPCOMMethodInfo *_methodInfo):
	ErlXPCOMMethod(_methodInfo)
{
	init();
}

ErlXPCOMLocalMethod::~ErlXPCOMLocalMethod() 
{
	free();
}

void ErlXPCOMLocalMethod::init() {
	int paramcount = methodInfo->getParamCount();
	// allocate the variants array
	variants = (nsXPTCVariant*) PR_Malloc(paramcount*sizeof(nsXPTCVariant));

	nsXPTCMiniVariant miniVariant;
	for (int i=0; i< paramcount; i++) {
		nsXPTParamInfo paramInfo = methodInfo->getParamInfo(i);
		// If is out parameter, alloc the internal minivariant
		if (methodInfo->getParamInfo(i).IsOut()) {
			nsXPTCMiniVariant *miniVariant2 = new nsXPTCMiniVariant();
			miniVariant.val.p = miniVariant2;
			// It's necesary indicate that the ptr in variant
			// is the real data or InvokeMethod will use a null pointer
			// Of cuorse, this is not documented! :-/
			variants[i].Init(miniVariant, paramInfo.GetType(), 
				paramInfo.flags | nsXPTCVariant::PTR_IS_DATA);
		// If is a dipper, allocate the container object and init like an Out
		} else if (methodInfo->getParamInfo(i).IsDipper()) {
			switch (methodInfo->getParamTag(i)) {
				case nsXPTType::T_ASTRING:
				case nsXPTType::T_DOMSTRING:
					miniVariant.val.p = new nsString();
					variants[i].Init(miniVariant, paramInfo.GetType(), 
						paramInfo.flags | 
						nsXPTCVariant::PTR_IS_DATA |
						nsXPTCVariant::VAL_IS_DOMSTR);
					break;  
				case nsXPTType::T_UTF8STRING:
				case nsXPTType::T_CSTRING:
					miniVariant.val.p = new nsCString();
					variants[i].Init(miniVariant, paramInfo.GetType(), 
						paramInfo.flags | 
						nsXPTCVariant::PTR_IS_DATA |
						nsXPTCVariant::VAL_IS_CSTR);
					break;
				default:
					PR_LOG( log, PR_LOG_DEBUG, 
							(" -- unhandled dipper type"));
					break;
			}
		// If not, simply init the variant
		} else {
			miniVariant.val.p = NULL;
			variants[i].Init(miniVariant, paramInfo.GetType(), 
							 paramInfo.flags);
		}
	}
}

// Check  if this type is data that must be freed 
bool mustBeDeleted(PRUint8 tag) {
	switch(tag) {
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
		case nsXPTType::T_ASTRING:           /* fall through */   				
			return true;
			break;
		default:
			return false;
			break;
	} 
}

void ErlXPCOMLocalMethod::free_concrete_variant(
	int paramNumber, PRUint8 typetag, nsXPTCMiniVariant &variant) 
{
	// If it's an array, check if the contained data must be 
	// deleted 
	if (typetag == nsXPTType::T_ARRAY) {
		PRUint8 elementTag = methodInfo->getTypeOfArray(paramNumber).TagPart();
		PR_LOG( log, PR_LOG_DEBUG, (" `-> freeing elements in the array"));

		if (mustBeDeleted(elementTag)) {
			// get the size of the array from the dependent param
			int dep1, dep2;
			methodInfo->getDependentParamNumbers(paramNumber, &dep1, &dep2);
			unsigned int arraySize = variants[dep1].val.u32;
			
			for (unsigned int i=0; i< arraySize; i++) {
				void *p = ((void**)variants[paramNumber].val.p)[i];
				if (p) PR_Free(p);
			}
		}
	}
	if (mustBeDeleted(methodInfo->getParamTag(paramNumber))) {
		PR_LOG( log, PR_LOG_DEBUG, 
			(" `-> freeing real data of in parameter pointer in variant 0x%08x", 
				variants[paramNumber].val.p));
		PR_Free(variants[paramNumber].val.p);
	}
}

void ErlXPCOMLocalMethod::free() {
	PR_LOG(log, PR_LOG_DEBUG, ("ErlXPCOMLocalMethod::free(), freeing variants"));

	// For each param
	int paramcount = methodInfo->getParamCount();
	for (int i=0; i<paramcount; i++) {
		if (variants[i].IsPtrData()) {
			free_concrete_variant(i, variants[i].type.TagPart(),
				*((nsXPTCMiniVariant *) variants[i].ptr));
			PR_LOG( log, PR_LOG_DEBUG, 
				(" `-> freeing variant in out parameter in 0x%08x", 
					variants[i].ptr));
			PR_Free(variants[i].ptr);
		} else {
			free_concrete_variant(i, variants[i].type.TagPart(), variants[i]);
		}
	}
	PR_LOG( log, PR_LOG_DEBUG, (" `-> freeing variant array 0x%08x", variants));
	PR_Free(variants);
	
}
	
nsXPTCMiniVariant* ErlXPCOMLocalMethod::getMiniVariant(int paramNumber) {
	// Small hack: 
	// If it is an dipper parameter, put the container in the temporan variant
	if (methodInfo->getParamInfo(paramNumber).IsDipper()) {
		tmpVariant.val.p = variants[paramNumber].ptr;
		return &tmpVariant;
	// If it is an out parameter, return the data minivariant in the ptr
	} if (methodInfo->isParamOut(paramNumber)) {
		return (nsXPTCMiniVariant*) variants[paramNumber].ptr;
	} else {
		return &variants[paramNumber];
	}
}


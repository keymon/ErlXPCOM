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
#include <prmem.h>
#include <nsMemory.h>
#include <nsISupports.h>

#include "ErlXPCOMMethodInfo.h"
#include "ErlXPCOMLog.h"

using namespace erlxpcom;

ErlXPCOMMethodInfo::ErlXPCOMMethodInfo(
	nsIInterfaceInfo* _interfaceInfo, 
	const nsXPTMethodInfo* _methodInfo, 
	PRUint16 _methodIndex)
	throw (XPCOMException): 
		log(ErlXPCOMLog::getLog()),
		interfaceInfo(_interfaceInfo), 
		methodInfo(_methodInfo), 
		methodIndex(_methodIndex)
{
	int paramcount = getParamCount();
	// Alloc data
	// Force the allocation of param info. It is a flyweight object, and
	// has no data, but it has an private constructuor, so we allocate
	// a array of superclass.
	
	paramInfo = (nsXPTParamInfo *) new XPTParamDescriptor[paramcount];
	
	dependentParams1 = new int[paramcount];
	dependentParams2 = new int[paramcount];
	dependedParams = new int[paramcount];
	inParamIndex = new int[paramcount];
	outParamIndex = new int[paramcount];
	inParamIndexNoSize = new int[paramcount];
	outParamIndexNoSize = new int[paramcount];
	
	// Init infos and depend arrays
	for (int i = 0; i<paramcount; i++) {
		dependentParams1[i] = dependentParams2[i] = dependedParams[i] = i;
	}
	paramInCount = 0;
	paramOutCount = 0;
	for (int i = 0; i<paramcount; i++) {
		nsresult rv;
		PRUint8 argnum;
		paramInfo[i] = methodInfo->GetParam(i);

		// Create indexes of in/out params
		if (isParamIn(i)) {
			inParamIndex[i] = paramInCount++;
		} else {
			inParamIndex[i] = -1;
		}
		if (isParamOut(i)) {
			outParamIndex[i] = paramOutCount++;
		} else {
			outParamIndex[i] = -1;
		}
		
		// Init depend array 
		switch (paramInfo[i].GetType().TagPart()) {
			case nsXPTType::T_ARRAY:
				rv = interfaceInfo->GetSizeIsArgNumberForParam(methodIndex, 
					&paramInfo[i], 0, &argnum);
				// If failed, is not size_is :?
				if (NS_SUCCEEDED(rv)) {
					dependentParams1[i] = argnum;
					dependedParams[argnum] = i;
				} else {
					PR_LOG( log, PR_LOG_ERROR, 
						("ErlXPCOMMethodInfo: failure getting dependent (size_is) param for param %i", i));
					throw XPCOMException("Failure getting interface info", rv);
				}
				break;
			case nsXPTType::T_PSTRING_SIZE_IS:
			case nsXPTType::T_PWSTRING_SIZE_IS:	
				rv = interfaceInfo->GetSizeIsArgNumberForParam(methodIndex, 
					&paramInfo[i], 0, &argnum);
				// If failed, is not size_is :?
				if (NS_SUCCEEDED(rv)) {
					dependentParams1[i] = argnum;
					dependedParams[argnum] = i;
				} else {
					PR_LOG( log, PR_LOG_ERROR, 
						("ErlXPCOMMethodInfo: failure getting dependent (size_is) param for param %i", i));
					throw XPCOMException("Failure getting interface info", rv);
				}
				// If failed, is not length_is :?
				rv = interfaceInfo->GetLengthIsArgNumberForParam(methodIndex, 
						&paramInfo[i], 0, &argnum);
				if (NS_SUCCEEDED(rv)) {
					dependentParams2[i] = argnum;
					dependedParams[argnum] = i;
				} else {
					PR_LOG( log, PR_LOG_ERROR, 
						("ErlXPCOMMethodInfo: failure getting dependent (length_is) param for param %i", i));
					throw XPCOMException("Failure getting interface info", rv);
				}
				break;
			case nsXPTType::T_INTERFACE_IS:
				rv = interfaceInfo->GetInterfaceIsArgNumberForParam(methodIndex, 
						&paramInfo[i], &argnum);
				if (NS_SUCCEEDED(rv)) {
					dependentParams1[i] = argnum;
					dependedParams[argnum] = i;
				} else {
					PR_LOG( log, PR_LOG_ERROR, 
						("ErlXPCOMMethodInfo: failure getting dependent param (iid_is) for param %i", i));
					throw XPCOMException("Failure getting interface info", rv);
				}
		}


	}

	int paramInNoSizeCount = 0;
	int paramOutNoSizeCount = 0;
	paramInSizeCount = 0;
	paramOutSizeCount = 0;
	for (int i = 0; i<paramcount; i++) {
		if (isParamIn(i) && !isDependedSizeLength(i)) {
			inParamIndexNoSize[i] = paramInNoSizeCount++;
		} else if (isParamOut(i) && !isDependedSizeLength(i)) {
			outParamIndexNoSize[i] = paramOutNoSizeCount++;
		}
		if (isParamIn(i) && isDependedSizeLength(i)) {
			inParamIndexNoSize[i] = -1;
			paramInSizeCount++;
		} else if (isParamOut(i) && isDependedSizeLength(i)) {
			outParamIndexNoSize[i] = -1;
			paramOutSizeCount++;
		}
	}
	
	
}
	
ErlXPCOMMethodInfo::~ErlXPCOMMethodInfo() {
	delete[] paramInfo;
	delete[] dependentParams1;
	delete[] dependentParams2;
	delete[] dependedParams;
	delete[] inParamIndex;
	delete[] outParamIndex;
	delete[] inParamIndexNoSize;
	delete[] outParamIndexNoSize;
}

nsIID *ErlXPCOMMethodInfo::getIID(int number) {
	nsXPTParamInfo paramInfo = getParamInfo(number);
	
	/** Get the iid if it is an interface or an array of interfaces  */
	if (paramInfo.GetType().TagPart() == nsXPTType::T_INTERFACE ||
		(paramInfo.GetType().TagPart() == nsXPTType::T_ARRAY &&
		 getTypeOfArray(number).TagPart() == nsXPTType::T_INTERFACE)) {
		nsIID *iid;
		nsresult rv;
		if (NS_FAILED(rv = 
				interfaceInfo->GetIIDForParam(methodIndex, &paramInfo, &iid))) {
			PR_LOG(log, PR_LOG_ERROR, 
				("ErlXPCOMMethodInfo::getIID(paramNumber=%i) failed for method=%s", 
					number, getName().c_str()));
			return 0;
		}
		return iid;
	} else {
		return 0;
	}
}

nsXPTType ErlXPCOMMethodInfo::getTypeOfArray(int number) {
	nsXPTParamInfo paramInfo = getParamInfo(number);
	
	/** Get the iid if it is an interface */
	if (paramInfo.GetType().TagPart() == nsXPTType::T_ARRAY) {
		nsXPTType retType;
		nsresult rv;
		if (NS_FAILED(rv = 
				interfaceInfo->GetTypeForParam(methodIndex, 
					&paramInfo, 1, &retType))) { // the 1 means "the internal type"
			PR_LOG(log, PR_LOG_ERROR, 
				("ErlXPCOMMethodInfo::getParamInfoOfArray(paramNumber=%i) failed for method=%s", 
					number, getName().c_str()));
			return 0;
		}
		return retType;
	} else {
		return 0;
	}

}

	
//COMO ODIO O API DE MERDA DE MOZILLA!
PRUint32 erlxpcom::getSizeForTagType( PRUint8 tag)
{
	PRUint32 ret;
	switch (tag) {
	case nsXPTType::T_U8:
	case nsXPTType::T_I8:
		ret = sizeof(PRInt8); 
		break;
	case nsXPTType::T_I16:
	case nsXPTType::T_U16:
		ret = sizeof(PRInt16); 
		break;
	case nsXPTType::T_I32:
	case nsXPTType::T_U32:
		ret = sizeof(PRInt32); 
		break;
	case nsXPTType::T_I64:
	case nsXPTType::T_U64:
		ret = sizeof(PRInt64); 
		break;
	case nsXPTType::T_FLOAT:
		ret = sizeof(float); 
		break;
	case nsXPTType::T_DOUBLE:
		ret = sizeof(double); 
		break;
	case nsXPTType::T_BOOL:
		ret = sizeof(PRBool); 
		break;
	case nsXPTType::T_CHAR:
		ret = sizeof(char); 
		break;
	case nsXPTType::T_WCHAR:
		ret = sizeof(PRUnichar); 
		break;
	default:
		ret = sizeof( void * );
		break;
	}
	return ret;
}


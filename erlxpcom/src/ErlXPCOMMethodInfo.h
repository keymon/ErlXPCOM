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
#ifndef __ERLXPCOMMETHODINFO_H
#define __ERLXPCOMMETHODINFO_H

#include <nsCOMPtr.h>
#include <xptcall.h>
#include <xptinfo.h>
#include <prtypes.h>
#include <nsIInterfaceInfo.h>

//#include "ErlangOrb.h"
#include "ErlXPCOMException.h"
#include "ErlXPCOMLog.h"

namespace erlxpcom {
/** 
 * This class enncapsulates all the information of a method, and is an 
 * abstration of XPCOM interface info API
 */
class ErlXPCOMMethodInfo {
public:
	/**
	 * Create a new ErlXPCOMMethodInfo from given information
	 */
	ErlXPCOMMethodInfo(nsIInterfaceInfo* _interfaceInfo, 
					   const nsXPTMethodInfo* _methodInfo, 
					   PRUint16 _methodIndex)
		throw (XPCOMException);
	
	virtual ~ErlXPCOMMethodInfo();
	
	/**
	 * Get the method name
	 */
	inline std::string getName() {
		return std::string(methodInfo->GetName());
	}
	
	inline PRUint16 getMethodIndex() {
		return methodIndex;
	}
	
	inline int getParamCount() {
		return methodInfo->GetParamCount();
	}
	
	/** Get the number of Out params */
	inline int getParamOutCount() {
		return paramOutCount;
	}

	/** Get the number of In params */
	inline int getParamInCount() {
		return paramInCount;
	}

	/** Get the number of Out params that are not size_is or length_is */
	inline int getParamOutSizeCount() {
		return paramOutSizeCount;
	}

	/** Get the number of In params that are not size_is or length_is */
	inline int getParamInSizeCount() {
		return paramInSizeCount;
	}

	/**
	 * Check if is getter
	 */
	inline bool isGetter() {
		return methodInfo->IsGetter();
	}

	/**
	 * Check if is setter
	 */
	inline bool isSetter() {
		return methodInfo->IsSetter();
	}
	
	inline bool isParamIn(int paramNumber) {
		return paramInfo[paramNumber].IsIn() && !paramInfo[paramNumber].IsDipper();
	}

	inline bool isParamOut(int paramNumber) {
		return paramInfo[paramNumber].IsOut() || paramInfo[paramNumber].IsDipper();
	}
	
	/** 
	 * Get the out parameter number for a param number. If it is not
	 * an out parameter, returns -1
	 */
	inline int getOutNumber(int paramNumber) {
		return outParamIndex[paramNumber];
	}

	/** 
	 * Get the in parameter number for a param number. If it is not
	 * an in parameter, returns -1
	 * Size and length are not counted
	 */
	inline int getInNumberNoSize(int paramNumber) {
		return inParamIndexNoSize[paramNumber];
	}
	
	/** 
	 * Get the out parameter number for a param number. If it is not
	 * an out parameter, returns -1
	 * Size and length are not counted
	 */
	inline int getOutNumberNoSize(int paramNumber) {
		return outParamIndexNoSize[paramNumber];
	}

	/** 
	 * Get the in parameter number for a param number. If it is not
	 * an in parameter, returns -1
	 */
	inline int getInNumber(int paramNumber) {
		return inParamIndex[paramNumber];
	}
	

	inline uint8 getParamTag(int paramNumber) {
		return paramInfo[paramNumber].GetType().TagPart();
	}
	
	inline nsXPTParamInfo getParamInfo(int paramNumber) {
		return paramInfo[paramNumber];
	}
	
	/**
	 * Check if this param depends on other
	 */
	inline bool isDependent(int paramNumber) {
		return paramInfo[paramNumber].GetType().IsDependent();
	}
	
	/**
	 * Check if this param is dependend by other (p.ex. it is the IID of iid_is)
	 */
	inline bool isDepended(int paramNumber) {
		return (paramNumber != dependedParams[paramNumber]);
	}
	
	/**
	 * Check if this param is the size or length of other 
	 */
	inline bool isDependedSizeLength(int paramNumber) {
		int depended = dependedParams[paramNumber];
		if (paramNumber != depended) {
			return (getParamTag(depended) == nsXPTType::T_ARRAY ||
					getParamTag(depended) == nsXPTType::T_PSTRING_SIZE_IS ||
				    getParamTag(depended) == nsXPTType::T_PWSTRING_SIZE_IS);
		} else {
			return false;
		}
	}
		
	/**
	 * Get the number of the depended params. 
	 * It will init them to paramNumber if no dependent param exists
	 */
	inline void getDependentParamNumbers(int paramNumber, 
									     int* depParamNumber1, 
									     int* depParamNumber2) 
	{
		*depParamNumber1 = dependentParams1[paramNumber];
		*depParamNumber2 = dependentParams2[paramNumber];
	}
							   
	/**
	 * Gets the param that depends on this param. It will return 
	 * paramNumber if no param depends on it.
	 */
	inline int getDependedParamNumber(int paramNumber) {
		return dependedParams[paramNumber];
	}
	
	/**
	 * Returns the IID of the given param if the param is an interface.
	 * It returns a pointer to a new nsIID that must be deleted, or null if
	 * the param is not an interface.
	 */
	nsIID *getIID(int number);
	
	/**
	 * Returns the type info of the inner type in a array. 
	 * It returns the pointer to the nsXPTParamInfo that must NOT be deleted, 
	 * or null if the param is not an array.
	 */
	nsXPTType getTypeOfArray(int number);
	
public:
	PRLogModuleInfo *log;

	nsCOMPtr<nsIInterfaceInfo> interfaceInfo;
	const nsXPTMethodInfo* methodInfo;
	PRUint16 methodIndex;

	nsXPTParamInfo* paramInfo;
	int* dependentParams1;
	int* dependentParams2;
	int* dependedParams;
	// Index param number -> in param number
	int* inParamIndex;
	// Index param number -> out param number
	int* outParamIndex;

	// Index param number -> in param number (size_is and length_is ignored)
	int *inParamIndexNoSize;
	// Index param number -> out param number (size_is and length_is ignored)
	int *outParamIndexNoSize;
	
	int paramInCount;
	int paramOutCount;
	int paramInSizeCount;
	int paramOutSizeCount;

	
};

/**
 * Returns the size of an elemente. Used in arrays. 
 * Forward declaration, implemented
 */
extern PRUint32 getSizeForTagType(PRUint8 tag);

}
#endif // __ERLXPCOMMETHODINFO_H



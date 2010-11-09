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
#ifndef __ERLXPCOMMETHOD_H
#define __ERLXPCOMMETHOD_H

#include <prmem.h>

#include "ErlXPCOMMethodInfo.h"

#include "ErlXPCOMLog.h"


namespace erlxpcom {

class ErlXPCOMMethodVisitor;

/**
 * This struct contains all the info of param:
 *  - the associated methodInfo
 *	- the param position in the list of params
 *  - the In param position if is an in parameter (if is not In, -1)
 *  - the Out param position if is an out parameter (if is not out, -1)
 *  - the minivariant
 *  - the paramInfo
 */
struct ErlXPCOMParam {
	/**
	 * Construct a param in a method
	 */
	inline ErlXPCOMParam(ErlXPCOMMethodInfo *_methodInfo, 
			  const int _number,  
			  nsXPTCMiniVariant *_miniVariant):
				methodInfo(_methodInfo),
				number(_number), 
				inNumber(methodInfo->getInNumber(number)), 
				outNumber(methodInfo->getOutNumber(number)),
				inNumberNoSize(methodInfo->getInNumberNoSize(number)), 
				outNumberNoSize(methodInfo->getOutNumberNoSize(number)),
				paramInfo(methodInfo->getParamInfo(number)), 
				miniVariant(_miniVariant), 
				iid(methodInfo->getIID(number)),
				tag(paramInfo.GetType().TagPart()), 
				isArrayElement(false),
				arrayElementType(methodInfo->getTypeOfArray(number)),
				arrayElementNumber(-1)
	{
	}

	/**
	 * Construct a param for a element of an array. 
	 * All info refers to the array param, except:
	 * tag, minivariant, arrayElementNumber   
	 */
	inline ErlXPCOMParam(ErlXPCOMParam &param, 
						 const int _arrayElementNumber,  
						 uint8 arrayElementTag,
						 nsXPTCMiniVariant *_miniVariant):
				methodInfo(param.methodInfo),
				number(param.number), 
				inNumber(param.inNumber), 
				outNumber(param.outNumber),
				inNumberNoSize(param.inNumberNoSize), 
				outNumberNoSize(param.outNumberNoSize),
				paramInfo(param.paramInfo), 
				miniVariant(_miniVariant), 
				iid(methodInfo->getIID(number)),
				tag(arrayElementTag), 
				isArrayElement(true),
				arrayElementType(param.arrayElementType), 
				arrayElementNumber(_arrayElementNumber)
	{
	}
	
	std::string toString() {
		std::ostringstream oss;
		oss << "ErlXPCOMParam{number=" << number <<
			   ", inNumber=" << inNumber <<
			   ", outNumber=" << outNumber << 
			   ", miniVariant=" << std::hex << miniVariant;
		if (iid) {
			char* iidstr = iid->ToString();
			oss << ", iid=" << iidstr;
			PR_Free(iidstr);
		} 
		oss << "}";
		return oss.str();
	}	
			
	ErlXPCOMMethodInfo *methodInfo;
	const int number;
	const int inNumber; 
	const int outNumber;
	const int inNumberNoSize; 
	const int outNumberNoSize;

	const nsXPTParamInfo paramInfo;
	nsXPTCMiniVariant *miniVariant;
	nsIID *iid;

	uint8 tag; //  type tag
	
	bool isArrayElement;
	nsXPTType arrayElementType;
	const int arrayElementNumber;

	
};

/**
 * This class represents a method, and encasulates the methodinfo and 
 * parameters (in and out) of a call.
 * This class implements the concrete object of a visitor pattern, and will
 * be visited by the ErlXPCOMMethodVisitor class. When visited, this 
 * class will check the type of each parameter, passing the pointer to the
 * appropiate MiniVariants (of param and dependend params). 
 * To get the pointer to minivariants, this class defines a template method,
 * getMiniVariant(int paramNumber), that must be implemented by concrete 
 * subclasses.
 *
 * The visitor can modify, or not, the minivariants.
 */
class ErlXPCOMMethod {
public:
	inline ErlXPCOMMethod(ErlXPCOMMethodInfo *_methodInfo): 
		log(ErlXPCOMLog::getLog()),
		methodInfo(_methodInfo)
	{}

	inline virtual ~ErlXPCOMMethod() {}

	inline ErlXPCOMMethodInfo* getMethodInfo() const {
		return methodInfo;
	}


	/** 
	 * Get the minivariant pointer for parameter number paramNumber.
	 * This method must return a pointer to REAL data minivariant!
	 */
	virtual nsXPTCMiniVariant* getMiniVariant(int paramNumber) = 0;
	
	/**
	 * This method will iterate the visitor over all the in params, 
	 * except the dependents (size_is, length_is, iid_is).
	 */
	void processInParams(ErlXPCOMMethodVisitor &visitor) 
		throw (InterfaceMismatchException, InternalErrorException);

	/**
	 * This method will iterate the visitor over all the out params, 
	 * except the dependents (size_is, length_is, iid_is).
	 */
	void processOutParams(ErlXPCOMMethodVisitor &visitor) 
		throw (InterfaceMismatchException, InternalErrorException);

protected:	
	PRLogModuleInfo *log;
	ErlXPCOMMethodInfo *methodInfo;
		
private:
	/**
	 * Process a concrete param number
	 */
	void processParam(ErlXPCOMMethodVisitor &visitor,
				      int paramNumber) 
		throw (InterfaceMismatchException, InternalErrorException);
	

	
};

}
#endif // __ERLXPCOMMETHOD_H

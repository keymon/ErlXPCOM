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
#include <memory>
#include <sstream>

#include "ErlXPCOMMethod.h"
#include "ErlXPCOMMethodVisitor.h"

#include "ErlXPCOMLog.h"

using namespace erlxpcom;


void ErlXPCOMMethod::processInParams(ErlXPCOMMethodVisitor &visitor) 
	throw (InterfaceMismatchException, InternalErrorException) 
{
	PR_LOG( log, PR_LOG_DEBUG, 
		("ErlXPCOMMethod::processInParams(method=%s, visitor=0x%08x)", 
			methodInfo->getName().c_str(), &visitor));
			
	for (int i=0; i< methodInfo->getParamCount(); i++) {
		// Process first all the iids depended. We need them when processing
		// interfaces
		if (methodInfo->isParamIn(i) && methodInfo->isDepended(i) &&
			methodInfo->getParamTag(i) == nsXPTType::T_IID) {
			processParam(visitor, i);
		}
	}
	for (int i=0; i< methodInfo->getParamCount(); i++) {
		// If is an undepended in param, process it 
		if (methodInfo->isParamIn(i) && !methodInfo->isDepended(i)) {
			processParam(visitor, i);
		}
	}
}


void ErlXPCOMMethod::processOutParams(ErlXPCOMMethodVisitor &visitor) 
	throw (InterfaceMismatchException, InternalErrorException) 
{
	PR_LOG( log, PR_LOG_DEBUG, 
		("ErlXPCOMMethod::processOutParams(method=%s, visitor=0x%08x)", 
			methodInfo->getName().c_str(), &visitor));

	for (int i=0; i< methodInfo->getParamCount(); i++) {
		// Process first all the iids dependent. We need them when processing
		// interfaces
		if (methodInfo->isParamOut(i) && methodInfo->isDepended(i) &&
			methodInfo->getParamTag(i) == nsXPTType::T_IID) {
			processParam(visitor, i);
		}
	}
	for (int i=0; i< methodInfo->getParamCount(); i++) {
		// If is an undepended out  param, process it 
		if (methodInfo->isParamOut(i) 
			&& !methodInfo->isDepended(i)) {
			processParam(visitor, i);
		}
	}
}
	
/**
 * Process a concrete param number
 */
void ErlXPCOMMethod::processParam(ErlXPCOMMethodVisitor &visitor, int paramNumber) 
	throw (InterfaceMismatchException, InternalErrorException) 
{
	// Get the params
	int paramNumberDep1;
	int paramNumberDep2;
	methodInfo->getDependentParamNumbers(paramNumber, 
					&paramNumberDep1, &paramNumberDep2);

	std::auto_ptr<ErlXPCOMParam> param(
		new ErlXPCOMParam(methodInfo, paramNumber, getMiniVariant(paramNumber)));
	std::auto_ptr<ErlXPCOMParam> paramDep1(
		paramNumberDep1 == paramNumber?
			NULL: 
			new ErlXPCOMParam(methodInfo, paramNumberDep1, 
			                  getMiniVariant(paramNumberDep1)));
	std::auto_ptr<ErlXPCOMParam> paramDep2(
		paramNumberDep2 == paramNumber?
			NULL: 
			new ErlXPCOMParam(methodInfo, paramNumberDep2, 
			                  getMiniVariant(paramNumberDep2)));	
	
	visitor.do_visit(param.get(), paramDep1.get(), paramDep2.get());
}

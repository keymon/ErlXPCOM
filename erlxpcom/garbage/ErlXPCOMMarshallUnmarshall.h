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
#ifndef __ERLXPCOMMARSHALLUNMARSHALL_H
#define __ERLXPCOMMARSHALLUNMARSHALL_H

/*
Defined mapping for IDL-XPCOM-Erlang types:
 IDL  	          C++ mapping       Erlang (EPI)
void                void            -
boolean 	        PRBool          ErlAtom ("true", "false")
octet 	            PRUint8         ErlLong
short 	            PRInt16         ErlLong
long 	            PRInt32         ErlLong 
long long 	        PRInt64         ErlLong
unsigned short 	    PRUint16        ErlLong
unsigned long 	    PRUint32        ErlLong
unsigned long long 	PRUint64        ???? 
float 	            float           ErlDouble
double 	            double          ErlDouble
char 	            char            ErlLong
wchar 	            PRUnichar       ErlLong
string 	            char*           ErlList/ErlString
wstring 	        PRUnichar*      ErlList


*/

#include <epi.hpp>

#include <nsCOMPtr.h>
#include <xptcall.h>
#include <xptinfo.h>
#include <prtypes.h>
#include <nsIInterfaceInfo.h>

#include "ErlangOrb.h"
#include "ErlXPCOMException.h"
#include "ErlXPCOMLog.h"

namespace erlxpcom {

using namespace epi::type;
using namespace epi::error;

class ErlXPCOMMarshallerUnmarshaller {
public:
	ErlXPCOMMarshallerUnmarshaller(ErlangORB* _orb, 
								   nsIInterfaceInfo* _interfaceInfo, 
								   const nsXPTMethodInfo* _methodInfo, 
								   PRUint16 _methodIndex);
	
	/**
	 * Unmarshall the input parameters to nsXPTCVariant array. 
	 * Used in Erlang-XPCOM calls. The allocated array should be freed
	 * using freeXPTCVariant() method
	 * @param inParams ErlList with the in parameters from Erlang call
	 * @return A new nsXPTCVariant* array. Must be deleted.
	 * @throw ErlXPCOMInterfaceMismatchException if the parameters does not
	 * complaint the interface.
	 */
	nsXPTCVariant* unmarshallInParams(ErlList* inParams) 
		throw (InterfaceMismatchException, InternalErrorException);

	/**
	 * Marshall the output from a nsXPTCVariant array. 
	 * Used in Erlang-XPCOM calls.
	 * @param params nsXPTCVariant* array with the output (and input) parameters.
	 * @return a new ErlList with erlang out parameters
	 * @throw ErlXPCOMInterfaceMismatchException if the parameters does not
	 * complaint the interface.
	 */
	ErlList* marshallOutParams(nsXPTCVariant* params)
		throw (InterfaceMismatchException, InternalErrorException);
		
	/**
	 * Marshall the input parameters from a nsXPTCVariant array. 
	 * used in XPCOM-Erlang calls
	 * @param params nsXPTCMiniVariant* array with the input (and output) parameters.
	 * @return a new ErlList with erlang in parameters
	 * @throw ErlXPCOMInterfaceMismatchException if the parameters does not
	 * complaint the interface.
	 */
	ErlList* marshallInParams(nsXPTCMiniVariant* params)
			throw (InterfaceMismatchException);

	/**
	 * Unmarshall the output parameters to nsXPTCVariant array. 
	 * Used in XPCOM-Erlang calls.
	 * @param params nsXPTCMiniVariant* array with the params where out parameters
	 *  will be encoded. The same array is used to input and output parameters.
	 * @param outParams ErlList with the in parameters from Erlang call
	 * @throw ErlXPCOMInterfaceMismatchException if the parameters does not
	 * complaint the interface.
	 */
	void unmarshallOutParams(nsXPTCMiniVariant* params, ErlList* outParams)
			throw (InterfaceMismatchException);
			
	/**
	 * Frees the memory of an nsXPTCVariant array allocated using.
	 * This method will delete ALL the pointers in the array using PR_Free.
	 * unmarshallInParams()
	 * @param params nsXPTCVariant* array	 
	 */ 
	void freeXPTCVariant(nsXPTCVariant* params);
	
	/**
	 * Gets the erlang representation of and nsISupports pointer. 
	 * This method will check if the object is an existing stub, an proxy and
	 * if not, will create and register a new stub for it.
	 */
	ErlTerm* XPCOMtoErlangObject(nsISupports* obj, nsIID &iid);
	
	
private:
	PRLogModuleInfo *log;
	
	ErlangORB* orb;

	nsCOMPtr<nsIInterfaceInfo> interfaceInfo;
	const nsXPTMethodInfo* methodInfo;
	PRUint16 methodIndex;

	ErlTerm* xptcMiniVariant2Erlang(
		const nsXPTCMiniVariant &miniVariant, 
	 	const nsXPTCMiniVariant &miniVariantDep1, 
	 	const nsXPTCMiniVariant &miniVariantDep2, 
		const nsXPTParamInfo &paramInfo,
		const int number) 
	throw (InterfaceMismatchException, InternalErrorException);
		
	void Erlang2xptcMiniVariant(
		ErlTerm* term, 
	 	nsXPTCMiniVariant &miniVariant, 
	 	nsXPTCMiniVariant &miniVariantDep1, 
	 	nsXPTCMiniVariant &miniVariantDep2, 
		const nsXPTParamInfo &paramInfo,
		bool &isAllocated,
		const int number);


};

} // erlxpcom

#endif // __ERLXPCOMMARSHALLUNMARSHALL_H
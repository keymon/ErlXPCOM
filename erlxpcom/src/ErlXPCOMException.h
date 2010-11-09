/*
***** BEGIN LICENSE BLOCK *****

This file is part of the erlXPCOM (Erlang XPCOM binding) project.

Copyright (C) 2005 Hector Rivas Gandara <keymon@gmail.com>

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
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
#ifndef __ERLXPCOMEXCEPTION_H
#define __ERLXPCOMEXCEPTION_H

#include <string>
#include <exception>
#include <sstream>

#include <nscore.h>

namespace erlxpcom {

/**
 * Base class for ErlXPCOM exceptions
 */
class ErlXPCOMException: std::exception {
public:
    inline explicit ErlXPCOMException(const std::string &msg): mMsg(msg) {
    }
  
	virtual inline ~ErlXPCOMException() throw() {}

    virtual inline const char* what() const throw() {
        return mMsg.c_str();
    };

    virtual inline const std::string getMessage() const {
        return mMsg;
    }

protected:
	std::string mMsg;

}; 
 

/**
 * Abstraction of XPCOM internal error in ErlXPCOM, with XPCOM.
 */ 
class InternalErrorException: public ErlXPCOMException {
public:
    inline explicit InternalErrorException(const std::string &msg): 
		ErlXPCOMException(msg) {
    }

    virtual inline ~InternalErrorException() throw() {}
};

/**
 * Internal error while using XPCOM API.
 */
class XPCOMException: public InternalErrorException {
public:
    inline explicit XPCOMException(const std::string &msg, 
		const nsresult result): 
		InternalErrorException(msg), mResult(result)  {
    }

    virtual inline ~XPCOMException() throw() {}

    virtual inline const char* what() const throw() {
        return this->getMessage().c_str();
    };

    virtual inline const std::string getMessage() const {
        std::ostringstream oss;
        oss << mMsg << std::endl <<
			"Internal Error Code:" << std::endl <<
			mResult;
			
		return oss.str();
    }

protected:

	nsresult mResult;
};

/**
 * XPCOM call already executed
 */ 
class AlreadyExecutedException: public ErlXPCOMException {
public:
    inline explicit AlreadyExecutedException(const std::string &msg): 
		ErlXPCOMException(msg) {
    }

    virtual inline ~AlreadyExecutedException() throw() {}

};

/**
 * Remote node unreacheable exception
 */ 
class RemoteNodeUnreacheableException: public InternalErrorException {
public:
    inline explicit RemoteNodeUnreacheableException(const std::string &msg): 
		InternalErrorException(msg) {
    }

    virtual inline ~RemoteNodeUnreacheableException() throw() {}

};

/**
 * Type is not supported
 */ 
class UnsupportedTypeException: public ErlXPCOMException {
public:
    inline explicit UnsupportedTypeException(const std::string &msg): 
		ErlXPCOMException(msg) {
    }

    virtual inline ~UnsupportedTypeException() throw() {}

};

/**
 * interface or parameter type error.
 */ 
class InterfaceMismatchException: public ErlXPCOMException {
public:
    inline explicit InterfaceMismatchException(const std::string &msg): 
		ErlXPCOMException(msg) {
    }

    virtual inline ~InterfaceMismatchException() throw() {}

};

/**
 * call to unknown method 
 */ 
class UnknownMethodException: public InterfaceMismatchException {
public:
    inline explicit UnknownMethodException(const std::string &_methodName): 
		InterfaceMismatchException(std::string("Unknown Method ")+_methodName),
		methodName(_methodName) 
	{}
		
	std::string getMethodName()	{
		return methodName;
    }
	
    virtual inline ~UnknownMethodException() throw() {}
private:
	std::string methodName;

};


} // namespace erlxpcom

#endif // __ERLXPCOMEXCEPTION_H


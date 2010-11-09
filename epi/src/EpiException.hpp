/*
***** BEGIN LICENSE BLOCK *****

This file is part of the EPI (Erlang Plus Interface) Library.

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

#ifndef _EPIEXCEPTION_HPP
#define _EPIEXCEPTION_HPP

#include <exception>
#include <string>
#include <sstream>


namespace epi {
namespace type {
	class ErlTerm;
}

namespace error {

using namespace epi::type;

/**
 * Base class for the other EPI erlang exception classes.
 */
class EpiException: public std::exception {
public:
    inline explicit EpiException(const std::string &msg): mMsg(msg) {
    }

    virtual inline ~EpiException() throw() {}

    inline const char* what() const throw() {
        return mMsg.c_str();
    };

    inline const std::string getMessage() const {
        return mMsg;
    }

protected:

    std::string mMsg;

};

/**
 * Timeout exception
 */
class EpiTimeout: public EpiException {
public:
    inline explicit EpiTimeout():
        EpiException("") {}
    virtual inline ~EpiTimeout() throw() {}
};

/**
 * Exception for invalid terms
 */
class EpiInvalidTerm: public EpiException {
public:
    inline explicit EpiInvalidTerm(const std::string &msg):
        EpiException(msg) {}
    virtual inline ~EpiInvalidTerm() throw() {}
};

/**
 * Exception for initialized terms
 */
class EpiAlreadyInitialized: public EpiException {
public:
    inline explicit EpiAlreadyInitialized(const std::string &msg):
        EpiException(msg) {}
    virtual inline ~EpiAlreadyInitialized() throw() {}
};


/**
 * Exception for bad argument
 */
class EpiBadArgument: public EpiException {
public:
    inline explicit EpiBadArgument(const std::string &msg):
        EpiException(msg) {}
    virtual inline ~EpiBadArgument() throw() {}
};

/**
 * Exception for unbound variables
 */
class EpiVariableUnbound: public EpiException {
    public:
    inline explicit EpiVariableUnbound(const std::string &variable):
        EpiException(""), mVariableName(variable) {
        std::ostringstream oss;
        oss << "Variable '" << variable << "' is unbound";
        mMsg = oss.str();

    }
    inline std::string getVariableName() const {
        return mVariableName;
    }
    virtual inline ~EpiVariableUnbound() throw() {}

private:
    std::string mVariableName;

};

/**
 * Exception for Bad RPC calls
 */
class EpiBadRPC: public EpiException {
    public:
    explicit EpiBadRPC(ErlTerm *error);
	ErlTerm* getError() const;
	virtual ~EpiBadRPC() throw();

private:
    ErlTerm *mError;

};

/**
 * Exception while encoding
 */
class EpiEncodeException: public EpiException {
public:
    inline explicit EpiEncodeException(const std::string &msg):
        EpiException(msg) {}
    virtual inline ~EpiEncodeException() throw() {}
};

/**
 * Exception while encoding with EI Library
 */
class EpiEIEncodeException: public EpiEncodeException {
public:
    inline explicit EpiEIEncodeException(const std::string &msg):
        EpiEncodeException(msg), mEiErrorCode(0) {}
    inline explicit EpiEIEncodeException(const std::string &msg, const int ei_err):
            EpiEncodeException(msg), mEiErrorCode(ei_err) {}
    virtual inline ~EpiEIEncodeException() throw() {}
private:
        int mEiErrorCode;
};

/**
 * Exception while decoding
 */
class EpiDecodeException: public EpiException {
public:
    inline explicit EpiDecodeException(const std::string &msg):
        EpiException(msg) {}
    virtual inline ~EpiDecodeException() throw() {}
};

/**
 * Exception while decoding with EI library
 */
class EpiEIDecodeException: public EpiDecodeException {
public:
    inline explicit EpiEIDecodeException(const std::string &msg):
        EpiDecodeException(msg) {}
    inline explicit EpiEIDecodeException(const std::string &msg, const int ei_err):
        EpiDecodeException(msg), mEiErrorCode(ei_err) {}
    virtual inline ~EpiEIDecodeException() throw() {}

private:
        int mEiErrorCode;
};

/**
 * Invalid use of an empty list
 */
class EpiEmptyList: public EpiException {
public:
    inline explicit EpiEmptyList(const std::string &msg):
        EpiException(msg) {}
    virtual inline ~EpiEmptyList() throw() {}
};

/**
 * Error while parsing an ErlTerm from an string
 */
class EpiParseError: public EpiException {
public:
    inline explicit EpiParseError(const std::string &msg, const int aposition = -1):
        EpiException(msg), position(aposition) {}

    virtual inline ~EpiParseError() throw() {}

    inline int getPosition() {
        return position;
    }
private:
        int position;
};

/**
 * Conection error
 */
class EpiConnectionException: public EpiException {
public:
    inline explicit EpiConnectionException(const std::string &msg):
        EpiException(msg) {}
    virtual inline ~EpiConnectionException() throw() {}
};

/**
 * Error in EI library
 */
class EpiEIException: public EpiConnectionException {
public:
    inline explicit EpiEIException(const std::string &msg, const int ei_err = 0):
        EpiConnectionException(""), mEiErrorCode(ei_err)
    {
        std::ostringstream oss;
        oss << msg << " ERROR CODE="<< ei_err;
        mMsg = oss.str();
    }
    virtual inline ~EpiEIException() throw() {}
private:
    int mEiErrorCode;
};

/**
 * Unknown message
 */
class EpiUnknownMessageException: public EpiConnectionException {
public:
    inline explicit EpiUnknownMessageException(const std::string &msg):
        EpiConnectionException(msg) {}
    virtual inline ~EpiUnknownMessageException() throw() {}
private:
        int errorCode;
};

/**
 * Cookie error
 */
class EpiAuthException: public EpiConnectionException {
public:
    inline explicit EpiAuthException(const std::string &msg):
        EpiConnectionException(msg) {}
    virtual inline ~EpiAuthException() throw() {}
private:
        int errorCode;
};

/**
 * Network exception
 */
class EpiNetworkException: public EpiConnectionException {
public:
    inline explicit EpiNetworkException(const std::string &msg, const int err = 0):
        EpiConnectionException(msg), errorCode(err) {}
    virtual inline ~EpiNetworkException() throw() {}

private:
        int errorCode;
};

/**
 * Exception for unknown protocol
 */
class EpiUnknownProtocol: public EpiException {
public:
    inline explicit EpiUnknownProtocol(const std::string &protocol):
        EpiException("")
    {
        std::ostringstream oss;
        oss << "Unknown protocol: " << protocol;
        mMsg = oss.str();
    }
    virtual inline ~EpiUnknownProtocol() throw() {}
};

} // namespace error
} // namespace epi

#endif // _EPIEXCEPTION_HPP

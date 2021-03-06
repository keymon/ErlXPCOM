/*
***** BEGIN LICENSE BLOCK *****

This file is part of the XPInterface Component.

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

#include "lfMailBox.hpp"

#include "lfXPInterfaceHelper.hpp"
#include "lfErlPid.hpp"
#include "lfIXPInterfaceError.h"

#include "lfException.hpp"

using namespace epi::type;
using namespace epi::node;
using namespace epi::error;

NS_IMPL_ISUPPORTS1(lfMailBox, lfIMailBox);

lfMailBox::lfMailBox(MailBox *mailbox): mMailBox(mailbox)
{
}

lfMailBox::~lfMailBox()
{
  /* destructor code */
}

/* readonly attribute lfIErlPid self; */
NS_IMETHODIMP lfMailBox::GetSelf(lfIErlPid * *aSelf)
{
    NS_ENSURE_ARG_POINTER(aSelf);
    NS_ADDREF(*aSelf = new lfErlPid(mMailBox->self()));
    return NS_OK;
}

/* void send (in lfIErlPid toPid, in lfIErlTerm term); */
NS_IMETHODIMP lfMailBox::Send(lfIErlPid *toPid, lfIErlTerm *term)
{
    ErlTerm *_toPid;
    toPid->GetErlTerm(&_toPid);
    ErlTerm *_term;
    term->GetErlTerm(&_term);
    try {
        mMailBox->send((ErlPid *) _toPid, _term);
    } catch (EpiInvalidTerm &e) {
        return NS_ERROR_INVALID_ARG;
    } catch (EpiEncodeException &e) {
        return NS_ERROR_XPINTERFACE_ENCODE;
    } catch (EpiConnectionException &e) {
        return NS_ERROR_XPINTERFACE_CONNECTION;
    }

    return NS_OK;
}

/* void regSend (in string toName, in lfIErlTerm term); */
NS_IMETHODIMP lfMailBox::RegSend(const char *toName, lfIErlTerm *term)
{
    ErlTerm *_term;
    term->GetErlTerm(&_term);
    try {
        mMailBox->send(toName, _term);
    } catch (EpiInvalidTerm &e) {
        return NS_ERROR_INVALID_ARG;
    } catch (EpiEncodeException &e) {
        return NS_ERROR_XPINTERFACE_ENCODE;
    } catch (EpiConnectionException &e) {
        return NS_ERROR_XPINTERFACE_CONNECTION;
    }

    return NS_OK;
}

/* void remoteRegSend (in string toNode, in string toName, in lfIErlTerm term); */
NS_IMETHODIMP lfMailBox::RemoteRegSend(const char *toNode, const char *toName, lfIErlTerm *term)
{
    ErlTerm *_term;
    term->GetErlTerm(&_term);
    try {
        mMailBox->send(toNode, toName, _term);
    } catch (EpiInvalidTerm &e) {
        return NS_ERROR_INVALID_ARG;
    } catch (EpiEncodeException &e) {
        return NS_ERROR_XPINTERFACE_ENCODE;
    } catch (EpiConnectionException &e) {
        //return NS_ERROR_XPINTERFACE_CONNECTION;
        return lfException::AddException(
                NS_ERROR_XPINTERFACE_CONNECTION,
                nsDependentCString("EpiConnectionException"),
                nsDependentCString(e.getMessage().c_str()));
    }

    return NS_OK;
}


/* void sendRPC (in string mod, in string fun, in lfIErlList args); */
NS_IMETHODIMP lfMailBox::SendRPC(const char *node, const char *mod, const char *fun, lfIErlList *args)
{
    ErlTerm *_args;
    args->GetErlTerm(&_args);
    try {
        mMailBox->sendRPC(node, mod, fun, (ErlConsList *) _args);
    } catch (EpiBadArgument &e) {
        return NS_ERROR_INVALID_ARG;
    } catch (EpiInvalidTerm &e) {
        return NS_ERROR_INVALID_ARG;
    } catch (EpiEncodeException &e) {
        return NS_ERROR_XPINTERFACE_ENCODE;
    } catch (EpiConnectionException &e) {
        return NS_ERROR_XPINTERFACE_CONNECTION;
    }

    return NS_OK;
}



/* lfIErlTerm receive (); */
NS_IMETHODIMP lfMailBox::Receive(lfIErlTerm **_retval)
{
    NS_ENSURE_ARG_POINTER(_retval);
    try {
        ErlTerm *term = mMailBox->receive();
        NS_ADDREF(*_retval = lfXPInterfaceHelper::fromErlTerm(term));
        return NS_OK;
    } catch (EpiDecodeException &e) {
        return NS_ERROR_XPINTERFACE_ENCODE;
    } catch (EpiConnectionException &e) {
        return NS_ERROR_XPINTERFACE_CONNECTION;
    }

}

/* lfIErlTerm receiveWithTimeout (in long timeout); */
NS_IMETHODIMP lfMailBox::ReceiveWithTimeout(PRInt32 timeout, lfIErlTerm **_retval)
{
    NS_ENSURE_ARG_POINTER(_retval);
    try {
        ErlTerm *term = mMailBox->receive(timeout);
		if (term) {
			NS_ADDREF(*_retval = lfXPInterfaceHelper::fromErlTerm(term));
		} else {
			*_retval = NULL;
		}
        return NS_OK;
    } catch (EpiDecodeException &e) {
        return NS_ERROR_XPINTERFACE_ENCODE;
    } catch (EpiConnectionException &e) {
        return NS_ERROR_XPINTERFACE_CONNECTION;
    }

}

/* lfIErlTerm receivePattern (in lfIErlTerm pattern, in lfIVariableBinding binding); */
NS_IMETHODIMP lfMailBox::ReceivePattern(lfIErlTerm *pattern, lfIVariableBinding *binding, lfIErlTerm **_retval)
{
    NS_ENSURE_ARG_POINTER(_retval);
    try {
        ErlTerm *_pattern;
        pattern->GetErlTerm(&_pattern);
        ErlTerm *term = mMailBox->receive(_pattern);
        NS_ADDREF(*_retval = lfXPInterfaceHelper::fromErlTerm(term));
        return NS_OK;
    } catch (EpiDecodeException &e) {
        return NS_ERROR_XPINTERFACE_ENCODE;
    } catch (EpiConnectionException &e) {
        return NS_ERROR_XPINTERFACE_CONNECTION;
    }
}

/* lfIErlTerm receivePatternWithTimeout (in lfIErlTerm pattern, in lfIVariableBinding binding, in long timeout); */
NS_IMETHODIMP lfMailBox::ReceivePatternWithTimeout(lfIErlTerm *pattern, lfIVariableBinding *binding, PRInt32 timeout, lfIErlTerm **_retval)
{
    try {
        ErlTerm *_pattern;
        pattern->GetErlTerm(&_pattern);
        ErlTerm *term = mMailBox->receive(_pattern, timeout);
        NS_ADDREF(*_retval = lfXPInterfaceHelper::fromErlTerm(term));
        return NS_OK;
    } catch (EpiDecodeException &e) {
        return NS_ERROR_XPINTERFACE_ENCODE;
    } catch (EpiConnectionException &e) {
        return NS_ERROR_XPINTERFACE_CONNECTION;
    }
}

/* lfIErlTerm receiveRPC (); */
NS_IMETHODIMP lfMailBox::ReceiveRPC(lfIErlTerm **_retval)
{
    NS_ENSURE_ARG_POINTER(_retval);
    try {
        ErlTerm *term = mMailBox->receiveRPC();
        NS_ADDREF(*_retval = lfXPInterfaceHelper::fromErlTerm(term));
        return NS_OK;
    } catch (EpiDecodeException &e) {
        return NS_ERROR_XPINTERFACE_ENCODE;
    } catch (EpiConnectionException &e) {
        return NS_ERROR_XPINTERFACE_CONNECTION;
    }
}


/* lfIErlTerm receiveRPCWithTimeout (in long timeout); */
NS_IMETHODIMP lfMailBox::ReceiveRPCWithTimeout(PRInt32 timeout, lfIErlTerm **_retval)
{
    NS_ENSURE_ARG_POINTER(_retval);
    try {
        ErlTerm *term = mMailBox->receiveRPC(timeout);
        NS_ADDREF(*_retval = lfXPInterfaceHelper::fromErlTerm(term));
        return NS_OK;
    } catch (EpiDecodeException &e) {
        return NS_ERROR_XPINTERFACE_ENCODE;
    } catch (EpiConnectionException &e) {
        return NS_ERROR_XPINTERFACE_CONNECTION;
    }
}

/* [noscript] lfNativeMailBox getNative (); */
NS_IMETHODIMP lfMailBox::GetNative(epi::node::MailBox * *_retval)
{
    *_retval = mMailBox;
    return NS_OK;
}


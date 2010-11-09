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

#include "lfNode.hpp"
#include "lfAbstractNode.hpp"
#include "lfMailBox.hpp"
#include "lfErlRef.hpp"
#include "lfErlPort.hpp"
#include "lfErlPid.hpp"
#include "xpcom_utils.hpp"

NS_IMPL_ISUPPORTS2(lfNode, lfIAbstractNode, lfINode);

lfNode::lfNode(const char *aNodeName, const char *aCookie)
     throw (EpiBadArgument, EpiConnectionException):
        mAutoNode(aNodeName, aCookie), mAbstractNodeImpl(&mAutoNode)
{
    mAutoNode.start();
}

lfNode::~lfNode()
{
    mAutoNode.close();
}

/* lfIMailBox createMailBox (); */
NS_IMETHODIMP lfNode::CreateMailBox(lfIMailBox **_retval)
{
    NS_ENSURE_ARG_POINTER(_retval);
    NS_ADDREF(*_retval = new lfMailBox(mAutoNode.createMailBox()));
    return NS_OK;
}

/* lfIErlPid createPid (); */
NS_IMETHODIMP lfNode::CreatePid(lfIErlPid **_retval)
{
    NS_ADDREF(*_retval = new lfErlPid(mAutoNode.createPid()));
    return NS_OK;
}

/* lfIErlRef createRef (); */
NS_IMETHODIMP lfNode::CreateRef(lfIErlRef **_retval)
{
    NS_ADDREF(*_retval = new lfErlRef(mAutoNode.createRef()));
    return NS_OK;
}

/* lfIErlRef createPort (); */
NS_IMETHODIMP lfNode::CreatePort(lfIErlPort **_retval)
{
    NS_ADDREF(*_retval = new lfErlPort(mAutoNode.createPort()));
    return NS_OK;
}

/* void registerMailBox (in string name, in lfIMailBox mailbox); */
NS_IMETHODIMP lfNode::RegisterMailBox(const char *name, lfIMailBox *mailbox)
{
    MailBox *_mailbox;
    mailbox->GetNative(&_mailbox);
    mAutoNode.registerMailBox(name, _mailbox);
    return NS_OK;
}

/* void unRegisterName (in string name); */
NS_IMETHODIMP lfNode::UnRegisterName(const char *name)
{
    mAutoNode.unRegisterMailBox(name);
    return NS_OK;
}

/* void unRegisterMailBox (in lfIMailBox mailbox); */
NS_IMETHODIMP lfNode::UnRegisterMailBox(lfIMailBox *mailbox)
{
    MailBox *_mailbox;
    mailbox->GetNative(&_mailbox);
    mAutoNode.unRegisterMailBox(_mailbox);
    return NS_OK;
}

/* boolean ping (in string remotename, in long timeout); */
NS_IMETHODIMP lfNode::Ping(const char *remotename, PRInt32 timeout, PRBool *_retval)
{
	if (mAutoNode.ping(remotename, timeout)) {
		*_retval=true;
	} else {
		*_retval=false;
	}
	return NS_OK;
}

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

#include "lfException.hpp"
#include "nsReadableUtils.h"
#include "nsIXPConnect.h"

lfException::lfException(nsresult aStatus, const nsACString & aName,
                                 const nsACString & aMessage, nsIException* aInner) :
                                 mStatus(aStatus),mName(aName),mMessage(aMessage),
                                 mInner(aInner)
{
  nsresult rc;
  nsCOMPtr<nsIXPConnect> xpc(do_GetService(nsIXPConnect::GetCID(), &rc));
  if(NS_SUCCEEDED(rc)) {
    xpc->GetCurrentJSStack(getter_AddRefs(mFrame));
  }
}

lfException::~lfException()
{
}

NS_IMPL_ISUPPORTS1(lfException, nsIException);

/* readonly attribute string message; */
NS_IMETHODIMP
lfException::GetMessage(char * *aMessage)
{
  NS_ENSURE_ARG_POINTER(aMessage);

  *aMessage = ToNewCString(mMessage);
  return NS_OK;
}

/* readonly attribute nsresult result; */
NS_IMETHODIMP
lfException::GetResult(nsresult *aResult)
{
  NS_ENSURE_ARG_POINTER(aResult);

  *aResult = mStatus;
  return NS_OK;
}

/* readonly attribute string name; */
NS_IMETHODIMP
lfException::GetName(char * *aName)
{
  NS_ENSURE_ARG_POINTER(aName);

  *aName = ToNewCString(mName);
  return NS_OK;
}

/* readonly attribute string filename; */
NS_IMETHODIMP
lfException::GetFilename(char * *aFilename)
{
  NS_ENSURE_ARG_POINTER(aFilename);
  if (mFrame) {
    return mFrame->GetFilename(aFilename);
  }

  *aFilename = nsnull;
  return NS_OK;
}

/* readonly attribute PRUint32 lineNumber; */
NS_IMETHODIMP
lfException::GetLineNumber(PRUint32 *aLineNumber)
{
  NS_ENSURE_ARG_POINTER(aLineNumber);
  if (mFrame) {
    PRInt32 l = 0;
    mFrame->GetLineNumber(&l);
    *aLineNumber = (PRUint32)l;
    return NS_OK;
  }

  *aLineNumber = 0;
  return NS_OK;
}

/* readonly attribute PRUint32 columnNumber; */
NS_IMETHODIMP
lfException::GetColumnNumber(PRUint32 *aColumnNumber)
{
  NS_ENSURE_ARG_POINTER(aColumnNumber);

  *aColumnNumber = 0;
  return NS_OK;
}

/* readonly attribute nsIStackFrame location; */
NS_IMETHODIMP
lfException::GetLocation(nsIStackFrame * *aLocation)
{
  NS_ENSURE_ARG_POINTER(aLocation);

  *aLocation = mFrame;
  NS_IF_ADDREF(*aLocation);
  return NS_OK;
}

/* readonly attribute nsIException inner; */
NS_IMETHODIMP
lfException::GetInner(nsIException * *aInner)
{
  NS_ENSURE_ARG_POINTER(aInner);

  *aInner = mInner;
  NS_IF_ADDREF(*aInner);
  return NS_OK;
}

/* readonly attribute nsISupports data; */
NS_IMETHODIMP
lfException::GetData(nsISupports * *aData)
{
  NS_ENSURE_ARG_POINTER(aData);

  *aData = nsnull;
  return NS_OK;
}

/* string toString (); */
NS_IMETHODIMP
lfException::ToString(char **_retval)
{
  NS_ENSURE_ARG_POINTER(_retval);
  nsCAutoString s;
  s.Append(mName);
  s.Append(NS_LITERAL_CSTRING(": "));
  s.Append(mMessage);
  if (mFrame) {
    char* str = nsnull;
    mFrame->ToString(&str);
    if (str) {
      s.Append(NS_LITERAL_CSTRING(", called by "));
      s.Append(nsDependentCString(str));
      nsMemory::Free(str);
    }
  }
  if (mInner) {
    char* str = nsnull;
    mInner->ToString(&str);
    if (str) {
      s.Append(NS_LITERAL_CSTRING(", caused by "));
      s.Append(nsDependentCString(str));
      nsMemory::Free(str);
    }
  }

  *_retval = ToNewCString(s);
  return NS_OK;
}

nsresult lfException::AddException(nsresult aStatus, const nsACString & aName,
  const nsACString & aMessage,PRBool aClear)
{
  nsCOMPtr<nsIExceptionService> xs =
    do_GetService(NS_EXCEPTIONSERVICE_CONTRACTID);
  if (xs) {
    nsCOMPtr<nsIExceptionManager> xm;
    xs->GetCurrentExceptionManager(getter_AddRefs(xm));
    if (xm) {
      nsCOMPtr<nsIException> old;
      if (!aClear)
        xs->GetCurrentException(getter_AddRefs(old));
      nsCOMPtr<nsIException> exception = new lfException(aStatus, aName,
                                                                            aMessage, old);
      if (exception) {
        xm->SetCurrentException(exception);
      }
    }
  }
  return aStatus;
}

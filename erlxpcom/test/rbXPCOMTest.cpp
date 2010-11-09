/* 
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 * 
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 * 
 * The Original Code is rbXPCOM.
 * 
 * The Initial Developer of the Original Code is Kenichi Komiya.
 * Portions created by Kenichi Komiya are Copyright (C) 2000 Kenichi
 * Komiya.  All Rights Reserved.
 * 
 * Contributor(s): 
 *   Kenichi Komiya <kom@mail1.accsnet.ne.jp> (original auther)
 * 
 * Alternatively, the contents of this file may be used under the
 * terms of the GNU General Public License Version 2 or later (the
 * "GPL"), in which case the provisions of the GPL are applicable 
 * instead of those above.  If you wish to allow use of your 
 * version of this file only under the terms of the GPL and not to
 * allow others to use your version of this file under the MPL,
 * indicate your decision by deleting the provisions above and
 * replace them with the notice and other provisions required by
 * the GPL.  If you do not delete the provisions above, a recipient
 * may use your version of this file under either the MPL or the
 * GPL.
 */

#include "rbXPCOMTest_private.h"
#include "nsISupports.h"
#include "nsCOMPtr.h"
#include "nsIModule.h"
#include "nsIGenericFactory.h"
#include "nsLiteralString.h"
#include "nsReadableUtils.h"
#include "nsCRT.h"
#include <string.h>
#include <stdio.h>

NS_IMPL_ISUPPORTS1(rbXPCOMTest, rbIXPCOMTest)

rbXPCOMTest::~rbXPCOMTest()
{
    if(mFoo)
        nsMemory::Free(mFoo);
}

rbXPCOMTest::rbXPCOMTest()
    : mFoo((char*)nsMemory::Clone("foo", 4))
{
    NS_INIT_ISUPPORTS();
    NS_ADDREF_THIS();
}

/* attribute string foo; */
NS_IMETHODIMP rbXPCOMTest::GetFoo(char * *aFoo)
{
    *aFoo = (char*)nsMemory::Clone(mFoo, strlen(mFoo) + 1);
    return *aFoo ? NS_OK : NS_ERROR_OUT_OF_MEMORY;
}
NS_IMETHODIMP rbXPCOMTest::SetFoo(const char * aFoo)
{
    if(mFoo)
        nsMemory::Free(mFoo);
    mFoo = (char*)nsMemory::Clone(aFoo, strlen(aFoo) + 1);
    return mFoo ? NS_OK : NS_ERROR_OUT_OF_MEMORY;
}

/* readonly attribute string bar; */
NS_IMETHODIMP rbXPCOMTest::GetBar(char * *aBar)
{
    *aBar = (char*)nsMemory::Clone("bar", strlen("bar") + 1);
    return *aBar ? NS_OK : NS_ERROR_OUT_OF_MEMORY;
}

/* readonly attribute DOMString domstr; */
NS_IMETHODIMP rbXPCOMTest::GetDomstr(nsAWritableString & aDomstr)
{
    CopyASCIItoUCS2(nsDependentCString("domstr"), aDomstr);
    return NS_OK;
}

  /* PRInt16 AddPRInt16 (in PRInt16 a, in PRInt16 b, inout PRInt16 c, out PRInt16 abc); */
NS_IMETHODIMP
rbXPCOMTest::AddPRInt16(PRInt16 a, PRInt16 b, PRInt16 *c, PRInt16 *neg,
                        PRInt16 *_retval)
{
    *_retval = a + b + *c;
    *c = - *c;
    *neg = - *_retval;
    return NS_OK;
}

/* PRInt16 EchoPRInt16 (in PRInt16 n); */
NS_IMETHODIMP
rbXPCOMTest::EchoPRInt16(PRInt16 n, PRInt16 *_retval)
{
    *_retval = n;
    return NS_OK;
}


/* PRInt32 EchoPRInt32 (in PRInt32 n); */
NS_IMETHODIMP
rbXPCOMTest::EchoPRInt32(PRInt32 n, PRInt32 *_retval)
{
    *_retval = n;
    return NS_OK;
}

/* PRInt64 EchoPRInt64 (in PRInt64 n); */
NS_IMETHODIMP
rbXPCOMTest::EchoPRInt64(PRInt64 n, PRInt64 *_retval)
{
    *_retval = n;
    return NS_OK;
}

/* PRUint8 EchoPRUint8 (in PRUint8 n); */
NS_IMETHODIMP
rbXPCOMTest::EchoPRUint8(PRUint8 n, PRUint8 *_retval)
{
    *_retval = n;
    return NS_OK;
}

/* PRUint16 EchoPRUint16 (in PRUint16 n); */
NS_IMETHODIMP
rbXPCOMTest::EchoPRUint16(PRUint16 n, PRUint16 *_retval)
{
    *_retval = n;
    return NS_OK;
}


/* PRUint32 EchoPRUint32 (in PRUint32 n); */
NS_IMETHODIMP
rbXPCOMTest::EchoPRUint32(PRUint32 n, PRUint32 *_retval)
{
    *_retval = n;
    return NS_OK;
}

/* PRUint64 EchoPRUint64 (in PRUint64 n); */
NS_IMETHODIMP
rbXPCOMTest::EchoPRUint64(PRUint64 n, PRUint64 *_retval)
{
    *_retval = n;
    return NS_OK;
}

/* float EchoFloat (in float n); */
NS_IMETHODIMP
rbXPCOMTest::EchoFloat(float n, float *_retval)
{
    *_retval = n;
    return NS_OK;
}

/* double EchoDouble (in double n); */
NS_IMETHODIMP
rbXPCOMTest::EchoDouble(double n, double *_retval)
{
    *_retval = n;
    return NS_OK;
}


/* boolean EchoBoolean (in boolean b); */
NS_IMETHODIMP
rbXPCOMTest::EchoBoolean(PRBool b, PRBool *_retval)
{
    *_retval = b;
    return NS_OK;
}

/* wchar EchoWchar (in wchar wc); */
NS_IMETHODIMP
rbXPCOMTest::EchoWchar(PRUnichar wc, PRUnichar *_retval)
{
    *_retval = wc;
    return NS_OK;
}

/* char EchoChar (in char c); */
NS_IMETHODIMP
rbXPCOMTest::EchoChar(char c, char *_retval)
{
    *_retval = c;
    return NS_OK;
}

/* void EchoArrayI16 (in PRUint32 size, [array, size_is (size)] in PRInt16 i, [array, size_is (size)] out PRInt16 o); */
NS_IMETHODIMP
rbXPCOMTest::EchoArrayI16(PRUint32 size, PRInt16 *i, PRInt16 **o)
{
    // handle null input for nil <-> conversion test
    if(!i)
    {
        *o = nsnull;
        return NS_OK;
    }
    *o = (PRInt16*) nsMemory::Clone(i, sizeof(PRInt16) * size);
    return *o ? NS_OK : NS_ERROR_OUT_OF_MEMORY;
}

/* void EchoArrayI32 (in PRUint32 size, [array, size_is (size)] in PRInt32 i, [array, size_is (size)] out PRInt32 o); */
NS_IMETHODIMP
rbXPCOMTest::EchoArrayI32(PRUint32 size, PRInt32 *i, PRInt32 **o)
{
    *o = (PRInt32*) nsMemory::Clone(i, sizeof(PRInt32) * size);
    return *o ? NS_OK : NS_ERROR_OUT_OF_MEMORY;
}

/* void EchoArrayI64 (in PRUint32 size, [array, size_is (size)] in PRInt64 i, [array, size_is (size)] out PRInt64 o); */
NS_IMETHODIMP
rbXPCOMTest::EchoArrayI64(PRUint32 size, PRInt64 *i, PRInt64 **o)
{
    *o = (PRInt64*) nsMemory::Clone(i, sizeof(PRInt64) * size);
    return *o ? NS_OK : NS_ERROR_OUT_OF_MEMORY;
}

/* void EchoArrayU8 (in PRUint32 size, [array, size_is (size)] in PRUint8 i, [array, size_is (size)] out PRUint8 o); */
NS_IMETHODIMP
rbXPCOMTest::EchoArrayU8(PRUint32 size, PRUint8 *i, PRUint8 **o)
{
    *o = (PRUint8*) nsMemory::Clone(i, sizeof(PRUint8) * size);
    return *o ? NS_OK : NS_ERROR_OUT_OF_MEMORY;
}

/* void EchoArrayU16 (in PRUint32 size, [array, size_is (size)] in PRUint16 i, [array, size_is (size)] out PRUint16 o); */
NS_IMETHODIMP
rbXPCOMTest::EchoArrayU16(PRUint32 size, PRUint16 *i, PRUint16 **o)
{
    *o = (PRUint16*) nsMemory::Clone(i, sizeof(PRUint16) * size);
    return *o ? NS_OK : NS_ERROR_OUT_OF_MEMORY;
}

/* void EchoArrayU32 (in PRUint32 size, [array, size_is (size)] in PRUint32 i, [array, size_is (size)] out PRUint32 o); */
NS_IMETHODIMP
rbXPCOMTest::EchoArrayU32(PRUint32 size, PRUint32 *i, PRUint32 **o)
{
    *o = (PRUint32*) nsMemory::Clone(i, sizeof(PRUint32) * size);
    return *o ? NS_OK : NS_ERROR_OUT_OF_MEMORY;
}

/* void EchoArrayU64 (in PRUint32 size, [array, size_is (size)] in PRUint64 i, [array, size_is (size)] out PRUint64 o); */
NS_IMETHODIMP
rbXPCOMTest::EchoArrayU64(PRUint32 size, PRUint64 *i, PRUint64 **o)
{
    *o = (PRUint64*) nsMemory::Clone(i, sizeof(PRUint64) * size);
    return *o ? NS_OK : NS_ERROR_OUT_OF_MEMORY;
}

/* void EchoArrayFLOAT (in PRUint32 size, [array, size_is (size)] in float i, [array, size_is (size)] out float o); */
NS_IMETHODIMP
rbXPCOMTest::EchoArrayFLOAT(PRUint32 size, float *i, float **o)
{
    *o = (float*) nsMemory::Clone(i, sizeof(float) * size);
    return *o ? NS_OK : NS_ERROR_OUT_OF_MEMORY;
}

/* void EchoArrayDOUBLE (in PRUint32 size, [array, size_is (size)] in double i, [array, size_is (size)] out double o); */
NS_IMETHODIMP
rbXPCOMTest::EchoArrayDOUBLE(PRUint32 size, double *i, double **o)
{
    *o = (double*) nsMemory::Clone(i, sizeof(double) * size);
    return *o ? NS_OK : NS_ERROR_OUT_OF_MEMORY;
}

/* void EchoArrayBOOL (in PRUint32 size, [array, size_is (size)] in boolean i, [array, size_is (size)] out boolean o); */
NS_IMETHODIMP
rbXPCOMTest::EchoArrayBOOL(PRUint32 size, PRBool *i, PRBool **o)
{
    *o = (PRBool*) nsMemory::Clone(i, sizeof(PRBool) * size);
    return *o ? NS_OK : NS_ERROR_OUT_OF_MEMORY;
}

/* void EchoArrayCHAR (in PRUint32 size, [array, size_is (size)] in wchar i, [array, size_is (size)] out wchar o); */
NS_IMETHODIMP
rbXPCOMTest::EchoArrayCHAR(PRUint32 size, PRUnichar *i, PRUnichar **o)
{
    *o = (PRUnichar*) nsMemory::Clone(i, sizeof(PRUnichar) * size);
    return *o ? NS_OK : NS_ERROR_OUT_OF_MEMORY;
}

/* void EchoArrayWCHAR (in PRUint32 size, [array, size_is (size)] in char i, [array, size_is (size)] out char o); */
NS_IMETHODIMP
rbXPCOMTest::EchoArrayWCHAR(PRUint32 size, char *i, char **o)
{
    *o = (char*) nsMemory::Clone(i, sizeof(char) * size);
    return *o ? NS_OK : NS_ERROR_OUT_OF_MEMORY;
}

/* void EchoArrayIID (in PRUint32 size, [array, size_is (size)] in nsIIDPtr i, [array, size_is (size)] out nsIIDPtr o); */
NS_IMETHODIMP
rbXPCOMTest::EchoArrayIID(PRUint32 size, const nsIID * *i, nsIID * **o)
{
    nsIID **array = (nsIID**) nsMemory::Alloc(sizeof(nsIID*) * size);
    if(!array)
    {
        *o = nsnull;
        return NS_ERROR_OUT_OF_MEMORY;
    }
    PRUint32 idx, idx2;
    for(idx = 0; idx < size; idx++)
    {
        array[idx] = (nsIID*) nsMemory::Clone(i[idx], sizeof(nsIID));
        if(!array[idx])
            break;
    }
    if(idx < size)
    {
        for(idx2 = 0; idx2 < idx; idx2++)
            nsMemory::Free(array[idx2]);
        nsMemory::Free(array);
        *o = nsnull;
        return NS_ERROR_OUT_OF_MEMORY;
    }
    *o = array;
    return NS_OK;
}

/* void EchoArrayCHAR_STR (in PRUint32 size, [array, size_is (size)] in string i, [array, size_is (size)] out string o); */
NS_IMETHODIMP
rbXPCOMTest::EchoArrayCHAR_STR(PRUint32 size, const char **i, char ***o)
{
    if(!i)
    {
        *o = nsnull;
        return NS_OK;
    }

    char **array = (char**) nsMemory::Alloc(sizeof(char*) * size);
    if(!array)
    {
        *o = nsnull;
        return NS_ERROR_OUT_OF_MEMORY;
    }
    PRUint32 idx, idx2;
    for(idx = 0; idx < size; idx++)
    {
        if(!i[idx])
        {
            array[idx] = nsnull;
            continue;
        }
        char *clone = (char*)nsMemory::Clone(i[idx], strlen(i[idx]) + 1);
        if(!clone)
            break;
        else
            array[idx] = clone;
    }
    if(idx < size)
    {
        for(idx2 = 0; idx2 < idx; idx2++)
            if(array[idx2])
                nsMemory::Free(array[idx2]);
        nsMemory::Free(array);
        *o = nsnull;
        return NS_ERROR_OUT_OF_MEMORY;
    }
    *o = array;
    return NS_OK;
}

static size_t
wstring_len(const PRUnichar *wstring)
{
    size_t result = 0;

    for(const PRUnichar *ptr = wstring; *ptr != 0; ptr++)
        result += sizeof(PRUnichar);

    return result;
}

/* void EchoArrayWCHAR_STR (in PRUint32 size, [array, size_is (size)] in wstring i, [array, size_is (size)] out wstring o); */
NS_IMETHODIMP
rbXPCOMTest::EchoArrayWCHAR_STR(PRUint32 size, const PRUnichar **i, PRUnichar ***o)
{
    if(!i)
    {
        *o = nsnull;
        return NS_OK;
    }

    PRUnichar **array = (PRUnichar**) nsMemory::Alloc(sizeof(PRUnichar*) * size);
    if(!array)
    {
        *o = nsnull;
        return NS_ERROR_OUT_OF_MEMORY;
    }
    PRUint32 idx, idx2;
    for(idx = 0; idx < size; idx++)
    {
        if(!i[idx])
        {
            array[idx] = nsnull;
            continue;
        }
        size_t size = wstring_len(i[idx]) + sizeof(PRUnichar);
        PRUnichar *clone = (PRUnichar*) nsMemory::Clone(i[idx], size);

        if(!clone)
            break;
        else
            array[idx] = clone;
    }
    if(idx < size)
    {
        for(idx2 = 0; idx2 < idx; idx2++)
            if(array[idx2])
                nsMemory::Free(array[idx2]);
        nsMemory::Free(array);
        *o = nsnull;
        return NS_ERROR_OUT_OF_MEMORY;
    }
    *o = array;
    return NS_OK;
}

/* void EchoArrayINTERFACE (in PRUint32 size, [array, size_is (size)] in rbIXPCOMTest i, [array, size_is (size)] out rbIXPCOMTest o); */
NS_IMETHODIMP
rbXPCOMTest::EchoArrayINTERFACE(PRUint32 size, rbIXPCOMTest **i, rbIXPCOMTest ***o)
{
    *o = (rbIXPCOMTest**) nsMemory::Clone(i, sizeof(rbIXPCOMTest*) * size);
    if(!*o)
        return NS_ERROR_OUT_OF_MEMORY;
    for(PRUint32 idx = 0; idx < size; idx++)
        NS_ADDREF(i[idx]);
    return NS_OK;
}

/* void EchoArrayINTERFACE_IS (in PRUint32 size, in nsIIDPtr iid, [array, size_is (size), iid_is (iid)] in nsQIResult i, [array, size_is (size), iid_is (iid)] out nsQIResult o); */
NS_IMETHODIMP
rbXPCOMTest::EchoArrayINTERFACE_IS(PRUint32 size, const nsIID * iid, void * *i, void * **o)
{
    *o = (void**) nsMemory::Clone(i, sizeof(void*) * size);
    if(!*o)
        return NS_ERROR_OUT_OF_MEMORY;
    for(PRUint32 idx = 0; idx < size; idx++)
        NS_ADDREF(NS_STATIC_CAST(nsISupports*, i[idx]));
    return NS_OK;
}


/* boolean EchoBoolean2 (in boolean bi, out boolean bo); */
NS_IMETHODIMP
rbXPCOMTest::EchoBoolean2(PRBool bi, PRBool *bo, PRBool *_retval)
{
    *bo = bi;
    *_retval = bi;
    return NS_OK;
}

/* boolean EchoBoolean3 (inout boolean bio, out boolean bo); */
NS_IMETHODIMP
rbXPCOMTest::EchoBoolean3(PRBool *bio, PRBool *bo, PRBool *_retval)
{
    *bo = *bio;
    *_retval = *bio;
    *bio = !*bio;
    return NS_OK;
}


/* nsIIDPtr EchoIIDPtr (in nsIIDPtr id); */
NS_IMETHODIMP 
rbXPCOMTest::EchoIIDPtr(const nsIID * id, nsIID * *_retval)
{
    if(id)
        *_retval = new nsIID(*id);
    else
        *_retval = nsnull;
    return NS_OK;
}


/* rbIXPCOMTest EchoInterfacePtr (in rbIXPCOMTest i); */
NS_IMETHODIMP 
rbXPCOMTest::EchoInterfacePtr(rbIXPCOMTest *i, rbIXPCOMTest **_retval)
{
    if(i)
        NS_ADDREF(i);
    *_retval = i;
    return NS_OK;
}


/* void EchoInterfaceIsPtr (in nsIIDPtr idin, [iid_is (idin)] in nsQIResult ii, out nsIIDPtr idout, [iid_is (idout), retval] out nsQIResult io); */
NS_IMETHODIMP 
rbXPCOMTest::EchoInterfaceIsPtr(const nsIID * idin, void * ii, 
                                nsIID * *idout, void * *io)
{
    *idout = (nsIID*) nsMemory::Clone(idin, sizeof(nsIID));
    if(!*idout)
        return NS_ERROR_OUT_OF_MEMORY;
    if(ii)
        NS_ADDREF(NS_STATIC_CAST(nsISupports*, ii));
    *io = ii;
    return NS_OK;
}


/* DOMString EchoDOMString (in DOMString s); */
NS_IMETHODIMP 
rbXPCOMTest::EchoDOMString(const nsAReadableString & s, 
                           nsAWritableString & _retval)
{
    _retval = s;
    return NS_OK;
}

NS_IMETHODIMP 
rbXPCOMTest::EchoString(const char *s, char **_retval)
{
    if(!s)
    {
        *_retval = nsnull;
        return NS_OK;
    }

    *_retval = (char*) nsMemory::Clone(s, (strlen(s)+1));
    return *_retval ? NS_OK : NS_ERROR_OUT_OF_MEMORY;
};


/* wstring EchoWstring (in wstring s); */
NS_IMETHODIMP
rbXPCOMTest::EchoWstring(const PRUnichar *s, PRUnichar **_retval)
{
    if(!s)
    {
        *_retval = nsnull;
        return NS_OK;
    }

    size_t size = wstring_len(s) + sizeof(PRUnichar);
    *_retval = (PRUnichar*) nsMemory::Clone(s, size);
    return *_retval ? NS_OK : NS_ERROR_OUT_OF_MEMORY;
}

/* void EchoStringSizeIs (in PRUint32 size, [size_is (size)] in string si, [size_is (size), retval] out string so); */
NS_IMETHODIMP 
rbXPCOMTest::EchoStringSizeIs(PRUint32 size, const char *si, char **so)
{
    if(!si)
    {
        *so = nsnull;
        return NS_OK;
    }
    *so = (char*) nsMemory::Clone(si, size + 1);
    return *so ? NS_OK : NS_ERROR_OUT_OF_MEMORY;
}

/* void EchoWstringSizeIs (in PRUint32 size, [size_is (size)] in wstring si, [size_is (size), retval] out wstring so); */
NS_IMETHODIMP
rbXPCOMTest::EchoWstringSizeIs(PRUint32 size, const PRUnichar *si, 
                               PRUnichar **so)
{
    if(!si)
    {
        *so = nsnull;
        return NS_OK;
    }
    *so = (PRUnichar*) nsMemory::Clone(si, (size + 1) * sizeof(PRUnichar));
    return *so ? NS_OK : NS_ERROR_OUT_OF_MEMORY;
}

/* boolean TestInterfacePtrIn (in rbIXPCOMTest i, in boolean b); */
NS_IMETHODIMP 
rbXPCOMTest::TestInterfacePtrIn(rbIXPCOMTest *i, PRBool b, PRBool *_retval)
{
    return i->EchoBoolean(b, _retval);
}

/* [noscript] void Unscriptable (in voidPtr p); */
NS_IMETHODIMP 
rbXPCOMTest::rbXPCOMTest::Unscriptable(void * p)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* PRUint16 TestArrayIn1 (in PRUint32 size, [array, size_is (size)] in PRUint16 ary); */
NS_IMETHODIMP 
rbXPCOMTest::TestArrayIn1(PRUint32 size, PRUint16 *ary, PRUint16 *_retval)
{
    PRUint16 sum = 0;
    for(PRUint32 i = 0; i < size; i++)
        sum += ary[i];
    *_retval = sum;
    return NS_OK;
}

/* PRUint16 TestArrayIn2 ([array, size_is (size)] in PRUint16 ary, in PRUint32 size); */
NS_IMETHODIMP 
rbXPCOMTest::TestArrayIn2(PRUint16 *ary, PRUint32 size, PRUint16 *_retval)
{
    PRUint16 sum = 0;
    for(PRUint32 i = 0; i < size; i++)
        sum += ary[i];
    *_retval = sum;
    return NS_OK;
}


/* void TestArrayOut1 (out PRUint32 size, [array, size_is (size)] out PRUint16 ary); */
NS_IMETHODIMP 
rbXPCOMTest::TestArrayOut1(PRUint32 *size, PRUint16 **ary)
{
    *size = 3;
    ((void*)*ary) = nsMemory::Alloc(*size * sizeof(PRUint16));
    PRUint16 *ptr = *ary;
    for(PRUint32 i = 0; i < *size; i++)
      ptr[i] = i;
    return NS_OK;
}


/* void TestArrayOut2 ([array, size_is (size)] out PRUint16 ary, out PRUint32 size); */
NS_IMETHODIMP
rbXPCOMTest::TestArrayOut2(PRUint16 **ary, PRUint32 *size)
{
    *size = 3;
    ((void*)*ary) = nsMemory::Alloc(*size * sizeof(PRUint16));
    PRUint16 *ptr = *ary;
    for(PRUint32 i = 0; i < *size; i++)
      ptr[i] = i;
    return NS_OK;
}


/* void TestArrayOut3 (in PRUint32 size, [array, size_is (size), retval] out PRUint16 ary); */
NS_IMETHODIMP 
rbXPCOMTest::TestArrayOut3(PRUint32 size, PRUint16 **ary)
{
    ((void*)*ary) = nsMemory::Alloc(size * sizeof(PRUint16));
    PRUint16 *ptr = *ary;
    for(PRUint32 i = 0; i < size; i++)
        ptr[i] = i;
    return NS_OK;
}


/* PRUint32 TestSizeIsOverwrite (in PRUint32 size, [size_is (size)] in string s1, [size_is (size)] in string s2); */
NS_IMETHODIMP
rbXPCOMTest::TestSizeIsOverwrite(PRUint32 size, const char *s1, const char *s2, PRUint32 *_retval)
{
    *_retval = size;
    return NS_OK;
}


/* rbIXPCOMTest TestInterfacePtrOut (); */
NS_IMETHODIMP 
rbXPCOMTest::TestInterfacePtrOut(rbIXPCOMTest **_retval)
{
    NS_ADDREF(this);
    *_retval = this;
    return NS_OK;
}

/* void TestInterfacesIsInout (inout nsIIDPtr iid, [iid_is (iid)] inout nsQIResult ii); */
NS_IMETHODIMP
rbXPCOMTest::TestInterfacesIsInout(nsIID * *iid, void * *ii)
{
    nsMemory::Free(*iid);
    *iid = (nsIID*)nsMemory::Clone(&NS_GET_IID(rbIXPCOMTest), 
                                   sizeof(nsIID));
    NS_IF_RELEASE((nsISupports*)*ii);
    NS_ADDREF(this);
    *ii = (void*)this;
    return NS_OK;
}

NS_IMETHODIMP
rbXPCOMTest::ConstructTest(nsISupports *aOuter, REFNSIID aIID, 
                           void **aResult)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

NS_IMETHODIMP
ConstructrbXPCOMTest(nsISupports *aOuter, REFNSIID aIID, void **aResult)
{
    nsresult rv;
    NS_ASSERTION(aOuter == nsnull, "no aggregation");
    rbXPCOMTest *obj = new rbXPCOMTest();

    if(obj)
    {
        rv = obj->QueryInterface(aIID, aResult);
        NS_ASSERTION(NS_SUCCEEDED(rv), "unable to find correct interface");
        NS_RELEASE(obj);
    }
    else
    {
        *aResult = nsnull;
        rv = NS_ERROR_OUT_OF_MEMORY;
    }
    return rv;
};

static nsModuleComponentInfo components[] = {
    {nsnull, RB_XPCOMTEST_CID, "@ruby/test/Test;1", 
     ConstructrbXPCOMTest},
    {nsnull, RB_XPCOMREFLECTIONTEST_CID, "@ruby/test/Reflection;1", 
     rbXPCOMTest::ConstructReflectionTest},
};
                                                               
NS_IMPL_NSGETMODULE("ruby xpcom test", components)

    

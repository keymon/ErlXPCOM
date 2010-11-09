/*
 * DO NOT EDIT.  THIS FILE IS GENERATED FROM lib/xpinterface/idl/lfIErlBinary.idl
 */

#ifndef __gen_lfIErlBinary_h__
#define __gen_lfIErlBinary_h__


#ifndef __gen_lfIErlTerm_h__
#include "lfIErlTerm.h"
#endif

/* For IDL files that don't want to include root IDL files. */
#ifndef NS_NO_VTABLE
#define NS_NO_VTABLE
#endif

/* starting interface:    lfIErlBinary */
#define LFIERLBINARY_IID_STR "ab722e30-bb32-4896-a27e-7fac09c4f163"

#define LFIERLBINARY_IID \
  {0xab722e30, 0xbb32, 0x4896, \
    { 0xa2, 0x7e, 0x7f, 0xac, 0x09, 0xc4, 0xf1, 0x63 }}

class NS_NO_VTABLE lfIErlBinary : public lfIErlTerm {
 public: 

  NS_DEFINE_STATIC_IID_ACCESSOR(LFIERLBINARY_IID)

  /** binary size */
  /* readonly attribute unsigned long size; */
  NS_IMETHOD GetSize(PRUint32 *aSize) = 0;

  /** Get one element by position */
  /* octet elementAt (in unsigned long position); */
  NS_IMETHOD ElementAt(PRUint32 position, PRUint8 *_retval) = 0;

};

/* Use this macro when declaring classes that implement this interface. */
#define NS_DECL_LFIERLBINARY \
  NS_IMETHOD GetSize(PRUint32 *aSize); \
  NS_IMETHOD ElementAt(PRUint32 position, PRUint8 *_retval); 

/* Use this macro to declare functions that forward the behavior of this interface to another object. */
#define NS_FORWARD_LFIERLBINARY(_to) \
  NS_IMETHOD GetSize(PRUint32 *aSize) { return _to GetSize(aSize); } \
  NS_IMETHOD ElementAt(PRUint32 position, PRUint8 *_retval) { return _to ElementAt(position, _retval); } 

/* Use this macro to declare functions that forward the behavior of this interface to another object in a safe way. */
#define NS_FORWARD_SAFE_LFIERLBINARY(_to) \
  NS_IMETHOD GetSize(PRUint32 *aSize) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetSize(aSize); } \
  NS_IMETHOD ElementAt(PRUint32 position, PRUint8 *_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->ElementAt(position, _retval); } 

#if 0
/* Use the code below as a template for the implementation class for this interface. */

/* Header file */
class lfErlBinary : public lfIErlBinary
{
public:
  NS_DECL_ISUPPORTS
  NS_DECL_LFIERLBINARY

  lfErlBinary();

private:
  ~lfErlBinary();

protected:
  /* additional members */
};

/* Implementation file */
NS_IMPL_ISUPPORTS1(lfErlBinary, lfIErlBinary)

lfErlBinary::lfErlBinary()
{
  /* member initializers and constructor code */
}

lfErlBinary::~lfErlBinary()
{
  /* destructor code */
}

/* readonly attribute unsigned long size; */
NS_IMETHODIMP lfErlBinary::GetSize(PRUint32 *aSize)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* octet elementAt (in unsigned long position); */
NS_IMETHODIMP lfErlBinary::ElementAt(PRUint32 position, PRUint8 *_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* End of implementation class template. */
#endif


#endif /* __gen_lfIErlBinary_h__ */

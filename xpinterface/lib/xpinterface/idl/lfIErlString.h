/*
 * DO NOT EDIT.  THIS FILE IS GENERATED FROM lib/xpinterface/idl/lfIErlString.idl
 */

#ifndef __gen_lfIErlString_h__
#define __gen_lfIErlString_h__


#ifndef __gen_lfIErlList_h__
#include "lfIErlList.h"
#endif

/* For IDL files that don't want to include root IDL files. */
#ifndef NS_NO_VTABLE
#define NS_NO_VTABLE
#endif

/* starting interface:    lfIErlString */
#define LFIERLSTRING_IID_STR "8c1902ad-c49b-4f31-bb49-8f1ad0ab803c"

#define LFIERLSTRING_IID \
  {0x8c1902ad, 0xc49b, 0x4f31, \
    { 0xbb, 0x49, 0x8f, 0x1a, 0xd0, 0xab, 0x80, 0x3c }}

class NS_NO_VTABLE lfIErlString : public lfIErlList {
 public: 

  NS_DEFINE_STATIC_IID_ACCESSOR(LFIERLSTRING_IID)

  /** Long value */
  /* readonly attribute string value; */
  NS_IMETHOD GetValue(char * *aValue) = 0;

};

/* Use this macro when declaring classes that implement this interface. */
#define NS_DECL_LFIERLSTRING \
  NS_IMETHOD GetValue(char * *aValue); 

/* Use this macro to declare functions that forward the behavior of this interface to another object. */
#define NS_FORWARD_LFIERLSTRING(_to) \
  NS_IMETHOD GetValue(char * *aValue) { return _to GetValue(aValue); } 

/* Use this macro to declare functions that forward the behavior of this interface to another object in a safe way. */
#define NS_FORWARD_SAFE_LFIERLSTRING(_to) \
  NS_IMETHOD GetValue(char * *aValue) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetValue(aValue); } 

#if 0
/* Use the code below as a template for the implementation class for this interface. */

/* Header file */
class lfErlString : public lfIErlString
{
public:
  NS_DECL_ISUPPORTS
  NS_DECL_LFIERLSTRING

  lfErlString();

private:
  ~lfErlString();

protected:
  /* additional members */
};

/* Implementation file */
NS_IMPL_ISUPPORTS1(lfErlString, lfIErlString)

lfErlString::lfErlString()
{
  /* member initializers and constructor code */
}

lfErlString::~lfErlString()
{
  /* destructor code */
}

/* readonly attribute string value; */
NS_IMETHODIMP lfErlString::GetValue(char * *aValue)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* End of implementation class template. */
#endif


#endif /* __gen_lfIErlString_h__ */

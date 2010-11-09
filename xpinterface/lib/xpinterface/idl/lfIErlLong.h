/*
 * DO NOT EDIT.  THIS FILE IS GENERATED FROM lib/xpinterface/idl/lfIErlLong.idl
 */

#ifndef __gen_lfIErlLong_h__
#define __gen_lfIErlLong_h__


#ifndef __gen_lfIErlTerm_h__
#include "lfIErlTerm.h"
#endif

/* For IDL files that don't want to include root IDL files. */
#ifndef NS_NO_VTABLE
#define NS_NO_VTABLE
#endif

/* starting interface:    lfIErlLong */
#define LFIERLLONG_IID_STR "dc3526bb-82c2-491c-960d-bb4ad27b7810"

#define LFIERLLONG_IID \
  {0xdc3526bb, 0x82c2, 0x491c, \
    { 0x96, 0x0d, 0xbb, 0x4a, 0xd2, 0x7b, 0x78, 0x10 }}

class NS_NO_VTABLE lfIErlLong : public lfIErlTerm {
 public: 

  NS_DEFINE_STATIC_IID_ACCESSOR(LFIERLLONG_IID)

  /**
     * Long value
     */
  /* readonly attribute long long value; */
  NS_IMETHOD GetValue(PRInt64 *aValue) = 0;

};

/* Use this macro when declaring classes that implement this interface. */
#define NS_DECL_LFIERLLONG \
  NS_IMETHOD GetValue(PRInt64 *aValue); 

/* Use this macro to declare functions that forward the behavior of this interface to another object. */
#define NS_FORWARD_LFIERLLONG(_to) \
  NS_IMETHOD GetValue(PRInt64 *aValue) { return _to GetValue(aValue); } 

/* Use this macro to declare functions that forward the behavior of this interface to another object in a safe way. */
#define NS_FORWARD_SAFE_LFIERLLONG(_to) \
  NS_IMETHOD GetValue(PRInt64 *aValue) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetValue(aValue); } 

#if 0
/* Use the code below as a template for the implementation class for this interface. */

/* Header file */
class lfErlLong : public lfIErlLong
{
public:
  NS_DECL_ISUPPORTS
  NS_DECL_LFIERLLONG

  lfErlLong();

private:
  ~lfErlLong();

protected:
  /* additional members */
};

/* Implementation file */
NS_IMPL_ISUPPORTS1(lfErlLong, lfIErlLong)

lfErlLong::lfErlLong()
{
  /* member initializers and constructor code */
}

lfErlLong::~lfErlLong()
{
  /* destructor code */
}

/* readonly attribute long long value; */
NS_IMETHODIMP lfErlLong::GetValue(PRInt64 *aValue)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* End of implementation class template. */
#endif


#endif /* __gen_lfIErlLong_h__ */

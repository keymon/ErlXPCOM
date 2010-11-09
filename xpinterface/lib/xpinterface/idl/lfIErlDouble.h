/*
 * DO NOT EDIT.  THIS FILE IS GENERATED FROM lib/xpinterface/idl/lfIErlDouble.idl
 */

#ifndef __gen_lfIErlDouble_h__
#define __gen_lfIErlDouble_h__


#ifndef __gen_lfIErlTerm_h__
#include "lfIErlTerm.h"
#endif

/* For IDL files that don't want to include root IDL files. */
#ifndef NS_NO_VTABLE
#define NS_NO_VTABLE
#endif

/* starting interface:    lfIErlDouble */
#define LFIERLDOUBLE_IID_STR "aaac2a4e-e8e3-4716-98ce-4f45a8c90160"

#define LFIERLDOUBLE_IID \
  {0xaaac2a4e, 0xe8e3, 0x4716, \
    { 0x98, 0xce, 0x4f, 0x45, 0xa8, 0xc9, 0x01, 0x60 }}

class NS_NO_VTABLE lfIErlDouble : public lfIErlTerm {
 public: 

  NS_DEFINE_STATIC_IID_ACCESSOR(LFIERLDOUBLE_IID)

  /** Double value */
  /* readonly attribute double value; */
  NS_IMETHOD GetValue(double *aValue) = 0;

};

/* Use this macro when declaring classes that implement this interface. */
#define NS_DECL_LFIERLDOUBLE \
  NS_IMETHOD GetValue(double *aValue); 

/* Use this macro to declare functions that forward the behavior of this interface to another object. */
#define NS_FORWARD_LFIERLDOUBLE(_to) \
  NS_IMETHOD GetValue(double *aValue) { return _to GetValue(aValue); } 

/* Use this macro to declare functions that forward the behavior of this interface to another object in a safe way. */
#define NS_FORWARD_SAFE_LFIERLDOUBLE(_to) \
  NS_IMETHOD GetValue(double *aValue) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetValue(aValue); } 

#if 0
/* Use the code below as a template for the implementation class for this interface. */

/* Header file */
class lfErlDouble : public lfIErlDouble
{
public:
  NS_DECL_ISUPPORTS
  NS_DECL_LFIERLDOUBLE

  lfErlDouble();

private:
  ~lfErlDouble();

protected:
  /* additional members */
};

/* Implementation file */
NS_IMPL_ISUPPORTS1(lfErlDouble, lfIErlDouble)

lfErlDouble::lfErlDouble()
{
  /* member initializers and constructor code */
}

lfErlDouble::~lfErlDouble()
{
  /* destructor code */
}

/* readonly attribute double value; */
NS_IMETHODIMP lfErlDouble::GetValue(double *aValue)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* End of implementation class template. */
#endif


#endif /* __gen_lfIErlDouble_h__ */

/*
 * DO NOT EDIT.  THIS FILE IS GENERATED FROM lib/xpinterface/idl/lfIErlVariable.idl
 */

#ifndef __gen_lfIErlVariable_h__
#define __gen_lfIErlVariable_h__


#ifndef __gen_lfIErlTerm_h__
#include "lfIErlTerm.h"
#endif

/* For IDL files that don't want to include root IDL files. */
#ifndef NS_NO_VTABLE
#define NS_NO_VTABLE
#endif

/* starting interface:    lfIErlVariable */
#define LFIERLVARIABLE_IID_STR "7c1c01ae-efa3-4e12-bf03-5b9f8f7b422f"

#define LFIERLVARIABLE_IID \
  {0x7c1c01ae, 0xefa3, 0x4e12, \
    { 0xbf, 0x03, 0x5b, 0x9f, 0x8f, 0x7b, 0x42, 0x2f }}

class NS_NO_VTABLE lfIErlVariable : public lfIErlTerm {
 public: 

  NS_DEFINE_STATIC_IID_ACCESSOR(LFIERLVARIABLE_IID)

  /** Variable name */
  /* readonly attribute string name; */
  NS_IMETHOD GetName(char * *aName) = 0;

};

/* Use this macro when declaring classes that implement this interface. */
#define NS_DECL_LFIERLVARIABLE \
  NS_IMETHOD GetName(char * *aName); 

/* Use this macro to declare functions that forward the behavior of this interface to another object. */
#define NS_FORWARD_LFIERLVARIABLE(_to) \
  NS_IMETHOD GetName(char * *aName) { return _to GetName(aName); } 

/* Use this macro to declare functions that forward the behavior of this interface to another object in a safe way. */
#define NS_FORWARD_SAFE_LFIERLVARIABLE(_to) \
  NS_IMETHOD GetName(char * *aName) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetName(aName); } 

#if 0
/* Use the code below as a template for the implementation class for this interface. */

/* Header file */
class lfErlVariable : public lfIErlVariable
{
public:
  NS_DECL_ISUPPORTS
  NS_DECL_LFIERLVARIABLE

  lfErlVariable();

private:
  ~lfErlVariable();

protected:
  /* additional members */
};

/* Implementation file */
NS_IMPL_ISUPPORTS1(lfErlVariable, lfIErlVariable)

lfErlVariable::lfErlVariable()
{
  /* member initializers and constructor code */
}

lfErlVariable::~lfErlVariable()
{
  /* destructor code */
}

/* readonly attribute string name; */
NS_IMETHODIMP lfErlVariable::GetName(char * *aName)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* End of implementation class template. */
#endif


#endif /* __gen_lfIErlVariable_h__ */

/*
 * DO NOT EDIT.  THIS FILE IS GENERATED FROM lib/xpinterface/idl/lfIErlAtom.idl
 */

#ifndef __gen_lfIErlAtom_h__
#define __gen_lfIErlAtom_h__


#ifndef __gen_lfIErlTerm_h__
#include "lfIErlTerm.h"
#endif

/* For IDL files that don't want to include root IDL files. */
#ifndef NS_NO_VTABLE
#define NS_NO_VTABLE
#endif

/* starting interface:    lfIErlAtom */
#define LFIERLATOM_IID_STR "70544387-14c9-4ee2-913f-45497eb5e90b"

#define LFIERLATOM_IID \
  {0x70544387, 0x14c9, 0x4ee2, \
    { 0x91, 0x3f, 0x45, 0x49, 0x7e, 0xb5, 0xe9, 0x0b }}

class NS_NO_VTABLE lfIErlAtom : public lfIErlTerm {
 public: 

  NS_DEFINE_STATIC_IID_ACCESSOR(LFIERLATOM_IID)

  /** 
	  * Atom value
	  */
  /* readonly attribute string value; */
  NS_IMETHOD GetValue(char * *aValue) = 0;

};

/* Use this macro when declaring classes that implement this interface. */
#define NS_DECL_LFIERLATOM \
  NS_IMETHOD GetValue(char * *aValue); 

/* Use this macro to declare functions that forward the behavior of this interface to another object. */
#define NS_FORWARD_LFIERLATOM(_to) \
  NS_IMETHOD GetValue(char * *aValue) { return _to GetValue(aValue); } 

/* Use this macro to declare functions that forward the behavior of this interface to another object in a safe way. */
#define NS_FORWARD_SAFE_LFIERLATOM(_to) \
  NS_IMETHOD GetValue(char * *aValue) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetValue(aValue); } 

#if 0
/* Use the code below as a template for the implementation class for this interface. */

/* Header file */
class lfErlAtom : public lfIErlAtom
{
public:
  NS_DECL_ISUPPORTS
  NS_DECL_LFIERLATOM

  lfErlAtom();

private:
  ~lfErlAtom();

protected:
  /* additional members */
};

/* Implementation file */
NS_IMPL_ISUPPORTS1(lfErlAtom, lfIErlAtom)

lfErlAtom::lfErlAtom()
{
  /* member initializers and constructor code */
}

lfErlAtom::~lfErlAtom()
{
  /* destructor code */
}

/* readonly attribute string value; */
NS_IMETHODIMP lfErlAtom::GetValue(char * *aValue)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* End of implementation class template. */
#endif


#endif /* __gen_lfIErlAtom_h__ */

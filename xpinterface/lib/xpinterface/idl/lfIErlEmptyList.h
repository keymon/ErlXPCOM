/*
 * DO NOT EDIT.  THIS FILE IS GENERATED FROM lib/xpinterface/idl/lfIErlEmptyList.idl
 */

#ifndef __gen_lfIErlEmptyList_h__
#define __gen_lfIErlEmptyList_h__


#ifndef __gen_lfIErlList_h__
#include "lfIErlList.h"
#endif

/* For IDL files that don't want to include root IDL files. */
#ifndef NS_NO_VTABLE
#define NS_NO_VTABLE
#endif

/* starting interface:    lfIErlEmptyList */
#define LFIERLEMPTYLIST_IID_STR "cb6e5f3e-7a4c-4918-98ea-c05f5f9c6646"

#define LFIERLEMPTYLIST_IID \
  {0xcb6e5f3e, 0x7a4c, 0x4918, \
    { 0x98, 0xea, 0xc0, 0x5f, 0x5f, 0x9c, 0x66, 0x46 }}

class NS_NO_VTABLE lfIErlEmptyList : public lfIErlList {
 public: 

  NS_DEFINE_STATIC_IID_ACCESSOR(LFIERLEMPTYLIST_IID)

};

/* Use this macro when declaring classes that implement this interface. */
#define NS_DECL_LFIERLEMPTYLIST \
  /* no methods! */

/* Use this macro to declare functions that forward the behavior of this interface to another object. */
#define NS_FORWARD_LFIERLEMPTYLIST(_to) \
  /* no methods! */

/* Use this macro to declare functions that forward the behavior of this interface to another object in a safe way. */
#define NS_FORWARD_SAFE_LFIERLEMPTYLIST(_to) \
  /* no methods! */

#if 0
/* Use the code below as a template for the implementation class for this interface. */

/* Header file */
class lfErlEmptyList : public lfIErlEmptyList
{
public:
  NS_DECL_ISUPPORTS
  NS_DECL_LFIERLEMPTYLIST

  lfErlEmptyList();

private:
  ~lfErlEmptyList();

protected:
  /* additional members */
};

/* Implementation file */
NS_IMPL_ISUPPORTS1(lfErlEmptyList, lfIErlEmptyList)

lfErlEmptyList::lfErlEmptyList()
{
  /* member initializers and constructor code */
}

lfErlEmptyList::~lfErlEmptyList()
{
  /* destructor code */
}

/* End of implementation class template. */
#endif


#endif /* __gen_lfIErlEmptyList_h__ */

/*
 * DO NOT EDIT.  THIS FILE IS GENERATED FROM lib/xpinterface/idl/lfIErlList.idl
 */

#ifndef __gen_lfIErlList_h__
#define __gen_lfIErlList_h__


#ifndef __gen_lfIErlTerm_h__
#include "lfIErlTerm.h"
#endif

/* For IDL files that don't want to include root IDL files. */
#ifndef NS_NO_VTABLE
#define NS_NO_VTABLE
#endif

/* starting interface:    lfIErlList */
#define LFIERLLIST_IID_STR "99e697b6-f8e2-476e-8bb4-f556796ffc62"

#define LFIERLLIST_IID \
  {0x99e697b6, 0xf8e2, 0x476e, \
    { 0x8b, 0xb4, 0xf5, 0x56, 0x79, 0x6f, 0xfc, 0x62 }}

class NS_NO_VTABLE lfIErlList : public lfIErlTerm {
 public: 

  NS_DEFINE_STATIC_IID_ACCESSOR(LFIERLLIST_IID)

  /** Length of the list */
  /* readonly attribute unsigned long arity; */
  NS_IMETHOD GetArity(PRUint32 *aArity) = 0;

  /**
     * Extract an specified element
     */
  /* lfIErlTerm elementAt (in unsigned long position); */
  NS_IMETHOD ElementAt(PRUint32 position, lfIErlTerm **_retval) = 0;

  /**
     * Get the tail of this list in given position
     */
  /* lfIErlTerm tail (in unsigned long position); */
  NS_IMETHOD Tail(PRUint32 position, lfIErlTerm **_retval) = 0;

};

/* Use this macro when declaring classes that implement this interface. */
#define NS_DECL_LFIERLLIST \
  NS_IMETHOD GetArity(PRUint32 *aArity); \
  NS_IMETHOD ElementAt(PRUint32 position, lfIErlTerm **_retval); \
  NS_IMETHOD Tail(PRUint32 position, lfIErlTerm **_retval); 

/* Use this macro to declare functions that forward the behavior of this interface to another object. */
#define NS_FORWARD_LFIERLLIST(_to) \
  NS_IMETHOD GetArity(PRUint32 *aArity) { return _to GetArity(aArity); } \
  NS_IMETHOD ElementAt(PRUint32 position, lfIErlTerm **_retval) { return _to ElementAt(position, _retval); } \
  NS_IMETHOD Tail(PRUint32 position, lfIErlTerm **_retval) { return _to Tail(position, _retval); } 

/* Use this macro to declare functions that forward the behavior of this interface to another object in a safe way. */
#define NS_FORWARD_SAFE_LFIERLLIST(_to) \
  NS_IMETHOD GetArity(PRUint32 *aArity) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetArity(aArity); } \
  NS_IMETHOD ElementAt(PRUint32 position, lfIErlTerm **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->ElementAt(position, _retval); } \
  NS_IMETHOD Tail(PRUint32 position, lfIErlTerm **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->Tail(position, _retval); } 

#if 0
/* Use the code below as a template for the implementation class for this interface. */

/* Header file */
class lfErlList : public lfIErlList
{
public:
  NS_DECL_ISUPPORTS
  NS_DECL_LFIERLLIST

  lfErlList();

private:
  ~lfErlList();

protected:
  /* additional members */
};

/* Implementation file */
NS_IMPL_ISUPPORTS1(lfErlList, lfIErlList)

lfErlList::lfErlList()
{
  /* member initializers and constructor code */
}

lfErlList::~lfErlList()
{
  /* destructor code */
}

/* readonly attribute unsigned long arity; */
NS_IMETHODIMP lfErlList::GetArity(PRUint32 *aArity)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlTerm elementAt (in unsigned long position); */
NS_IMETHODIMP lfErlList::ElementAt(PRUint32 position, lfIErlTerm **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlTerm tail (in unsigned long position); */
NS_IMETHODIMP lfErlList::Tail(PRUint32 position, lfIErlTerm **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* End of implementation class template. */
#endif


#endif /* __gen_lfIErlList_h__ */

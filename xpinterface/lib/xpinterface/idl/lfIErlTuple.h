/*
 * DO NOT EDIT.  THIS FILE IS GENERATED FROM lib/xpinterface/idl/lfIErlTuple.idl
 */

#ifndef __gen_lfIErlTuple_h__
#define __gen_lfIErlTuple_h__


#ifndef __gen_lfIErlTerm_h__
#include "lfIErlTerm.h"
#endif

/* For IDL files that don't want to include root IDL files. */
#ifndef NS_NO_VTABLE
#define NS_NO_VTABLE
#endif

/* starting interface:    lfIErlTuple */
#define LFIERLTUPLE_IID_STR "742d6782-d4e5-4316-bbb1-dc9f69f4a84b"

#define LFIERLTUPLE_IID \
  {0x742d6782, 0xd4e5, 0x4316, \
    { 0xbb, 0xb1, 0xdc, 0x9f, 0x69, 0xf4, 0xa8, 0x4b }}

class NS_NO_VTABLE lfIErlTuple : public lfIErlTerm {
 public: 

  NS_DEFINE_STATIC_IID_ACCESSOR(LFIERLTUPLE_IID)

  /** Size of the tuple */
  /* readonly attribute unsigned long arity; */
  NS_IMETHOD GetArity(PRUint32 *aArity) = 0;

  /**
	 * Set the terms of the tuple in a secuence.
	 * This function must be called 'size' times to init the tuple.
	 * The fisrt time inits the position 0, second the position 1, etc...
	 * The function fails if all the elements have been added
	 * @param anErlTerm lfIErlTerm to add
     */
  /* void initElement (in lfIErlTerm anErlTerm); */
  NS_IMETHOD InitElement(lfIErlTerm *anErlTerm) = 0;

  /**
	 * Extract an specified element
	 */
  /* lfIErlTerm elementAt (in unsigned long position); */
  NS_IMETHOD ElementAt(PRUint32 position, lfIErlTerm **_retval) = 0;

};

/* Use this macro when declaring classes that implement this interface. */
#define NS_DECL_LFIERLTUPLE \
  NS_IMETHOD GetArity(PRUint32 *aArity); \
  NS_IMETHOD InitElement(lfIErlTerm *anErlTerm); \
  NS_IMETHOD ElementAt(PRUint32 position, lfIErlTerm **_retval); 

/* Use this macro to declare functions that forward the behavior of this interface to another object. */
#define NS_FORWARD_LFIERLTUPLE(_to) \
  NS_IMETHOD GetArity(PRUint32 *aArity) { return _to GetArity(aArity); } \
  NS_IMETHOD InitElement(lfIErlTerm *anErlTerm) { return _to InitElement(anErlTerm); } \
  NS_IMETHOD ElementAt(PRUint32 position, lfIErlTerm **_retval) { return _to ElementAt(position, _retval); } 

/* Use this macro to declare functions that forward the behavior of this interface to another object in a safe way. */
#define NS_FORWARD_SAFE_LFIERLTUPLE(_to) \
  NS_IMETHOD GetArity(PRUint32 *aArity) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetArity(aArity); } \
  NS_IMETHOD InitElement(lfIErlTerm *anErlTerm) { return !_to ? NS_ERROR_NULL_POINTER : _to->InitElement(anErlTerm); } \
  NS_IMETHOD ElementAt(PRUint32 position, lfIErlTerm **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->ElementAt(position, _retval); } 

#if 0
/* Use the code below as a template for the implementation class for this interface. */

/* Header file */
class lfErlTuple : public lfIErlTuple
{
public:
  NS_DECL_ISUPPORTS
  NS_DECL_LFIERLTUPLE

  lfErlTuple();

private:
  ~lfErlTuple();

protected:
  /* additional members */
};

/* Implementation file */
NS_IMPL_ISUPPORTS1(lfErlTuple, lfIErlTuple)

lfErlTuple::lfErlTuple()
{
  /* member initializers and constructor code */
}

lfErlTuple::~lfErlTuple()
{
  /* destructor code */
}

/* readonly attribute unsigned long arity; */
NS_IMETHODIMP lfErlTuple::GetArity(PRUint32 *aArity)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void initElement (in lfIErlTerm anErlTerm); */
NS_IMETHODIMP lfErlTuple::InitElement(lfIErlTerm *anErlTerm)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlTerm elementAt (in unsigned long position); */
NS_IMETHODIMP lfErlTuple::ElementAt(PRUint32 position, lfIErlTerm **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* End of implementation class template. */
#endif


#endif /* __gen_lfIErlTuple_h__ */

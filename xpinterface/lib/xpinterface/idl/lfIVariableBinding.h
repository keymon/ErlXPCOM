/*
 * DO NOT EDIT.  THIS FILE IS GENERATED FROM lib/xpinterface/idl/lfIVariableBinding.idl
 */

#ifndef __gen_lfIVariableBinding_h__
#define __gen_lfIVariableBinding_h__


#ifndef __gen_nsISupports_h__
#include "nsISupports.h"
#endif

/* For IDL files that don't want to include root IDL files. */
#ifndef NS_NO_VTABLE
#define NS_NO_VTABLE
#endif
class lfIErlTerm; /* forward declaration */


/* starting interface:    lfIVariableBinding */
#define LFIVARIABLEBINDING_IID_STR "7c1c01ae-efa3-4e12-bf03-5b9f8f7b422f"

#define LFIVARIABLEBINDING_IID \
  {0x7c1c01ae, 0xefa3, 0x4e12, \
    { 0xbf, 0x03, 0x5b, 0x9f, 0x8f, 0x7b, 0x42, 0x2f }}

class NS_NO_VTABLE lfIVariableBinding : public nsISupports {
 public: 

  NS_DEFINE_STATIC_IID_ACCESSOR(LFIVARIABLEBINDING_IID)

  /**
     * Search a bound variable by name
     * @param name name to search
     * @return term if it is bound, null otherwise
     */
  /* lfIErlTerm search (in string name); */
  NS_IMETHOD Search(const char *name, lfIErlTerm **_retval) = 0;

  /**
     * bind a variable to a term
     * @param name variable name to bind
     * @param term ErlTerm to bind to this variable
     */
  /* void bind (in string name, in lfIErlTerm term); */
  NS_IMETHOD Bind(const char *name, lfIErlTerm *term) = 0;

  /**
     * Returns the native EPI representation
     * (VariableBinding *) of this binding.
     */
  /* [noscript] lfNativeVariableBinding getNative (); */
  NS_IMETHOD GetNative(epi::type::VariableBinding * *_retval) = 0;

};

/* Use this macro when declaring classes that implement this interface. */
#define NS_DECL_LFIVARIABLEBINDING \
  NS_IMETHOD Search(const char *name, lfIErlTerm **_retval); \
  NS_IMETHOD Bind(const char *name, lfIErlTerm *term); \
  NS_IMETHOD GetNative(epi::type::VariableBinding * *_retval); 

/* Use this macro to declare functions that forward the behavior of this interface to another object. */
#define NS_FORWARD_LFIVARIABLEBINDING(_to) \
  NS_IMETHOD Search(const char *name, lfIErlTerm **_retval) { return _to Search(name, _retval); } \
  NS_IMETHOD Bind(const char *name, lfIErlTerm *term) { return _to Bind(name, term); } \
  NS_IMETHOD GetNative(epi::type::VariableBinding * *_retval) { return _to GetNative(_retval); } 

/* Use this macro to declare functions that forward the behavior of this interface to another object in a safe way. */
#define NS_FORWARD_SAFE_LFIVARIABLEBINDING(_to) \
  NS_IMETHOD Search(const char *name, lfIErlTerm **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->Search(name, _retval); } \
  NS_IMETHOD Bind(const char *name, lfIErlTerm *term) { return !_to ? NS_ERROR_NULL_POINTER : _to->Bind(name, term); } \
  NS_IMETHOD GetNative(epi::type::VariableBinding * *_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetNative(_retval); } 

#if 0
/* Use the code below as a template for the implementation class for this interface. */

/* Header file */
class lfVariableBinding : public lfIVariableBinding
{
public:
  NS_DECL_ISUPPORTS
  NS_DECL_LFIVARIABLEBINDING

  lfVariableBinding();

private:
  ~lfVariableBinding();

protected:
  /* additional members */
};

/* Implementation file */
NS_IMPL_ISUPPORTS1(lfVariableBinding, lfIVariableBinding)

lfVariableBinding::lfVariableBinding()
{
  /* member initializers and constructor code */
}

lfVariableBinding::~lfVariableBinding()
{
  /* destructor code */
}

/* lfIErlTerm search (in string name); */
NS_IMETHODIMP lfVariableBinding::Search(const char *name, lfIErlTerm **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void bind (in string name, in lfIErlTerm term); */
NS_IMETHODIMP lfVariableBinding::Bind(const char *name, lfIErlTerm *term)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* [noscript] lfNativeVariableBinding getNative (); */
NS_IMETHODIMP lfVariableBinding::GetNative(epi::type::VariableBinding * *_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* End of implementation class template. */
#endif


#endif /* __gen_lfIVariableBinding_h__ */

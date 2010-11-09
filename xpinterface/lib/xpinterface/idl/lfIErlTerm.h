/*
 * DO NOT EDIT.  THIS FILE IS GENERATED FROM lib/xpinterface/idl/lfIErlTerm.idl
 */

#ifndef __gen_lfIErlTerm_h__
#define __gen_lfIErlTerm_h__


#ifndef __gen_nsISupports_h__
#include "nsISupports.h"
#endif

#ifndef __gen_lfIVariableBinding_h__
#include "lfIVariableBinding.h"
#endif

/* For IDL files that don't want to include root IDL files. */
#ifndef NS_NO_VTABLE
#define NS_NO_VTABLE
#endif

/* starting interface:    lfIErlTerm */
#define LFIERLTERM_IID_STR "c286c293-505a-48fc-9e7b-278140c19f66"

#define LFIERLTERM_IID \
  {0xc286c293, 0x505a, 0x48fc, \
    { 0x9e, 0x7b, 0x27, 0x81, 0x40, 0xc1, 0x9f, 0x66 }}

class NS_NO_VTABLE lfIErlTerm : public nsISupports {
 public: 

  NS_DEFINE_STATIC_IID_ACCESSOR(LFIERLTERM_IID)

  /** Returns the String representation of the term */
  /* string toString (); */
  NS_IMETHOD ToString(char **_retval) = 0;

  /**
     * Returns the String representation of the term,
     * using a VariableBinding
     */
  /* string toStringWithBinding (in lfIVariableBinding binding); */
  NS_IMETHOD ToStringWithBinding(lfIVariableBinding *binding, char **_retval) = 0;

  /** Checks if the ErlTerm contains a valid value (is initializated) */
  /* boolean isValid (); */
  NS_IMETHOD IsValid(PRBool *_retval) = 0;

  /**
     * Check if two terms are equal
     */
  /* boolean equals (in lfIErlTerm term); */
  NS_IMETHOD Equals(lfIErlTerm *term, PRBool *_retval) = 0;

  /**
     * Perfom a pattern matching
     */
  /* boolean match (in lfIErlTerm pattern); */
  NS_IMETHOD Match(lfIErlTerm *pattern, PRBool *_retval) = 0;

  /**
     * Perfom a pattern matching using a variable binding. If success
     * VariableBinding will be updated with new variables.
     */
  /* boolean matchWithBinding (in lfIErlTerm pattern, in lfIVariableBinding binding); */
  NS_IMETHOD MatchWithBinding(lfIErlTerm *pattern, lfIVariableBinding *binding, PRBool *_retval) = 0;

  /**
     * Returns the equivalent without inner variables, using the
     * given binding to substitute them.
     */
  /* lfIErlTerm subst (in lfIVariableBinding binding); */
  NS_IMETHOD Subst(lfIVariableBinding *binding, lfIErlTerm **_retval) = 0;

  /**
     * Returns the native EPI representation (ErlTerm *) of this ErlTerm.
     */
  /* [noscript] lfNativeErlTerm getErlTerm (); */
  NS_IMETHOD GetErlTerm(epi::type::ErlTerm * *_retval) = 0;

};

/* Use this macro when declaring classes that implement this interface. */
#define NS_DECL_LFIERLTERM \
  NS_IMETHOD ToString(char **_retval); \
  NS_IMETHOD ToStringWithBinding(lfIVariableBinding *binding, char **_retval); \
  NS_IMETHOD IsValid(PRBool *_retval); \
  NS_IMETHOD Equals(lfIErlTerm *term, PRBool *_retval); \
  NS_IMETHOD Match(lfIErlTerm *pattern, PRBool *_retval); \
  NS_IMETHOD MatchWithBinding(lfIErlTerm *pattern, lfIVariableBinding *binding, PRBool *_retval); \
  NS_IMETHOD Subst(lfIVariableBinding *binding, lfIErlTerm **_retval); \
  NS_IMETHOD GetErlTerm(epi::type::ErlTerm * *_retval); 

/* Use this macro to declare functions that forward the behavior of this interface to another object. */
#define NS_FORWARD_LFIERLTERM(_to) \
  NS_IMETHOD ToString(char **_retval) { return _to ToString(_retval); } \
  NS_IMETHOD ToStringWithBinding(lfIVariableBinding *binding, char **_retval) { return _to ToStringWithBinding(binding, _retval); } \
  NS_IMETHOD IsValid(PRBool *_retval) { return _to IsValid(_retval); } \
  NS_IMETHOD Equals(lfIErlTerm *term, PRBool *_retval) { return _to Equals(term, _retval); } \
  NS_IMETHOD Match(lfIErlTerm *pattern, PRBool *_retval) { return _to Match(pattern, _retval); } \
  NS_IMETHOD MatchWithBinding(lfIErlTerm *pattern, lfIVariableBinding *binding, PRBool *_retval) { return _to MatchWithBinding(pattern, binding, _retval); } \
  NS_IMETHOD Subst(lfIVariableBinding *binding, lfIErlTerm **_retval) { return _to Subst(binding, _retval); } \
  NS_IMETHOD GetErlTerm(epi::type::ErlTerm * *_retval) { return _to GetErlTerm(_retval); } 

/* Use this macro to declare functions that forward the behavior of this interface to another object in a safe way. */
#define NS_FORWARD_SAFE_LFIERLTERM(_to) \
  NS_IMETHOD ToString(char **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->ToString(_retval); } \
  NS_IMETHOD ToStringWithBinding(lfIVariableBinding *binding, char **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->ToStringWithBinding(binding, _retval); } \
  NS_IMETHOD IsValid(PRBool *_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->IsValid(_retval); } \
  NS_IMETHOD Equals(lfIErlTerm *term, PRBool *_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->Equals(term, _retval); } \
  NS_IMETHOD Match(lfIErlTerm *pattern, PRBool *_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->Match(pattern, _retval); } \
  NS_IMETHOD MatchWithBinding(lfIErlTerm *pattern, lfIVariableBinding *binding, PRBool *_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->MatchWithBinding(pattern, binding, _retval); } \
  NS_IMETHOD Subst(lfIVariableBinding *binding, lfIErlTerm **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->Subst(binding, _retval); } \
  NS_IMETHOD GetErlTerm(epi::type::ErlTerm * *_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetErlTerm(_retval); } 

#if 0
/* Use the code below as a template for the implementation class for this interface. */

/* Header file */
class lfErlTerm : public lfIErlTerm
{
public:
  NS_DECL_ISUPPORTS
  NS_DECL_LFIERLTERM

  lfErlTerm();

private:
  ~lfErlTerm();

protected:
  /* additional members */
};

/* Implementation file */
NS_IMPL_ISUPPORTS1(lfErlTerm, lfIErlTerm)

lfErlTerm::lfErlTerm()
{
  /* member initializers and constructor code */
}

lfErlTerm::~lfErlTerm()
{
  /* destructor code */
}

/* string toString (); */
NS_IMETHODIMP lfErlTerm::ToString(char **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* string toStringWithBinding (in lfIVariableBinding binding); */
NS_IMETHODIMP lfErlTerm::ToStringWithBinding(lfIVariableBinding *binding, char **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* boolean isValid (); */
NS_IMETHODIMP lfErlTerm::IsValid(PRBool *_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* boolean equals (in lfIErlTerm term); */
NS_IMETHODIMP lfErlTerm::Equals(lfIErlTerm *term, PRBool *_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* boolean match (in lfIErlTerm pattern); */
NS_IMETHODIMP lfErlTerm::Match(lfIErlTerm *pattern, PRBool *_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* boolean matchWithBinding (in lfIErlTerm pattern, in lfIVariableBinding binding); */
NS_IMETHODIMP lfErlTerm::MatchWithBinding(lfIErlTerm *pattern, lfIVariableBinding *binding, PRBool *_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlTerm subst (in lfIVariableBinding binding); */
NS_IMETHODIMP lfErlTerm::Subst(lfIVariableBinding *binding, lfIErlTerm **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* [noscript] lfNativeErlTerm getErlTerm (); */
NS_IMETHODIMP lfErlTerm::GetErlTerm(epi::type::ErlTerm * *_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* End of implementation class template. */
#endif


#endif /* __gen_lfIErlTerm_h__ */

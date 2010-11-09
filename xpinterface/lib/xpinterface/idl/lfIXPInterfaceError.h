/*
 * DO NOT EDIT.  THIS FILE IS GENERATED FROM lib/xpinterface/idl/lfIXPInterfaceError.idl
 */

#ifndef __gen_lfIXPInterfaceError_h__
#define __gen_lfIXPInterfaceError_h__


#ifndef __gen_nsISupports_h__
#include "nsISupports.h"
#endif

/* For IDL files that don't want to include root IDL files. */
#ifndef NS_NO_VTABLE
#define NS_NO_VTABLE
#endif

/* starting interface:    lfIXPInterfaceError */
#define LFIXPINTERFACEERROR_IID_STR "431bf008-1bb5-402a-88ab-b2fd63515d5e"

#define LFIXPINTERFACEERROR_IID \
  {0x431bf008, 0x1bb5, 0x402a, \
    { 0x88, 0xab, 0xb2, 0xfd, 0x63, 0x51, 0x5d, 0x5e }}

class NS_NO_VTABLE lfIXPInterfaceError : public nsISupports {
 public: 

  NS_DEFINE_STATIC_IID_ACCESSOR(LFIXPINTERFACEERROR_IID)

  enum { MODULE_XPINTERFACE = 40 };

  enum { SUCCESS = 0 };

  enum { EMPTYLIST_ERROR = 1 };

  enum { CONNECTION_ERROR = 2 };

  enum { NETWORK_ERROR = 3 };

  enum { DECODE_ERROR = 4 };

  enum { ENCODE_ERROR = 5 };

};

/* Use this macro when declaring classes that implement this interface. */
#define NS_DECL_LFIXPINTERFACEERROR \

/* Use this macro to declare functions that forward the behavior of this interface to another object. */
#define NS_FORWARD_LFIXPINTERFACEERROR(_to) \

/* Use this macro to declare functions that forward the behavior of this interface to another object in a safe way. */
#define NS_FORWARD_SAFE_LFIXPINTERFACEERROR(_to) \

#if 0
/* Use the code below as a template for the implementation class for this interface. */

/* Header file */
class lfXPInterfaceError : public lfIXPInterfaceError
{
public:
  NS_DECL_ISUPPORTS
  NS_DECL_LFIXPINTERFACEERROR

  lfXPInterfaceError();

private:
  ~lfXPInterfaceError();

protected:
  /* additional members */
};

/* Implementation file */
NS_IMPL_ISUPPORTS1(lfXPInterfaceError, lfIXPInterfaceError)

lfXPInterfaceError::lfXPInterfaceError()
{
  /* member initializers and constructor code */
}

lfXPInterfaceError::~lfXPInterfaceError()
{
  /* destructor code */
}

/* End of implementation class template. */
#endif

#define NS_ERROR_MODULE_XPINTERFACE lfIXPInterfaceError::MODULE_XPINTERFACE
#define NS_ERROR_XPINTERFACE_EMPTYLIST \
    NS_ERROR_GENERATE_FAILURE(NS_ERROR_MODULE_XPINTERFACE, \
                              lfIXPInterfaceError::EMPTYLIST_ERROR)
#define NS_ERROR_XPINTERFACE_CONNECTION \
    NS_ERROR_GENERATE_FAILURE(NS_ERROR_MODULE_XPINTERFACE, \
                              lfIXPInterfaceError::CONNECTION_ERROR)
#define NS_ERROR_XPINTERFACE_NETWORK \
    NS_ERROR_GENERATE_FAILURE(NS_ERROR_MODULE_XPINTERFACE, \
                              lfIXPInterfaceError::NETWORK_ERROR)
#define NS_ERROR_XPINTERFACE_DECODE \
    NS_ERROR_GENERATE_FAILURE(NS_ERROR_MODULE_XPINTERFACE, \
                              lfIXPInterfaceError::DECODE_ERROR)
#define NS_ERROR_XPINTERFACE_ENCODE \
    NS_ERROR_GENERATE_FAILURE(NS_ERROR_MODULE_XPINTERFACE, \
                              lfIXPInterfaceError::ENCODE_ERROR)

#endif /* __gen_lfIXPInterfaceError_h__ */

/*
 * DO NOT EDIT.  THIS FILE IS GENERATED FROM lib/xpinterface/idl/lfIErlPort.idl
 */

#ifndef __gen_lfIErlPort_h__
#define __gen_lfIErlPort_h__


#ifndef __gen_lfIErlTerm_h__
#include "lfIErlTerm.h"
#endif

/* For IDL files that don't want to include root IDL files. */
#ifndef NS_NO_VTABLE
#define NS_NO_VTABLE
#endif

/* starting interface:    lfIErlPort */
#define LFIERLPORT_IID_STR "1509c80e-7234-4952-ae12-28cfdb5c80f9"

#define LFIERLPORT_IID \
  {0x1509c80e, 0x7234, 0x4952, \
    { 0xae, 0x12, 0x28, 0xcf, 0xdb, 0x5c, 0x80, 0xf9 }}

class NS_NO_VTABLE lfIErlPort : public lfIErlTerm {
 public: 

  NS_DEFINE_STATIC_IID_ACCESSOR(LFIERLPORT_IID)

  /** the node name from the Port. */
  /* readonly attribute string node; */
  NS_IMETHOD GetNode(char * *aNode) = 0;

  /** the id number from the Port. */
  /* readonly attribute unsigned long id; */
  NS_IMETHOD GetId(PRUint32 *aId) = 0;

  /** the creation number from the Port. */
  /* readonly attribute unsigned long creation; */
  NS_IMETHOD GetCreation(PRUint32 *aCreation) = 0;

};

/* Use this macro when declaring classes that implement this interface. */
#define NS_DECL_LFIERLPORT \
  NS_IMETHOD GetNode(char * *aNode); \
  NS_IMETHOD GetId(PRUint32 *aId); \
  NS_IMETHOD GetCreation(PRUint32 *aCreation); 

/* Use this macro to declare functions that forward the behavior of this interface to another object. */
#define NS_FORWARD_LFIERLPORT(_to) \
  NS_IMETHOD GetNode(char * *aNode) { return _to GetNode(aNode); } \
  NS_IMETHOD GetId(PRUint32 *aId) { return _to GetId(aId); } \
  NS_IMETHOD GetCreation(PRUint32 *aCreation) { return _to GetCreation(aCreation); } 

/* Use this macro to declare functions that forward the behavior of this interface to another object in a safe way. */
#define NS_FORWARD_SAFE_LFIERLPORT(_to) \
  NS_IMETHOD GetNode(char * *aNode) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetNode(aNode); } \
  NS_IMETHOD GetId(PRUint32 *aId) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetId(aId); } \
  NS_IMETHOD GetCreation(PRUint32 *aCreation) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetCreation(aCreation); } 

#if 0
/* Use the code below as a template for the implementation class for this interface. */

/* Header file */
class lfErlPort : public lfIErlPort
{
public:
  NS_DECL_ISUPPORTS
  NS_DECL_LFIERLPORT

  lfErlPort();

private:
  ~lfErlPort();

protected:
  /* additional members */
};

/* Implementation file */
NS_IMPL_ISUPPORTS1(lfErlPort, lfIErlPort)

lfErlPort::lfErlPort()
{
  /* member initializers and constructor code */
}

lfErlPort::~lfErlPort()
{
  /* destructor code */
}

/* readonly attribute string node; */
NS_IMETHODIMP lfErlPort::GetNode(char * *aNode)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* readonly attribute unsigned long id; */
NS_IMETHODIMP lfErlPort::GetId(PRUint32 *aId)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* readonly attribute unsigned long creation; */
NS_IMETHODIMP lfErlPort::GetCreation(PRUint32 *aCreation)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* End of implementation class template. */
#endif


#endif /* __gen_lfIErlPort_h__ */

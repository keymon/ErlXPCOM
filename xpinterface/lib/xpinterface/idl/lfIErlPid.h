/*
 * DO NOT EDIT.  THIS FILE IS GENERATED FROM lib/xpinterface/idl/lfIErlPid.idl
 */

#ifndef __gen_lfIErlPid_h__
#define __gen_lfIErlPid_h__


#ifndef __gen_lfIErlTerm_h__
#include "lfIErlTerm.h"
#endif

/* For IDL files that don't want to include root IDL files. */
#ifndef NS_NO_VTABLE
#define NS_NO_VTABLE
#endif

/* starting interface:    lfIErlPid */
#define LFIERLPID_IID_STR "71d85f38-9308-47f3-b785-f2a7b1c0af32"

#define LFIERLPID_IID \
  {0x71d85f38, 0x9308, 0x47f3, \
    { 0xb7, 0x85, 0xf2, 0xa7, 0xb1, 0xc0, 0xaf, 0x32 }}

class NS_NO_VTABLE lfIErlPid : public lfIErlTerm {
 public: 

  NS_DEFINE_STATIC_IID_ACCESSOR(LFIERLPID_IID)

  /** the node name from the PID. */
  /* readonly attribute string node; */
  NS_IMETHOD GetNode(char * *aNode) = 0;

  /** the id number from the PID. */
  /* readonly attribute unsigned long id; */
  NS_IMETHOD GetId(PRUint32 *aId) = 0;

  /** the serial number from the PID. */
  /* readonly attribute unsigned long serial; */
  NS_IMETHOD GetSerial(PRUint32 *aSerial) = 0;

  /** the creation number from the PID. */
  /* readonly attribute unsigned long creation; */
  NS_IMETHOD GetCreation(PRUint32 *aCreation) = 0;

};

/* Use this macro when declaring classes that implement this interface. */
#define NS_DECL_LFIERLPID \
  NS_IMETHOD GetNode(char * *aNode); \
  NS_IMETHOD GetId(PRUint32 *aId); \
  NS_IMETHOD GetSerial(PRUint32 *aSerial); \
  NS_IMETHOD GetCreation(PRUint32 *aCreation); 

/* Use this macro to declare functions that forward the behavior of this interface to another object. */
#define NS_FORWARD_LFIERLPID(_to) \
  NS_IMETHOD GetNode(char * *aNode) { return _to GetNode(aNode); } \
  NS_IMETHOD GetId(PRUint32 *aId) { return _to GetId(aId); } \
  NS_IMETHOD GetSerial(PRUint32 *aSerial) { return _to GetSerial(aSerial); } \
  NS_IMETHOD GetCreation(PRUint32 *aCreation) { return _to GetCreation(aCreation); } 

/* Use this macro to declare functions that forward the behavior of this interface to another object in a safe way. */
#define NS_FORWARD_SAFE_LFIERLPID(_to) \
  NS_IMETHOD GetNode(char * *aNode) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetNode(aNode); } \
  NS_IMETHOD GetId(PRUint32 *aId) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetId(aId); } \
  NS_IMETHOD GetSerial(PRUint32 *aSerial) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetSerial(aSerial); } \
  NS_IMETHOD GetCreation(PRUint32 *aCreation) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetCreation(aCreation); } 

#if 0
/* Use the code below as a template for the implementation class for this interface. */

/* Header file */
class lfErlPid : public lfIErlPid
{
public:
  NS_DECL_ISUPPORTS
  NS_DECL_LFIERLPID

  lfErlPid();

private:
  ~lfErlPid();

protected:
  /* additional members */
};

/* Implementation file */
NS_IMPL_ISUPPORTS1(lfErlPid, lfIErlPid)

lfErlPid::lfErlPid()
{
  /* member initializers and constructor code */
}

lfErlPid::~lfErlPid()
{
  /* destructor code */
}

/* readonly attribute string node; */
NS_IMETHODIMP lfErlPid::GetNode(char * *aNode)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* readonly attribute unsigned long id; */
NS_IMETHODIMP lfErlPid::GetId(PRUint32 *aId)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* readonly attribute unsigned long serial; */
NS_IMETHODIMP lfErlPid::GetSerial(PRUint32 *aSerial)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* readonly attribute unsigned long creation; */
NS_IMETHODIMP lfErlPid::GetCreation(PRUint32 *aCreation)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* End of implementation class template. */
#endif


#endif /* __gen_lfIErlPid_h__ */

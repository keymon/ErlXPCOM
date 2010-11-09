/*
 * DO NOT EDIT.  THIS FILE IS GENERATED FROM lib/xpinterface/idl/lfIErlRef.idl
 */

#ifndef __gen_lfIErlRef_h__
#define __gen_lfIErlRef_h__


#ifndef __gen_lfIErlTerm_h__
#include "lfIErlTerm.h"
#endif

/* For IDL files that don't want to include root IDL files. */
#ifndef NS_NO_VTABLE
#define NS_NO_VTABLE
#endif

/* starting interface:    lfIErlRef */
#define LFIERLREF_IID_STR "7e50d059-bda4-441d-91d9-6c2047a471f1"

#define LFIERLREF_IID \
  {0x7e50d059, 0xbda4, 0x441d, \
    { 0x91, 0xd9, 0x6c, 0x20, 0x47, 0xa4, 0x71, 0xf1 }}

class NS_NO_VTABLE lfIErlRef : public lfIErlTerm {
 public: 

  NS_DEFINE_STATIC_IID_ACCESSOR(LFIERLREF_IID)

  /** the node name from the Ref. */
  /* readonly attribute string node; */
  NS_IMETHOD GetNode(char * *aNode) = 0;

  /** the id(0) number from the Ref. */
  /* readonly attribute unsigned long id0; */
  NS_IMETHOD GetId0(PRUint32 *aId0) = 0;

  /** the id(1) number from the Ref. */
  /* readonly attribute unsigned long id1; */
  NS_IMETHOD GetId1(PRUint32 *aId1) = 0;

  /** the id(2) number from the Ref. */
  /* readonly attribute unsigned long id2; */
  NS_IMETHOD GetId2(PRUint32 *aId2) = 0;

  /** indicates if this an old/new style Ref */
  /* readonly attribute boolean newStyle; */
  NS_IMETHOD GetNewStyle(PRBool *aNewStyle) = 0;

  /** the creation number from the Ref. */
  /* readonly attribute unsigned long creation; */
  NS_IMETHOD GetCreation(PRUint32 *aCreation) = 0;

};

/* Use this macro when declaring classes that implement this interface. */
#define NS_DECL_LFIERLREF \
  NS_IMETHOD GetNode(char * *aNode); \
  NS_IMETHOD GetId0(PRUint32 *aId0); \
  NS_IMETHOD GetId1(PRUint32 *aId1); \
  NS_IMETHOD GetId2(PRUint32 *aId2); \
  NS_IMETHOD GetNewStyle(PRBool *aNewStyle); \
  NS_IMETHOD GetCreation(PRUint32 *aCreation); 

/* Use this macro to declare functions that forward the behavior of this interface to another object. */
#define NS_FORWARD_LFIERLREF(_to) \
  NS_IMETHOD GetNode(char * *aNode) { return _to GetNode(aNode); } \
  NS_IMETHOD GetId0(PRUint32 *aId0) { return _to GetId0(aId0); } \
  NS_IMETHOD GetId1(PRUint32 *aId1) { return _to GetId1(aId1); } \
  NS_IMETHOD GetId2(PRUint32 *aId2) { return _to GetId2(aId2); } \
  NS_IMETHOD GetNewStyle(PRBool *aNewStyle) { return _to GetNewStyle(aNewStyle); } \
  NS_IMETHOD GetCreation(PRUint32 *aCreation) { return _to GetCreation(aCreation); } 

/* Use this macro to declare functions that forward the behavior of this interface to another object in a safe way. */
#define NS_FORWARD_SAFE_LFIERLREF(_to) \
  NS_IMETHOD GetNode(char * *aNode) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetNode(aNode); } \
  NS_IMETHOD GetId0(PRUint32 *aId0) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetId0(aId0); } \
  NS_IMETHOD GetId1(PRUint32 *aId1) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetId1(aId1); } \
  NS_IMETHOD GetId2(PRUint32 *aId2) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetId2(aId2); } \
  NS_IMETHOD GetNewStyle(PRBool *aNewStyle) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetNewStyle(aNewStyle); } \
  NS_IMETHOD GetCreation(PRUint32 *aCreation) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetCreation(aCreation); } 

#if 0
/* Use the code below as a template for the implementation class for this interface. */

/* Header file */
class lfErlRef : public lfIErlRef
{
public:
  NS_DECL_ISUPPORTS
  NS_DECL_LFIERLREF

  lfErlRef();

private:
  ~lfErlRef();

protected:
  /* additional members */
};

/* Implementation file */
NS_IMPL_ISUPPORTS1(lfErlRef, lfIErlRef)

lfErlRef::lfErlRef()
{
  /* member initializers and constructor code */
}

lfErlRef::~lfErlRef()
{
  /* destructor code */
}

/* readonly attribute string node; */
NS_IMETHODIMP lfErlRef::GetNode(char * *aNode)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* readonly attribute unsigned long id0; */
NS_IMETHODIMP lfErlRef::GetId0(PRUint32 *aId0)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* readonly attribute unsigned long id1; */
NS_IMETHODIMP lfErlRef::GetId1(PRUint32 *aId1)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* readonly attribute unsigned long id2; */
NS_IMETHODIMP lfErlRef::GetId2(PRUint32 *aId2)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* readonly attribute boolean newStyle; */
NS_IMETHODIMP lfErlRef::GetNewStyle(PRBool *aNewStyle)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* readonly attribute unsigned long creation; */
NS_IMETHODIMP lfErlRef::GetCreation(PRUint32 *aCreation)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* End of implementation class template. */
#endif


#endif /* __gen_lfIErlRef_h__ */

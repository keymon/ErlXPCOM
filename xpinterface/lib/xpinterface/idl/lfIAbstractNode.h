/*
 * DO NOT EDIT.  THIS FILE IS GENERATED FROM lib/xpinterface/idl/lfIAbstractNode.idl
 */

#ifndef __gen_lfIAbstractNode_h__
#define __gen_lfIAbstractNode_h__


#ifndef __gen_nsISupports_h__
#include "nsISupports.h"
#endif

/* For IDL files that don't want to include root IDL files. */
#ifndef NS_NO_VTABLE
#define NS_NO_VTABLE
#endif

/* starting interface:    lfIAbstractNode */
#define LFIABSTRACTNODE_IID_STR "4dde9d13-e521-4dae-9cd3-d192dca13dc4"

#define LFIABSTRACTNODE_IID \
  {0x4dde9d13, 0xe521, 0x4dae, \
    { 0x9c, 0xd3, 0xd1, 0x92, 0xdc, 0xa1, 0x3d, 0xc4 }}

class NS_NO_VTABLE lfIAbstractNode : public nsISupports {
 public: 

  NS_DEFINE_STATIC_IID_ACCESSOR(LFIABSTRACTNODE_IID)

  /* readonly attribute string nodeName; */
  NS_IMETHOD GetNodeName(char * *aNodeName) = 0;

  /* readonly attribute string aliveName; */
  NS_IMETHOD GetAliveName(char * *aAliveName) = 0;

  /* readonly attribute string hostName; */
  NS_IMETHOD GetHostName(char * *aHostName) = 0;

  /* attribute string cookie; */
  NS_IMETHOD GetCookie(char * *aCookie) = 0;
  NS_IMETHOD SetCookie(const char * aCookie) = 0;

};

/* Use this macro when declaring classes that implement this interface. */
#define NS_DECL_LFIABSTRACTNODE \
  NS_IMETHOD GetNodeName(char * *aNodeName); \
  NS_IMETHOD GetAliveName(char * *aAliveName); \
  NS_IMETHOD GetHostName(char * *aHostName); \
  NS_IMETHOD GetCookie(char * *aCookie); \
  NS_IMETHOD SetCookie(const char * aCookie); 

/* Use this macro to declare functions that forward the behavior of this interface to another object. */
#define NS_FORWARD_LFIABSTRACTNODE(_to) \
  NS_IMETHOD GetNodeName(char * *aNodeName) { return _to GetNodeName(aNodeName); } \
  NS_IMETHOD GetAliveName(char * *aAliveName) { return _to GetAliveName(aAliveName); } \
  NS_IMETHOD GetHostName(char * *aHostName) { return _to GetHostName(aHostName); } \
  NS_IMETHOD GetCookie(char * *aCookie) { return _to GetCookie(aCookie); } \
  NS_IMETHOD SetCookie(const char * aCookie) { return _to SetCookie(aCookie); } 

/* Use this macro to declare functions that forward the behavior of this interface to another object in a safe way. */
#define NS_FORWARD_SAFE_LFIABSTRACTNODE(_to) \
  NS_IMETHOD GetNodeName(char * *aNodeName) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetNodeName(aNodeName); } \
  NS_IMETHOD GetAliveName(char * *aAliveName) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetAliveName(aAliveName); } \
  NS_IMETHOD GetHostName(char * *aHostName) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetHostName(aHostName); } \
  NS_IMETHOD GetCookie(char * *aCookie) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetCookie(aCookie); } \
  NS_IMETHOD SetCookie(const char * aCookie) { return !_to ? NS_ERROR_NULL_POINTER : _to->SetCookie(aCookie); } 

#if 0
/* Use the code below as a template for the implementation class for this interface. */

/* Header file */
class lfAbstractNode : public lfIAbstractNode
{
public:
  NS_DECL_ISUPPORTS
  NS_DECL_LFIABSTRACTNODE

  lfAbstractNode();

private:
  ~lfAbstractNode();

protected:
  /* additional members */
};

/* Implementation file */
NS_IMPL_ISUPPORTS1(lfAbstractNode, lfIAbstractNode)

lfAbstractNode::lfAbstractNode()
{
  /* member initializers and constructor code */
}

lfAbstractNode::~lfAbstractNode()
{
  /* destructor code */
}

/* readonly attribute string nodeName; */
NS_IMETHODIMP lfAbstractNode::GetNodeName(char * *aNodeName)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* readonly attribute string aliveName; */
NS_IMETHODIMP lfAbstractNode::GetAliveName(char * *aAliveName)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* readonly attribute string hostName; */
NS_IMETHODIMP lfAbstractNode::GetHostName(char * *aHostName)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* attribute string cookie; */
NS_IMETHODIMP lfAbstractNode::GetCookie(char * *aCookie)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}
NS_IMETHODIMP lfAbstractNode::SetCookie(const char * aCookie)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* End of implementation class template. */
#endif


#endif /* __gen_lfIAbstractNode_h__ */

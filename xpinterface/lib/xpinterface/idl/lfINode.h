/*
 * DO NOT EDIT.  THIS FILE IS GENERATED FROM lib/xpinterface/idl/lfINode.idl
 */

#ifndef __gen_lfINode_h__
#define __gen_lfINode_h__


#ifndef __gen_nsISupports_h__
#include "nsISupports.h"
#endif

#ifndef __gen_lfIAbstractNode_h__
#include "lfIAbstractNode.h"
#endif

#ifndef __gen_lfIMailBox_h__
#include "lfIMailBox.h"
#endif

#ifndef __gen_lfIErlPid_h__
#include "lfIErlPid.h"
#endif

#ifndef __gen_lfIErlPort_h__
#include "lfIErlPort.h"
#endif

#ifndef __gen_lfIErlRef_h__
#include "lfIErlRef.h"
#endif

/* For IDL files that don't want to include root IDL files. */
#ifndef NS_NO_VTABLE
#define NS_NO_VTABLE
#endif

/* starting interface:    lfINode */
#define LFINODE_IID_STR "0e08bf2b-10cb-4c0c-9666-9e451948046e"

#define LFINODE_IID \
  {0x0e08bf2b, 0x10cb, 0x4c0c, \
    { 0x96, 0x66, 0x9e, 0x45, 0x19, 0x48, 0x04, 0x6e }}

class NS_NO_VTABLE lfINode : public lfIAbstractNode {
 public: 

  NS_DEFINE_STATIC_IID_ACCESSOR(LFINODE_IID)

  /**
     * Create a new MailBox with a new pid associated to this node.
     */
  /* lfIMailBox createMailBox (); */
  NS_IMETHOD CreateMailBox(lfIMailBox **_retval) = 0;

  /**
     * Register an MailBox with given name. MailBox must be associated
     * to this node.
     */
  /* void registerMailBox (in string name, in lfIMailBox mailbox); */
  NS_IMETHOD RegisterMailBox(const char *name, lfIMailBox *mailbox) = 0;

  /**
     * Unregister a name for a mailbox
     */
  /* void unRegisterName (in string name); */
  NS_IMETHOD UnRegisterName(const char *name) = 0;

  /**
     * Unregister all names for this mailbox
     */
  /* void unRegisterMailBox (in lfIMailBox mailbox); */
  NS_IMETHOD UnRegisterMailBox(lfIMailBox *mailbox) = 0;

  /**
     * Create a new unique pid
     */
  /* lfIErlPid createPid (); */
  NS_IMETHOD CreatePid(lfIErlPid **_retval) = 0;

  /**
     * Create a new unique port
     */
  /* lfIErlPort createPort (); */
  NS_IMETHOD CreatePort(lfIErlPort **_retval) = 0;

  /**
     * Create a new unique ref
     */
  /* lfIErlRef createRef (); */
  NS_IMETHOD CreateRef(lfIErlRef **_retval) = 0;

  /**
	 * ping a remote node 
	 */
  /* boolean ping (in string remotename, in long timeout); */
  NS_IMETHOD Ping(const char *remotename, PRInt32 timeout, PRBool *_retval) = 0;

};

/* Use this macro when declaring classes that implement this interface. */
#define NS_DECL_LFINODE \
  NS_IMETHOD CreateMailBox(lfIMailBox **_retval); \
  NS_IMETHOD RegisterMailBox(const char *name, lfIMailBox *mailbox); \
  NS_IMETHOD UnRegisterName(const char *name); \
  NS_IMETHOD UnRegisterMailBox(lfIMailBox *mailbox); \
  NS_IMETHOD CreatePid(lfIErlPid **_retval); \
  NS_IMETHOD CreatePort(lfIErlPort **_retval); \
  NS_IMETHOD CreateRef(lfIErlRef **_retval); \
  NS_IMETHOD Ping(const char *remotename, PRInt32 timeout, PRBool *_retval); 

/* Use this macro to declare functions that forward the behavior of this interface to another object. */
#define NS_FORWARD_LFINODE(_to) \
  NS_IMETHOD CreateMailBox(lfIMailBox **_retval) { return _to CreateMailBox(_retval); } \
  NS_IMETHOD RegisterMailBox(const char *name, lfIMailBox *mailbox) { return _to RegisterMailBox(name, mailbox); } \
  NS_IMETHOD UnRegisterName(const char *name) { return _to UnRegisterName(name); } \
  NS_IMETHOD UnRegisterMailBox(lfIMailBox *mailbox) { return _to UnRegisterMailBox(mailbox); } \
  NS_IMETHOD CreatePid(lfIErlPid **_retval) { return _to CreatePid(_retval); } \
  NS_IMETHOD CreatePort(lfIErlPort **_retval) { return _to CreatePort(_retval); } \
  NS_IMETHOD CreateRef(lfIErlRef **_retval) { return _to CreateRef(_retval); } \
  NS_IMETHOD Ping(const char *remotename, PRInt32 timeout, PRBool *_retval) { return _to Ping(remotename, timeout, _retval); } 

/* Use this macro to declare functions that forward the behavior of this interface to another object in a safe way. */
#define NS_FORWARD_SAFE_LFINODE(_to) \
  NS_IMETHOD CreateMailBox(lfIMailBox **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->CreateMailBox(_retval); } \
  NS_IMETHOD RegisterMailBox(const char *name, lfIMailBox *mailbox) { return !_to ? NS_ERROR_NULL_POINTER : _to->RegisterMailBox(name, mailbox); } \
  NS_IMETHOD UnRegisterName(const char *name) { return !_to ? NS_ERROR_NULL_POINTER : _to->UnRegisterName(name); } \
  NS_IMETHOD UnRegisterMailBox(lfIMailBox *mailbox) { return !_to ? NS_ERROR_NULL_POINTER : _to->UnRegisterMailBox(mailbox); } \
  NS_IMETHOD CreatePid(lfIErlPid **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->CreatePid(_retval); } \
  NS_IMETHOD CreatePort(lfIErlPort **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->CreatePort(_retval); } \
  NS_IMETHOD CreateRef(lfIErlRef **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->CreateRef(_retval); } \
  NS_IMETHOD Ping(const char *remotename, PRInt32 timeout, PRBool *_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->Ping(remotename, timeout, _retval); } 

#if 0
/* Use the code below as a template for the implementation class for this interface. */

/* Header file */
class lfNode : public lfINode
{
public:
  NS_DECL_ISUPPORTS
  NS_DECL_LFINODE

  lfNode();

private:
  ~lfNode();

protected:
  /* additional members */
};

/* Implementation file */
NS_IMPL_ISUPPORTS1(lfNode, lfINode)

lfNode::lfNode()
{
  /* member initializers and constructor code */
}

lfNode::~lfNode()
{
  /* destructor code */
}

/* lfIMailBox createMailBox (); */
NS_IMETHODIMP lfNode::CreateMailBox(lfIMailBox **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void registerMailBox (in string name, in lfIMailBox mailbox); */
NS_IMETHODIMP lfNode::RegisterMailBox(const char *name, lfIMailBox *mailbox)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void unRegisterName (in string name); */
NS_IMETHODIMP lfNode::UnRegisterName(const char *name)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void unRegisterMailBox (in lfIMailBox mailbox); */
NS_IMETHODIMP lfNode::UnRegisterMailBox(lfIMailBox *mailbox)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlPid createPid (); */
NS_IMETHODIMP lfNode::CreatePid(lfIErlPid **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlPort createPort (); */
NS_IMETHODIMP lfNode::CreatePort(lfIErlPort **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlRef createRef (); */
NS_IMETHODIMP lfNode::CreateRef(lfIErlRef **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* boolean ping (in string remotename, in long timeout); */
NS_IMETHODIMP lfNode::Ping(const char *remotename, PRInt32 timeout, PRBool *_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* End of implementation class template. */
#endif


#endif /* __gen_lfINode_h__ */

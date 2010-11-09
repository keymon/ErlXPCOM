/*
 * DO NOT EDIT.  THIS FILE IS GENERATED FROM lib/xpinterface/idl/lfIMailBox.idl
 */

#ifndef __gen_lfIMailBox_h__
#define __gen_lfIMailBox_h__


#ifndef __gen_nsISupports_h__
#include "nsISupports.h"
#endif

#ifndef __gen_lfIErlTerm_h__
#include "lfIErlTerm.h"
#endif

#ifndef __gen_lfIErlPid_h__
#include "lfIErlPid.h"
#endif

#ifndef __gen_lfIErlList_h__
#include "lfIErlList.h"
#endif

#ifndef __gen_lfIVariableBinding_h__
#include "lfIVariableBinding.h"
#endif

/* For IDL files that don't want to include root IDL files. */
#ifndef NS_NO_VTABLE
#define NS_NO_VTABLE
#endif

/* starting interface:    lfIMailBox */
#define LFIMAILBOX_IID_STR "eabe3906-821e-4534-84b7-1458d0287d17"

#define LFIMAILBOX_IID \
  {0xeabe3906, 0x821e, 0x4534, \
    { 0x84, 0xb7, 0x14, 0x58, 0xd0, 0x28, 0x7d, 0x17 }}

class NS_NO_VTABLE lfIMailBox : public nsISupports {
 public: 

  NS_DEFINE_STATIC_IID_ACCESSOR(LFIMAILBOX_IID)

  /**
     * mailbox pid
     */
  /* readonly attribute lfIErlPid self; */
  NS_IMETHOD GetSelf(lfIErlPid * *aSelf) = 0;

  /**
     * Send data to a proccess
     * @param toPid receiver Pid
     * @param term ErlTerm to send
     */
  /* void send (in lfIErlPid toPid, in lfIErlTerm term); */
  NS_IMETHOD Send(lfIErlPid *toPid, lfIErlTerm *term) = 0;

  /**
     * Send data to locally registered process
     * @param toName detination registered name
     * @param term ErlTerm to send
     */
  /* void regSend (in string toName, in lfIErlTerm term); */
  NS_IMETHOD RegSend(const char *toName, lfIErlTerm *term) = 0;

  /**
     * Send data to remote registered process
     * @param toNode remote node name
     * @param toName detination registered name
     * @param term ErlTerm to send
     */
  /* void remoteRegSend (in string toNode, in string toName, in lfIErlTerm term); */
  NS_IMETHOD RemoteRegSend(const char *toNode, const char *toName, lfIErlTerm *term) = 0;

  /**
     * Send an RPC request to a remote Erlang node.
     * @param node the name of the Erlang node to send rpc
     * @param mod the name of the Erlang module containing the
     * function to be called.
     * @param fun the name of the function to call.
     * @param args a list of Erlang terms, to be used as arguments
     * to the function.
     */
  /* void sendRPC (in string node, in string mod, in string fun, in lfIErlList args); */
  NS_IMETHOD SendRPC(const char *node, const char *mod, const char *fun, lfIErlList *args) = 0;

  /**
     * Get a message from this mailbox.
     * Block until  a message arrives.
     * @return the ErlTerm representing
     * the body of the next message waiting in this mailbox
     **/
  /* lfIErlTerm receive (); */
  NS_IMETHOD Receive(lfIErlTerm **_retval) = 0;

  /**
     * Get a message from this mailbox.
     * Block until  a message arrives for this mailbox no more than timeout ms.
     * @param timeout the time, in milliseconds, to wait for a message
     * before returning 0.
     * @return the ErlTerm representing
     * the body of the next message waiting in this mailbox, or
     * null if timeout is reached.
     **/
  /* lfIErlTerm receiveWithTimeout (in long timeout); */
  NS_IMETHOD ReceiveWithTimeout(PRInt32 timeout, lfIErlTerm **_retval) = 0;

  /**
     * Get a message from mailbox that matches the given pattern.
     * It will block until an apropiate message arrives.
     * @param pattern ErlTerm with pattern to check
     * @param binding VariableBinding to use. It can be null.
     * @return the ErlTerm representing the body of the next message
     * waiting in this mailbox.
     * @exception EpiConnectionException if there was an connection error
     */
  /* lfIErlTerm receivePattern (in lfIErlTerm pattern, in lfIVariableBinding binding); */
  NS_IMETHOD ReceivePattern(lfIErlTerm *pattern, lfIVariableBinding *binding, lfIErlTerm **_retval) = 0;

  /**
     * Get a message from mailbox that matches the given pattern.
     * It will block until an apropiate message arrives.
     * @param pattern ErlTerm with pattern to check
     * @param binding VariableBinding to use. It can be null.
     * @param timeout Max time in ms to wait
     * @return the ErlTerm representing the body of the next message
     * waiting in this mailbox.
     * @exception EpiConnectionException if there was an connection error
     */
  /* lfIErlTerm receivePatternWithTimeout (in lfIErlTerm pattern, in lfIVariableBinding binding, in long timeout); */
  NS_IMETHOD ReceivePatternWithTimeout(lfIErlTerm *pattern, lfIVariableBinding *binding, PRInt32 timeout, lfIErlTerm **_retval) = 0;

  /**
     * Receive an RPC reply from the remote Erlang node.
     * @param timeout The maximum time (in ms) to wait for results
     */
  /* lfIErlTerm receiveRPC (); */
  NS_IMETHOD ReceiveRPC(lfIErlTerm **_retval) = 0;

  /**
     * Receive an RPC reply from the remote Erlang node.
     * @param timeout The maximum time (in ms) to wait for results
     */
  /* lfIErlTerm receiveRPCWithTimeout (in long timeout); */
  NS_IMETHOD ReceiveRPCWithTimeout(PRInt32 timeout, lfIErlTerm **_retval) = 0;

  /**
     * Returns the native EPI representation
     * (MailBox *) of this mailbox.
     */
  /* [noscript] lfNativeMailBox getNative (); */
  NS_IMETHOD GetNative(epi::node::MailBox * *_retval) = 0;

};

/* Use this macro when declaring classes that implement this interface. */
#define NS_DECL_LFIMAILBOX \
  NS_IMETHOD GetSelf(lfIErlPid * *aSelf); \
  NS_IMETHOD Send(lfIErlPid *toPid, lfIErlTerm *term); \
  NS_IMETHOD RegSend(const char *toName, lfIErlTerm *term); \
  NS_IMETHOD RemoteRegSend(const char *toNode, const char *toName, lfIErlTerm *term); \
  NS_IMETHOD SendRPC(const char *node, const char *mod, const char *fun, lfIErlList *args); \
  NS_IMETHOD Receive(lfIErlTerm **_retval); \
  NS_IMETHOD ReceiveWithTimeout(PRInt32 timeout, lfIErlTerm **_retval); \
  NS_IMETHOD ReceivePattern(lfIErlTerm *pattern, lfIVariableBinding *binding, lfIErlTerm **_retval); \
  NS_IMETHOD ReceivePatternWithTimeout(lfIErlTerm *pattern, lfIVariableBinding *binding, PRInt32 timeout, lfIErlTerm **_retval); \
  NS_IMETHOD ReceiveRPC(lfIErlTerm **_retval); \
  NS_IMETHOD ReceiveRPCWithTimeout(PRInt32 timeout, lfIErlTerm **_retval); \
  NS_IMETHOD GetNative(epi::node::MailBox * *_retval); 

/* Use this macro to declare functions that forward the behavior of this interface to another object. */
#define NS_FORWARD_LFIMAILBOX(_to) \
  NS_IMETHOD GetSelf(lfIErlPid * *aSelf) { return _to GetSelf(aSelf); } \
  NS_IMETHOD Send(lfIErlPid *toPid, lfIErlTerm *term) { return _to Send(toPid, term); } \
  NS_IMETHOD RegSend(const char *toName, lfIErlTerm *term) { return _to RegSend(toName, term); } \
  NS_IMETHOD RemoteRegSend(const char *toNode, const char *toName, lfIErlTerm *term) { return _to RemoteRegSend(toNode, toName, term); } \
  NS_IMETHOD SendRPC(const char *node, const char *mod, const char *fun, lfIErlList *args) { return _to SendRPC(node, mod, fun, args); } \
  NS_IMETHOD Receive(lfIErlTerm **_retval) { return _to Receive(_retval); } \
  NS_IMETHOD ReceiveWithTimeout(PRInt32 timeout, lfIErlTerm **_retval) { return _to ReceiveWithTimeout(timeout, _retval); } \
  NS_IMETHOD ReceivePattern(lfIErlTerm *pattern, lfIVariableBinding *binding, lfIErlTerm **_retval) { return _to ReceivePattern(pattern, binding, _retval); } \
  NS_IMETHOD ReceivePatternWithTimeout(lfIErlTerm *pattern, lfIVariableBinding *binding, PRInt32 timeout, lfIErlTerm **_retval) { return _to ReceivePatternWithTimeout(pattern, binding, timeout, _retval); } \
  NS_IMETHOD ReceiveRPC(lfIErlTerm **_retval) { return _to ReceiveRPC(_retval); } \
  NS_IMETHOD ReceiveRPCWithTimeout(PRInt32 timeout, lfIErlTerm **_retval) { return _to ReceiveRPCWithTimeout(timeout, _retval); } \
  NS_IMETHOD GetNative(epi::node::MailBox * *_retval) { return _to GetNative(_retval); } 

/* Use this macro to declare functions that forward the behavior of this interface to another object in a safe way. */
#define NS_FORWARD_SAFE_LFIMAILBOX(_to) \
  NS_IMETHOD GetSelf(lfIErlPid * *aSelf) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetSelf(aSelf); } \
  NS_IMETHOD Send(lfIErlPid *toPid, lfIErlTerm *term) { return !_to ? NS_ERROR_NULL_POINTER : _to->Send(toPid, term); } \
  NS_IMETHOD RegSend(const char *toName, lfIErlTerm *term) { return !_to ? NS_ERROR_NULL_POINTER : _to->RegSend(toName, term); } \
  NS_IMETHOD RemoteRegSend(const char *toNode, const char *toName, lfIErlTerm *term) { return !_to ? NS_ERROR_NULL_POINTER : _to->RemoteRegSend(toNode, toName, term); } \
  NS_IMETHOD SendRPC(const char *node, const char *mod, const char *fun, lfIErlList *args) { return !_to ? NS_ERROR_NULL_POINTER : _to->SendRPC(node, mod, fun, args); } \
  NS_IMETHOD Receive(lfIErlTerm **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->Receive(_retval); } \
  NS_IMETHOD ReceiveWithTimeout(PRInt32 timeout, lfIErlTerm **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->ReceiveWithTimeout(timeout, _retval); } \
  NS_IMETHOD ReceivePattern(lfIErlTerm *pattern, lfIVariableBinding *binding, lfIErlTerm **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->ReceivePattern(pattern, binding, _retval); } \
  NS_IMETHOD ReceivePatternWithTimeout(lfIErlTerm *pattern, lfIVariableBinding *binding, PRInt32 timeout, lfIErlTerm **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->ReceivePatternWithTimeout(pattern, binding, timeout, _retval); } \
  NS_IMETHOD ReceiveRPC(lfIErlTerm **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->ReceiveRPC(_retval); } \
  NS_IMETHOD ReceiveRPCWithTimeout(PRInt32 timeout, lfIErlTerm **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->ReceiveRPCWithTimeout(timeout, _retval); } \
  NS_IMETHOD GetNative(epi::node::MailBox * *_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetNative(_retval); } 

#if 0
/* Use the code below as a template for the implementation class for this interface. */

/* Header file */
class lfMailBox : public lfIMailBox
{
public:
  NS_DECL_ISUPPORTS
  NS_DECL_LFIMAILBOX

  lfMailBox();

private:
  ~lfMailBox();

protected:
  /* additional members */
};

/* Implementation file */
NS_IMPL_ISUPPORTS1(lfMailBox, lfIMailBox)

lfMailBox::lfMailBox()
{
  /* member initializers and constructor code */
}

lfMailBox::~lfMailBox()
{
  /* destructor code */
}

/* readonly attribute lfIErlPid self; */
NS_IMETHODIMP lfMailBox::GetSelf(lfIErlPid * *aSelf)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void send (in lfIErlPid toPid, in lfIErlTerm term); */
NS_IMETHODIMP lfMailBox::Send(lfIErlPid *toPid, lfIErlTerm *term)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void regSend (in string toName, in lfIErlTerm term); */
NS_IMETHODIMP lfMailBox::RegSend(const char *toName, lfIErlTerm *term)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void remoteRegSend (in string toNode, in string toName, in lfIErlTerm term); */
NS_IMETHODIMP lfMailBox::RemoteRegSend(const char *toNode, const char *toName, lfIErlTerm *term)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void sendRPC (in string node, in string mod, in string fun, in lfIErlList args); */
NS_IMETHODIMP lfMailBox::SendRPC(const char *node, const char *mod, const char *fun, lfIErlList *args)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlTerm receive (); */
NS_IMETHODIMP lfMailBox::Receive(lfIErlTerm **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlTerm receiveWithTimeout (in long timeout); */
NS_IMETHODIMP lfMailBox::ReceiveWithTimeout(PRInt32 timeout, lfIErlTerm **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlTerm receivePattern (in lfIErlTerm pattern, in lfIVariableBinding binding); */
NS_IMETHODIMP lfMailBox::ReceivePattern(lfIErlTerm *pattern, lfIVariableBinding *binding, lfIErlTerm **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlTerm receivePatternWithTimeout (in lfIErlTerm pattern, in lfIVariableBinding binding, in long timeout); */
NS_IMETHODIMP lfMailBox::ReceivePatternWithTimeout(lfIErlTerm *pattern, lfIVariableBinding *binding, PRInt32 timeout, lfIErlTerm **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlTerm receiveRPC (); */
NS_IMETHODIMP lfMailBox::ReceiveRPC(lfIErlTerm **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlTerm receiveRPCWithTimeout (in long timeout); */
NS_IMETHODIMP lfMailBox::ReceiveRPCWithTimeout(PRInt32 timeout, lfIErlTerm **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* [noscript] lfNativeMailBox getNative (); */
NS_IMETHODIMP lfMailBox::GetNative(epi::node::MailBox * *_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* End of implementation class template. */
#endif


#endif /* __gen_lfIMailBox_h__ */

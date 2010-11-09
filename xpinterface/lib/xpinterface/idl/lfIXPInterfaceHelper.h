/*
 * DO NOT EDIT.  THIS FILE IS GENERATED FROM lib/xpinterface/idl/lfIXPInterfaceHelper.idl
 */

#ifndef __gen_lfIXPInterfaceHelper_h__
#define __gen_lfIXPInterfaceHelper_h__


#ifndef __gen_nsISupports_h__
#include "nsISupports.h"
#endif

#ifndef __gen_lfIErlTerm_h__
#include "lfIErlTerm.h"
#endif

#ifndef __gen_lfIErlAtom_h__
#include "lfIErlAtom.h"
#endif

#ifndef __gen_lfIErlLong_h__
#include "lfIErlLong.h"
#endif

#ifndef __gen_lfIErlDouble_h__
#include "lfIErlDouble.h"
#endif

#ifndef __gen_lfIErlString_h__
#include "lfIErlString.h"
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

#ifndef __gen_lfIErlBinary_h__
#include "lfIErlBinary.h"
#endif

#ifndef __gen_lfIErlList_h__
#include "lfIErlList.h"
#endif

#ifndef __gen_lfIErlEmptyList_h__
#include "lfIErlEmptyList.h"
#endif

#ifndef __gen_lfIErlConsList_h__
#include "lfIErlConsList.h"
#endif

#ifndef __gen_lfIErlVariable_h__
#include "lfIErlVariable.h"
#endif

#ifndef __gen_lfIErlTuple_h__
#include "lfIErlTuple.h"
#endif

#ifndef __gen_lfINode_h__
#include "lfINode.h"
#endif

/* For IDL files that don't want to include root IDL files. */
#ifndef NS_NO_VTABLE
#define NS_NO_VTABLE
#endif

/* starting interface:    lfIXPInterfaceHelper */
#define LFIXPINTERFACEHELPER_IID_STR "7eac2af8-0efb-44f5-b86f-158b34e0f3f3"

#define LFIXPINTERFACEHELPER_IID \
  {0x7eac2af8, 0x0efb, 0x44f5, \
    { 0xb8, 0x6f, 0x15, 0x8b, 0x34, 0xe0, 0xf3, 0xf3 }}

class NS_NO_VTABLE lfIXPInterfaceHelper : public nsISupports {
 public: 

  NS_DEFINE_STATIC_IID_ACCESSOR(LFIXPINTERFACEHELPER_IID)

  /**
	 * Create a new auto node. 
	 * @param aNodeName Node name
	 * @param aCookie Cookie to use
	 */
  /* lfINode newNode (in string aNodeName, in string aCookie); */
  NS_IMETHOD NewNode(const char *aNodeName, const char *aCookie, lfINode **_retval) = 0;

  /**
     * Create a new Atom
     * @param atom The atom value
     */
  /* lfIErlAtom newAtom (in string atom); */
  NS_IMETHOD NewAtom(const char *atom, lfIErlAtom **_retval) = 0;

  /**
     * Create a new Long
     * @param value The Long value
     */
  /* lfIErlLong newLong (in long long value); */
  NS_IMETHOD NewLong(PRInt64 value, lfIErlLong **_retval) = 0;

  /**
     * Create a new Double
     * @param value The Double value
     */
  /* lfIErlDouble newDouble (in double value); */
  NS_IMETHOD NewDouble(double value, lfIErlDouble **_retval) = 0;

  /**
     * Create a new String
     * @param value The string value
     */
  /* lfIErlString newString (in string value); */
  NS_IMETHOD NewString(const char *value, lfIErlString **_retval) = 0;

  /**
     * Create a new Binary from an array
     * @param count size of the binary
     * @param data binary data
     */
  /* lfIErlBinary newBinary (in unsigned long count, [array, size_is (count)] in octet data); */
  NS_IMETHOD NewBinary(PRUint32 count, PRUint8 *data, lfIErlBinary **_retval) = 0;

  /**
     * Create an Erlang pid from its components.
	 * NOTE: Do not use this function, pids should not be created manually, 
	 * use the lfINode.createPid() instead.
     * @param node the nodename.
     * @param id an arbitrary number. Only the low order 15 bits will
     * be used.
     * @param serial another arbitrary number. Only the low order 13 bits
     * will be used.
     * @param creation yet another arbitrary number. Only the low order
     * 2 bits will be used.
     **/
  /* lfIErlPid newPid (in string node, in unsigned long id, in unsigned long serial, in unsigned long creation); */
  NS_IMETHOD NewPid(const char *node, PRUint32 id, PRUint32 serial, PRUint32 creation, lfIErlPid **_retval) = 0;

  /**
     * Create an old style Erlang ref from its components.
	 * NOTE: Do not use this function, refs should not be created manually, 
	 * use the lfINode.createRef() instead.
     * @param node the nodename.
     * @param id an arbitrary number. Only the low order 15 bits will
     * be used.
     * @param creation yet another arbitrary number. Only the low order
     * 2 bits will be used.
     **/
  /* lfIErlRef newOldStyleRef (in string node, in unsigned long id, in unsigned long creation); */
  NS_IMETHOD NewOldStyleRef(const char *node, PRUint32 id, PRUint32 creation, lfIErlRef **_retval) = 0;

  /**
     * Create an new style Erlang ref from its components.
	 * NOTE: Do not use this function, refs should not be created manually, 
	 * use the lfINode.createRef() instead.
     * @param node the nodename.
     * @param id0 an arbitrary number. Only the low order 18 bits will
     * be used.
     * @param id1 an arbitrary number.
     * @param id2 an arbitrary number.
     * @param creation yet another arbitrary number. Only the low order
     * 2 bits will be used.
     **/
  /* lfIErlRef newNewStyleRef (in string node, in unsigned long id0, in unsigned long id1, in unsigned long id2, in unsigned long creation); */
  NS_IMETHOD NewNewStyleRef(const char *node, PRUint32 id0, PRUint32 id1, PRUint32 id2, PRUint32 creation, lfIErlRef **_retval) = 0;

  /**
     * Create an Erlang port from its components.
	 * NOTE: Do not use this function, ports should not be created manually, 
	 * use the lfINode.createPort() instead.
     * @param node the nodename.
     * @param id an arbitrary number. Only the low order 18 bits will
     * be used.
     * @param creation yet another arbitrary number. Only the low order
     * 2 bits will be used.
     **/
  /* lfIErlPort newPort (in string node, in unsigned long id, in unsigned long creation); */
  NS_IMETHOD NewPort(const char *node, PRUint32 id, PRUint32 creation, lfIErlPort **_retval) = 0;

  /**
     * Create a new Tuple. The created ErlTuple have unitialized
     * elements, and they must be initialized with lfIErlTerm.initElement().
     * @param arity Size of the tuple.
     */
  /* lfIErlTuple newTuple (in unsigned long arity); */
  NS_IMETHOD NewTuple(PRUint32 arity, lfIErlTuple **_retval) = 0;

  /**
     * Create a new Tuple from an array of terms
     * @param arity Size of the tuple.
     * @param terms terms to use
     */
  /* lfIErlTuple newTupleFromArray (in unsigned long arity, [array, size_is (arity)] in lfIErlTerm terms); */
  NS_IMETHOD NewTupleFromArray(PRUint32 arity, lfIErlTerm **terms, lfIErlTuple **_retval) = 0;

  /**
     * Create a new EmptyList
     */
  /* lfIErlEmptyList newEmptyList (); */
  NS_IMETHOD NewEmptyList(lfIErlEmptyList **_retval) = 0;

  /**
     * Create a new ConsList without elements to be initialized
     */
  /* lfIErlConsList newConsList (); */
  NS_IMETHOD NewConsList(lfIErlConsList **_retval) = 0;

  /**
     * Create a proper list from an array of elements
     * @param arity length of the list.
     * @param terms terms to use
     */
  /* lfIErlList newList (in unsigned long arity, [array, size_is (arity)] in lfIErlTerm terms); */
  NS_IMETHOD NewList(PRUint32 arity, lfIErlTerm **terms, lfIErlList **_retval) = 0;

  /**
     * Create a variable
     * @param name Variable name
     */
  /* lfIErlVariable newVariable (in string name); */
  NS_IMETHOD NewVariable(const char *name, lfIErlVariable **_retval) = 0;

  /**
     * lfIErlTerm native factory method. Constructs a valid lfIErlTerm from
     * an ErlTerm. It checks the type of the ErlTerm and calls
     * the appropiate constructor.
     */
  /* [noscript] lfIErlTerm createErlTerm (in lfNativeErlTerm aETerm); */
  NS_IMETHOD CreateErlTerm(epi::type::ErlTerm * aETerm, lfIErlTerm **_retval) = 0;

};

/* Use this macro when declaring classes that implement this interface. */
#define NS_DECL_LFIXPINTERFACEHELPER \
  NS_IMETHOD NewNode(const char *aNodeName, const char *aCookie, lfINode **_retval); \
  NS_IMETHOD NewAtom(const char *atom, lfIErlAtom **_retval); \
  NS_IMETHOD NewLong(PRInt64 value, lfIErlLong **_retval); \
  NS_IMETHOD NewDouble(double value, lfIErlDouble **_retval); \
  NS_IMETHOD NewString(const char *value, lfIErlString **_retval); \
  NS_IMETHOD NewBinary(PRUint32 count, PRUint8 *data, lfIErlBinary **_retval); \
  NS_IMETHOD NewPid(const char *node, PRUint32 id, PRUint32 serial, PRUint32 creation, lfIErlPid **_retval); \
  NS_IMETHOD NewOldStyleRef(const char *node, PRUint32 id, PRUint32 creation, lfIErlRef **_retval); \
  NS_IMETHOD NewNewStyleRef(const char *node, PRUint32 id0, PRUint32 id1, PRUint32 id2, PRUint32 creation, lfIErlRef **_retval); \
  NS_IMETHOD NewPort(const char *node, PRUint32 id, PRUint32 creation, lfIErlPort **_retval); \
  NS_IMETHOD NewTuple(PRUint32 arity, lfIErlTuple **_retval); \
  NS_IMETHOD NewTupleFromArray(PRUint32 arity, lfIErlTerm **terms, lfIErlTuple **_retval); \
  NS_IMETHOD NewEmptyList(lfIErlEmptyList **_retval); \
  NS_IMETHOD NewConsList(lfIErlConsList **_retval); \
  NS_IMETHOD NewList(PRUint32 arity, lfIErlTerm **terms, lfIErlList **_retval); \
  NS_IMETHOD NewVariable(const char *name, lfIErlVariable **_retval); \
  NS_IMETHOD CreateErlTerm(epi::type::ErlTerm * aETerm, lfIErlTerm **_retval); 

/* Use this macro to declare functions that forward the behavior of this interface to another object. */
#define NS_FORWARD_LFIXPINTERFACEHELPER(_to) \
  NS_IMETHOD NewNode(const char *aNodeName, const char *aCookie, lfINode **_retval) { return _to NewNode(aNodeName, aCookie, _retval); } \
  NS_IMETHOD NewAtom(const char *atom, lfIErlAtom **_retval) { return _to NewAtom(atom, _retval); } \
  NS_IMETHOD NewLong(PRInt64 value, lfIErlLong **_retval) { return _to NewLong(value, _retval); } \
  NS_IMETHOD NewDouble(double value, lfIErlDouble **_retval) { return _to NewDouble(value, _retval); } \
  NS_IMETHOD NewString(const char *value, lfIErlString **_retval) { return _to NewString(value, _retval); } \
  NS_IMETHOD NewBinary(PRUint32 count, PRUint8 *data, lfIErlBinary **_retval) { return _to NewBinary(count, data, _retval); } \
  NS_IMETHOD NewPid(const char *node, PRUint32 id, PRUint32 serial, PRUint32 creation, lfIErlPid **_retval) { return _to NewPid(node, id, serial, creation, _retval); } \
  NS_IMETHOD NewOldStyleRef(const char *node, PRUint32 id, PRUint32 creation, lfIErlRef **_retval) { return _to NewOldStyleRef(node, id, creation, _retval); } \
  NS_IMETHOD NewNewStyleRef(const char *node, PRUint32 id0, PRUint32 id1, PRUint32 id2, PRUint32 creation, lfIErlRef **_retval) { return _to NewNewStyleRef(node, id0, id1, id2, creation, _retval); } \
  NS_IMETHOD NewPort(const char *node, PRUint32 id, PRUint32 creation, lfIErlPort **_retval) { return _to NewPort(node, id, creation, _retval); } \
  NS_IMETHOD NewTuple(PRUint32 arity, lfIErlTuple **_retval) { return _to NewTuple(arity, _retval); } \
  NS_IMETHOD NewTupleFromArray(PRUint32 arity, lfIErlTerm **terms, lfIErlTuple **_retval) { return _to NewTupleFromArray(arity, terms, _retval); } \
  NS_IMETHOD NewEmptyList(lfIErlEmptyList **_retval) { return _to NewEmptyList(_retval); } \
  NS_IMETHOD NewConsList(lfIErlConsList **_retval) { return _to NewConsList(_retval); } \
  NS_IMETHOD NewList(PRUint32 arity, lfIErlTerm **terms, lfIErlList **_retval) { return _to NewList(arity, terms, _retval); } \
  NS_IMETHOD NewVariable(const char *name, lfIErlVariable **_retval) { return _to NewVariable(name, _retval); } \
  NS_IMETHOD CreateErlTerm(epi::type::ErlTerm * aETerm, lfIErlTerm **_retval) { return _to CreateErlTerm(aETerm, _retval); } 

/* Use this macro to declare functions that forward the behavior of this interface to another object in a safe way. */
#define NS_FORWARD_SAFE_LFIXPINTERFACEHELPER(_to) \
  NS_IMETHOD NewNode(const char *aNodeName, const char *aCookie, lfINode **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->NewNode(aNodeName, aCookie, _retval); } \
  NS_IMETHOD NewAtom(const char *atom, lfIErlAtom **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->NewAtom(atom, _retval); } \
  NS_IMETHOD NewLong(PRInt64 value, lfIErlLong **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->NewLong(value, _retval); } \
  NS_IMETHOD NewDouble(double value, lfIErlDouble **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->NewDouble(value, _retval); } \
  NS_IMETHOD NewString(const char *value, lfIErlString **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->NewString(value, _retval); } \
  NS_IMETHOD NewBinary(PRUint32 count, PRUint8 *data, lfIErlBinary **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->NewBinary(count, data, _retval); } \
  NS_IMETHOD NewPid(const char *node, PRUint32 id, PRUint32 serial, PRUint32 creation, lfIErlPid **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->NewPid(node, id, serial, creation, _retval); } \
  NS_IMETHOD NewOldStyleRef(const char *node, PRUint32 id, PRUint32 creation, lfIErlRef **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->NewOldStyleRef(node, id, creation, _retval); } \
  NS_IMETHOD NewNewStyleRef(const char *node, PRUint32 id0, PRUint32 id1, PRUint32 id2, PRUint32 creation, lfIErlRef **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->NewNewStyleRef(node, id0, id1, id2, creation, _retval); } \
  NS_IMETHOD NewPort(const char *node, PRUint32 id, PRUint32 creation, lfIErlPort **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->NewPort(node, id, creation, _retval); } \
  NS_IMETHOD NewTuple(PRUint32 arity, lfIErlTuple **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->NewTuple(arity, _retval); } \
  NS_IMETHOD NewTupleFromArray(PRUint32 arity, lfIErlTerm **terms, lfIErlTuple **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->NewTupleFromArray(arity, terms, _retval); } \
  NS_IMETHOD NewEmptyList(lfIErlEmptyList **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->NewEmptyList(_retval); } \
  NS_IMETHOD NewConsList(lfIErlConsList **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->NewConsList(_retval); } \
  NS_IMETHOD NewList(PRUint32 arity, lfIErlTerm **terms, lfIErlList **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->NewList(arity, terms, _retval); } \
  NS_IMETHOD NewVariable(const char *name, lfIErlVariable **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->NewVariable(name, _retval); } \
  NS_IMETHOD CreateErlTerm(epi::type::ErlTerm * aETerm, lfIErlTerm **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->CreateErlTerm(aETerm, _retval); } 

#if 0
/* Use the code below as a template for the implementation class for this interface. */

/* Header file */
class lfXPInterfaceHelper : public lfIXPInterfaceHelper
{
public:
  NS_DECL_ISUPPORTS
  NS_DECL_LFIXPINTERFACEHELPER

  lfXPInterfaceHelper();

private:
  ~lfXPInterfaceHelper();

protected:
  /* additional members */
};

/* Implementation file */
NS_IMPL_ISUPPORTS1(lfXPInterfaceHelper, lfIXPInterfaceHelper)

lfXPInterfaceHelper::lfXPInterfaceHelper()
{
  /* member initializers and constructor code */
}

lfXPInterfaceHelper::~lfXPInterfaceHelper()
{
  /* destructor code */
}

/* lfINode newNode (in string aNodeName, in string aCookie); */
NS_IMETHODIMP lfXPInterfaceHelper::NewNode(const char *aNodeName, const char *aCookie, lfINode **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlAtom newAtom (in string atom); */
NS_IMETHODIMP lfXPInterfaceHelper::NewAtom(const char *atom, lfIErlAtom **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlLong newLong (in long long value); */
NS_IMETHODIMP lfXPInterfaceHelper::NewLong(PRInt64 value, lfIErlLong **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlDouble newDouble (in double value); */
NS_IMETHODIMP lfXPInterfaceHelper::NewDouble(double value, lfIErlDouble **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlString newString (in string value); */
NS_IMETHODIMP lfXPInterfaceHelper::NewString(const char *value, lfIErlString **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlBinary newBinary (in unsigned long count, [array, size_is (count)] in octet data); */
NS_IMETHODIMP lfXPInterfaceHelper::NewBinary(PRUint32 count, PRUint8 *data, lfIErlBinary **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlPid newPid (in string node, in unsigned long id, in unsigned long serial, in unsigned long creation); */
NS_IMETHODIMP lfXPInterfaceHelper::NewPid(const char *node, PRUint32 id, PRUint32 serial, PRUint32 creation, lfIErlPid **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlRef newOldStyleRef (in string node, in unsigned long id, in unsigned long creation); */
NS_IMETHODIMP lfXPInterfaceHelper::NewOldStyleRef(const char *node, PRUint32 id, PRUint32 creation, lfIErlRef **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlRef newNewStyleRef (in string node, in unsigned long id0, in unsigned long id1, in unsigned long id2, in unsigned long creation); */
NS_IMETHODIMP lfXPInterfaceHelper::NewNewStyleRef(const char *node, PRUint32 id0, PRUint32 id1, PRUint32 id2, PRUint32 creation, lfIErlRef **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlPort newPort (in string node, in unsigned long id, in unsigned long creation); */
NS_IMETHODIMP lfXPInterfaceHelper::NewPort(const char *node, PRUint32 id, PRUint32 creation, lfIErlPort **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlTuple newTuple (in unsigned long arity); */
NS_IMETHODIMP lfXPInterfaceHelper::NewTuple(PRUint32 arity, lfIErlTuple **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlTuple newTupleFromArray (in unsigned long arity, [array, size_is (arity)] in lfIErlTerm terms); */
NS_IMETHODIMP lfXPInterfaceHelper::NewTupleFromArray(PRUint32 arity, lfIErlTerm **terms, lfIErlTuple **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlEmptyList newEmptyList (); */
NS_IMETHODIMP lfXPInterfaceHelper::NewEmptyList(lfIErlEmptyList **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlConsList newConsList (); */
NS_IMETHODIMP lfXPInterfaceHelper::NewConsList(lfIErlConsList **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlList newList (in unsigned long arity, [array, size_is (arity)] in lfIErlTerm terms); */
NS_IMETHODIMP lfXPInterfaceHelper::NewList(PRUint32 arity, lfIErlTerm **terms, lfIErlList **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* lfIErlVariable newVariable (in string name); */
NS_IMETHODIMP lfXPInterfaceHelper::NewVariable(const char *name, lfIErlVariable **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* [noscript] lfIErlTerm createErlTerm (in lfNativeErlTerm aETerm); */
NS_IMETHODIMP lfXPInterfaceHelper::CreateErlTerm(epi::type::ErlTerm * aETerm, lfIErlTerm **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* End of implementation class template. */
#endif


#endif /* __gen_lfIXPInterfaceHelper_h__ */

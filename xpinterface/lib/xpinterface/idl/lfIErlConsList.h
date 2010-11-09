/*
 * DO NOT EDIT.  THIS FILE IS GENERATED FROM lib/xpinterface/idl/lfIErlConsList.idl
 */

#ifndef __gen_lfIErlConsList_h__
#define __gen_lfIErlConsList_h__


#ifndef __gen_lfIErlList_h__
#include "lfIErlList.h"
#endif

/* For IDL files that don't want to include root IDL files. */
#ifndef NS_NO_VTABLE
#define NS_NO_VTABLE
#endif

/* starting interface:    lfIErlConsList */
#define LFIERLCONSLIST_IID_STR "9508b8cc-09fa-4c6b-a929-2d811b712f49"

#define LFIERLCONSLIST_IID \
  {0x9508b8cc, 0x09fa, 0x4c6b, \
    { 0xa9, 0x29, 0x2d, 0x81, 0x1b, 0x71, 0x2f, 0x49 }}

class NS_NO_VTABLE lfIErlConsList : public lfIErlList {
 public: 

  NS_DEFINE_STATIC_IID_ACCESSOR(LFIERLCONSLIST_IID)

  /**
     * Add elements to the list, before the tail.
     * @param elem a pointer to the element to use as element.
     */
  /* void addElement (in lfIErlTerm elem); */
  NS_IMETHOD AddElement(lfIErlTerm *elem) = 0;

  /**
     * Set the last tail of the list. Normaly, the last tail is
     * an empty list, becoming this list a proper list.
     * @param elem a pointer to the element to use as element.
     */
  /* void close (in lfIErlTerm elem); */
  NS_IMETHOD Close(lfIErlTerm *elem) = 0;

};

/* Use this macro when declaring classes that implement this interface. */
#define NS_DECL_LFIERLCONSLIST \
  NS_IMETHOD AddElement(lfIErlTerm *elem); \
  NS_IMETHOD Close(lfIErlTerm *elem); 

/* Use this macro to declare functions that forward the behavior of this interface to another object. */
#define NS_FORWARD_LFIERLCONSLIST(_to) \
  NS_IMETHOD AddElement(lfIErlTerm *elem) { return _to AddElement(elem); } \
  NS_IMETHOD Close(lfIErlTerm *elem) { return _to Close(elem); } 

/* Use this macro to declare functions that forward the behavior of this interface to another object in a safe way. */
#define NS_FORWARD_SAFE_LFIERLCONSLIST(_to) \
  NS_IMETHOD AddElement(lfIErlTerm *elem) { return !_to ? NS_ERROR_NULL_POINTER : _to->AddElement(elem); } \
  NS_IMETHOD Close(lfIErlTerm *elem) { return !_to ? NS_ERROR_NULL_POINTER : _to->Close(elem); } 

#if 0
/* Use the code below as a template for the implementation class for this interface. */

/* Header file */
class lfErlConsList : public lfIErlConsList
{
public:
  NS_DECL_ISUPPORTS
  NS_DECL_LFIERLCONSLIST

  lfErlConsList();

private:
  ~lfErlConsList();

protected:
  /* additional members */
};

/* Implementation file */
NS_IMPL_ISUPPORTS1(lfErlConsList, lfIErlConsList)

lfErlConsList::lfErlConsList()
{
  /* member initializers and constructor code */
}

lfErlConsList::~lfErlConsList()
{
  /* destructor code */
}

/* void addElement (in lfIErlTerm elem); */
NS_IMETHODIMP lfErlConsList::AddElement(lfIErlTerm *elem)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void close (in lfIErlTerm elem); */
NS_IMETHODIMP lfErlConsList::Close(lfIErlTerm *elem)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* End of implementation class template. */
#endif


#endif /* __gen_lfIErlConsList_h__ */

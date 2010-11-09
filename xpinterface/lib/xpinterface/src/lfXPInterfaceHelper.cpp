/*
***** BEGIN LICENSE BLOCK *****

This file is part of the XPInterface Component.

Copyright (C) 2005 Hector Rivas Gandara <keymon@gmail.com>

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

***** END LICENSE BLOCK *****
*/

#include "lfXPInterfaceHelper.hpp"
#include "lfErlTerm.hpp"
#include "lfErlAtom.hpp"
#include "lfErlBinary.hpp"
#include "lfErlDouble.hpp"
#include "lfErlLong.hpp"
#include "lfErlString.hpp"
#include "lfErlPort.hpp"
#include "lfErlRef.hpp"
#include "lfErlPid.hpp"
#include "lfErlConsList.hpp"
#include "lfErlEmptyList.hpp"
#include "lfErlTuple.hpp"
#include "lfErlVariable.hpp"

#include "lfNode.hpp"

#include "EpiException.hpp"
using namespace epi::error;

NS_IMPL_ISUPPORTS1(lfXPInterfaceHelper, lfIXPInterfaceHelper);

lfXPInterfaceHelper::lfXPInterfaceHelper()
{
}

lfXPInterfaceHelper::~lfXPInterfaceHelper()
{
}

/* lfISelfNode newSelfNode (in string aNodeName, in string aCookie, in long aPort); */
/* lfINode newNode (in string aNodeName, in string aCookie, in long aPort); */
NS_IMETHODIMP lfXPInterfaceHelper::NewNode(const char *aNodeName, const char *aCookie, lfINode **_retval)
{
    try {
        NS_ADDREF(*_retval = new lfNode(aNodeName, aCookie));
        return NS_OK;
    } catch (EpiException &e) {
        return NS_ERROR_INVALID_ARG;
    }
}

/* lfIErlAtom newAtom (in string atom); */
NS_IMETHODIMP lfXPInterfaceHelper::NewAtom(const char *atom, lfIErlAtom **_retval)
{
    try {
        NS_ADDREF(*_retval = new lfErlAtom(atom));
        return NS_OK;
    } catch (EpiBadArgument &e) {
        return NS_ERROR_INVALID_ARG;
    }
}

/* lfIErlLong newLong (in long long value); */
NS_IMETHODIMP lfXPInterfaceHelper::NewLong(PRInt64 value, lfIErlLong **_retval)
{
    NS_ADDREF(*_retval = new lfErlLong(value));
    return NS_OK;
}

/* lfIErlDouble newDouble (in double value); */
NS_IMETHODIMP lfXPInterfaceHelper::NewDouble(double value, lfIErlDouble **_retval)
{
    NS_ADDREF(*_retval = new lfErlDouble(value));
    return NS_OK;
}

/* lfIErlString newString (in string value); */
NS_IMETHODIMP lfXPInterfaceHelper::NewString(const char *value, lfIErlString **_retval)
{
    NS_ADDREF(*_retval = new lfErlString(value));
    return NS_OK;
}

/* lfIErlBinary newBinary (in unsigned long count, [array, size_is (count)] in octet data); */
NS_IMETHODIMP lfXPInterfaceHelper::NewBinary(PRUint32 count, PRUint8 *data, lfIErlBinary **_retval)
{
    NS_ADDREF(*_retval = new lfErlBinary(data, count));
    return NS_OK;
}

/* lfIErlPid newPid (in string node, in long id, in long serial, in long creation); */
NS_IMETHODIMP lfXPInterfaceHelper::NewPid(const char *node, PRUint32 id, PRUint32 serial, PRUint32 creation, lfIErlPid **_retval)
{
    NS_ADDREF(*_retval = new lfErlPid(node, id, serial, creation));
    return NS_OK;
}

/* lfIErlRef newOldStyleRef (in string node, in long id, in long creation); */
NS_IMETHODIMP lfXPInterfaceHelper::NewOldStyleRef(const char *node, PRUint32 id, PRUint32 creation, lfIErlRef **_retval)
{
    PRUint32 ids[3];
    ids[0]=id;
    NS_ADDREF(*_retval = new lfErlRef(node, ids, creation, false));
    return NS_OK;
}

/* lfIErlRef newNewStyleRef (in string node, in long id0, in long id1, in long id2, in long creation); */
NS_IMETHODIMP lfXPInterfaceHelper::NewNewStyleRef(const char *node, PRUint32 id0, PRUint32 id1, PRUint32 id2, PRUint32 creation, lfIErlRef **_retval)
{
    PRUint32 ids[3];
    ids[0]=id0;
    ids[1]=id1;
    ids[2]=id2;
    NS_ADDREF(*_retval = new lfErlRef(node, ids, creation, true));
    return NS_OK;
}

/* lfIErlPort newPort (in string node, in long id, in long creation); */
NS_IMETHODIMP lfXPInterfaceHelper::NewPort(const char *node, PRUint32 id, PRUint32 creation, lfIErlPort **_retval)
{
    NS_ADDREF(*_retval = new lfErlPort(node, id, creation));
    return NS_OK;
}

/* lfIErlTuple newTuple (in unsigned long arity); */
NS_IMETHODIMP lfXPInterfaceHelper::NewTuple(PRUint32 arity, lfIErlTuple **_retval)
{
    NS_ADDREF(*_retval = new lfErlTuple(arity));
    return NS_OK;
}

/* lfIErlTuple newTupleFromArray (in unsigned long arity, [array, size_is (arity)] in lfIErlTerm terms); */
NS_IMETHODIMP lfXPInterfaceHelper::NewTupleFromArray(PRUint32 arity, lfIErlTerm **terms, lfIErlTuple **_retval)
{
    try {
        NS_ADDREF(*_retval = new lfErlTuple(arity, terms));
        return NS_OK;
    } catch (EpiBadArgument *e) {
        delete e;
        return NS_ERROR_INVALID_ARG;
    }
}

/* lfIErlEmptyList newEmptyList (); */
NS_IMETHODIMP lfXPInterfaceHelper::NewEmptyList(lfIErlEmptyList **_retval)
{
    NS_ADDREF(*_retval = new lfErlEmptyList());
    return NS_OK;
}

/* lfIErlConsList newConsList (); */
NS_IMETHODIMP lfXPInterfaceHelper::NewConsList(lfIErlConsList **_retval)
{
    NS_ADDREF(*_retval = new lfErlConsList());
    return NS_OK;
}

/* lfIErlList newList (in unsigned long arity, [array, size_is (arity)] in lfIErlTerm terms); */
NS_IMETHODIMP lfXPInterfaceHelper::NewList(PRUint32 arity, lfIErlTerm **terms, lfIErlList **_retval)
{
   if (arity == 0) {
        NS_ADDREF(*_retval = new lfErlEmptyList());
    } else {
        try {
            NS_ADDREF(*_retval = new lfErlConsList(arity, terms));
        } catch (EpiBadArgument *e) {
            return NS_ERROR_INVALID_ARG;
        }
    }
    return NS_OK;
}

/* lfIErlConsList newConsList (); */
NS_IMETHODIMP lfXPInterfaceHelper::NewVariable(const char *aVariableName, lfIErlVariable **_retval)
{
    NS_ADDREF(*_retval = new lfErlVariable(aVariableName));
    return NS_OK;
}


/* [noscript] lfIErlTerm createErlTerm (in lfNativeErlTerm aETerm); */
NS_IMETHODIMP lfXPInterfaceHelper::CreateErlTerm(epi::type::ErlTerm * aErlTerm, lfIErlTerm **_retval)
{
    NS_ADDREF(*_retval = fromErlTerm(aErlTerm));
    return NS_OK;
}

lfIErlTerm *lfXPInterfaceHelper::fromErlTerm(epi::type::ErlTerm * aErlTerm) {
   if (aErlTerm == 0) {
        return 0;
    }
    switch(aErlTerm->termType()) {
        case ERL_LONG:
            return new lfErlLong((ErlLong*) aErlTerm);
            break;
        case ERL_DOUBLE:
            return new lfErlDouble((ErlDouble*) aErlTerm);
            break;
        case ERL_ATOM:
            return new lfErlAtom((ErlAtom*)aErlTerm);
            break;
        case ERL_STRING:
            return new lfErlString((ErlString*)aErlTerm);
            break;
        case ERL_REF:
            return new lfErlRef((ErlRef*)aErlTerm);
            break;
        case ERL_PORT:
            return new lfErlPort((ErlPort*)aErlTerm);
            break;
        case ERL_PID:
            return new lfErlPid((ErlPid*)aErlTerm);
            break;
        case ERL_BINARY:
            return new lfErlBinary((ErlBinary*)aErlTerm);
            break;
        case ERL_TUPLE:
            return new lfErlTuple((ErlTuple*)aErlTerm);
            break;
        case ERL_LIST:
        case ERL_EMPTY_LIST:
            return new lfErlEmptyList();
            break;
        case ERL_CONS_LIST:
            return new lfErlConsList((ErlConsList*)aErlTerm);
            break;
        case ERL_VARIABLE:
            return new lfErlVariable((ErlVariable*)aErlTerm);
            break;
    }
    return 0;
}


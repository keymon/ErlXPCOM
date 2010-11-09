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

#include "lfErlTerm.hpp"
#include "lfXPInterfaceHelper.hpp"

#include "xpcom_utils.hpp"

using namespace epi::error;

void lfErlTerm_Impl::init(ErlTerm *term) {
    mTerm = term;
}

/* string toString (); */
NS_IMETHODIMP lfErlTerm_Impl::ToString(char **_retval)
{
    const char *cresult = mTerm->toString().c_str();
    XPCOM_STRING_RETURN(cresult, _retval);
}

/* string toStringWithBinding (in lfIVariableBinding binding); */
NS_IMETHODIMP lfErlTerm_Impl::ToStringWithBinding(lfIVariableBinding *binding, char **_retval)
{
    if (binding == 0) {
        return NS_ERROR_NULL_POINTER;
    }
    VariableBinding *_binding;
    binding->GetNative(&_binding);
    const char *cresult = mTerm->toString(_binding).c_str();
    XPCOM_STRING_RETURN(cresult, _retval);
}

/* boolean isValid (); */
NS_IMETHODIMP lfErlTerm_Impl::IsValid(PRBool *_retval)
{
    if (mTerm->isValid()) {
        *_retval = 1;
    } else {
        *_retval = 0;
    }
    return NS_OK;
}

/* boolean equals (in lfIErlTerm term); */
NS_IMETHODIMP lfErlTerm_Impl::Equals(lfIErlTerm *term, PRBool *_retval)
{
    if (term == 0) {
        return NS_ERROR_NULL_POINTER;
    }
    ErlTerm *erlterm;
    term->GetErlTerm(&erlterm);
    if (mTerm->equals(*erlterm)) {
        *_retval = 1;
    } else {
        *_retval = 0;
    }
    return NS_OK;
}

/* boolean match (in lfIErlTerm pattern); */
NS_IMETHODIMP lfErlTerm_Impl::Match(lfIErlTerm *pattern, PRBool *_retval)
{
    if (pattern == 0) {
        return NS_ERROR_NULL_POINTER;
    }
    ErlTerm *erlterm;
    pattern->GetErlTerm(&erlterm);
    try {
        if (mTerm->match(erlterm)) {
            *_retval = 1;
        } else {
            *_retval = 0;
        }
        return NS_OK;
    } catch (EpiVariableUnbound &e) {
        return NS_ERROR_INVALID_ARG;
    }
}

/* boolean matchWithBinding (in lfIErlTerm pattern, in lfIVariableBinding binding); */
NS_IMETHODIMP lfErlTerm_Impl::MatchWithBinding(lfIErlTerm *pattern, lfIVariableBinding *binding, PRBool *_retval)
{
    if (pattern == 0 || binding == 0) {
        return NS_ERROR_NULL_POINTER;
    }
    ErlTerm *erlterm;
    pattern->GetErlTerm(&erlterm);
    VariableBinding *nBinding;
    binding->GetNative(&nBinding);

    try {
        if (mTerm->match(erlterm, nBinding)) {
            *_retval = 1;
        } else {
            *_retval = 0;
        }
        return NS_OK;
    } catch (EpiException &e) {
        return NS_ERROR_INVALID_ARG;
    }
}

/* lfIErlTerm subst (in lfIVariableBinding binding); */
NS_IMETHODIMP lfErlTerm_Impl::Subst(lfIVariableBinding *binding, lfIErlTerm **_retval)
{
    if (binding == 0) {
        return NS_ERROR_NULL_POINTER;
    }
    VariableBinding *nBinding;
    binding->GetNative(&nBinding);
    try {
        ErlTerm *newErlTerm = mTerm->subst(nBinding);
        NS_ADDREF(*_retval = lfXPInterfaceHelper::fromErlTerm(newErlTerm));
        return NS_OK;
    } catch (EpiException &e) {
        return NS_ERROR_INVALID_ARG;
    }

}

/* [noscript] lfNativeErlTerm getErlTerm (); */
NS_IMETHODIMP lfErlTerm_Impl::GetErlTerm(epi::type::ErlTerm * *_retval)
{
    *_retval = mTerm;
    return NS_OK;
}


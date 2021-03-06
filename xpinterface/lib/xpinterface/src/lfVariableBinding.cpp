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

/*
***** BEGIN LICENSE BLOCK *****

This file is part of the EPI (Erlang Plus Interface) Library.

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
#include "lfVariableBinding.hpp"

NS_IMPL_ISUPPORTS1(lfVariableBinding, lfIVariableBinding);

lfVariableBinding::lfVariableBinding(): mBinding()
{
  /* member initializers and constructor code */
}

lfVariableBinding::~lfVariableBinding()
{
  /* destructor code */
}

/* lfIErlTerm search (in string name); */
NS_IMETHODIMP lfVariableBinding::Search(const char *name, lfIErlTerm **_retval)
{
    ErlTerm *term = mBinding.search(name);
    if (term) {
        NS_ADDREF(*_retval = lfXPInterfaceHelper::fromErlTerm(term));
    } else {
        *_retval = 0;
    }
    return NS_OK;
}

/* void bind (in string name, in lfIErlTerm term); */
NS_IMETHODIMP lfVariableBinding::Bind(const char *name, lfIErlTerm *term)
{
    if (term == 0) {
        return NS_ERROR_NULL_POINTER;
    } else {
        ErlTerm *_term;
        term->GetErlTerm(&_term);
        mBinding.bind(name, _term);
    }
    return NS_OK;
}

/* [noscript] lfNativeVariableBinding getNative (); */
NS_IMETHODIMP lfVariableBinding::GetNative(epi::type::VariableBinding * *_retval)
{
    *_retval = &mBinding;
    return NS_OK;
}

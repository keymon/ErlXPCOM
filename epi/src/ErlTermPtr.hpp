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

#ifndef __ERLTERMPTR_HPP
#define __ERLTERMPTR_HPP

#include "ErlTerm.hpp"

namespace epi {
namespace type {

/**
 * Smart Pointer to manage ErlTerm instances.
 * Automatic manages reference counting methods.
 * @param T type to use, default: ErlTerm.
*/
template <class T = epi::type::ErlTerm>
class ErlTermPtr {
    typedef T* erlterm_ptr;

public:

    /**
     * Create a new ErlTermPtr from ErlTerm pointer,
     * increasing reference counter
     */
    inline ErlTermPtr<T>(erlterm_ptr term = 0): mErlTerm(term) {
        if (mErlTerm) {
            mErlTerm->addRef();
        }
    }

    /**
     * Copy constructor.
     * initialize a new ErlTermPtr with other one
     * increasing the Ref Counter
     */
    inline ErlTermPtr(const ErlTermPtr& rhs):
           mErlTerm(rhs.addRef()) {
        Dout(dc::erlang_memory, "["<<mErlTerm<<"]"<<"  \\- ErlTermPtr copy");
    }
    template <class U>
    inline ErlTermPtr<T>(const ErlTermPtr<U>& rhs):
            mErlTerm(rhs.addRef())
    {
        Dout(dc::erlang_memory, "["<<mErlTerm<<"]"<<"  \\- ErlTermPtr copy");
    }

    /** Destructor. Decreases counter */
    ~ErlTermPtr() {
        if (mErlTerm) {
            mErlTerm->release();
        }
    }

    /** Operator '*', returns a reference to the underlying object */
    inline T& operator*() const {
        return *mErlTerm;
    }

    /** Operator '->', returns a pointer to the underlying object  */
    inline erlterm_ptr operator->() const {
        return mErlTerm;
    }

    /** Returns a pointer to the underlying object  */
    inline erlterm_ptr get() const {
        return mErlTerm;
    }

    /**
     * Decreases the reference counter of underlying pointer
     * WITHOUT destroying it and resets the internal ErlTerm pointer to 0.
     * Use this method with care, just to return zero referenced ErlTerms
     * @returns underlying pointer to ErlTerm with counter decreased.
     * The reference counter can be <= 0!
     */
    inline erlterm_ptr drop() {
        if (mErlTerm) {
            mErlTerm->drop();
        }
        erlterm_ptr ret = mErlTerm;
        mErlTerm = 0;
        return ret;
    }

    /**
     * Sets the underlying ErlTerm to p, increasing reference counter
     * of the new pointer and decreasing the counter of the old one
     * @param newTerm new ErlTerm pointer to manage
     */
    inline void reset(erlterm_ptr newTerm = 0) {
        if (mErlTerm != newTerm) {
            if (mErlTerm) {
                mErlTerm->release();
            }

            mErlTerm = newTerm;
            if (mErlTerm) {
                mErlTerm->addRef();
            }
        }
    }

    /**
     * Operator '=', copies the given argument to *this, increasing
     * counter of given ErlTerm and releasing of old one
     */
    inline ErlTermPtr& operator=(ErlTermPtr &rhs) {
        reset(rhs.addRef());
        Dout(dc::erlang_memory, "["<<mErlTerm<<"]"<<"  \\-ErlTermPtr assigment");
        return *this;
    }
    template <class U>
    inline ErlTermPtr<T>& operator=(ErlTermPtr<U>& rhs) {
        reset(rhs.addRef());
        Dout(dc::erlang_memory, "["<<mErlTerm<<"]"<<"  \\-ErlTermPtr assigment");
        return *this;
    }
    inline ErlTermPtr& operator=(erlterm_ptr rhs) {
        reset(rhs);
        Dout(dc::erlang_memory, "["<<mErlTerm<<"]"<<"  \\-ErlTermPtr assigment (direct)");
        return *this;
    }
    template <class U>
    inline ErlTermPtr<T>& operator=(U *rhs) {
        reset(rhs);
        Dout(dc::erlang_memory, "["<<mErlTerm<<"]"<<"  \\-ErlTermPtr assigment (direct)");
        return *this;
    }

private:
 
    inline erlterm_ptr addRef() const {
        if (mErlTerm) {
            mErlTerm->addRef();
        }
        return mErlTerm;
    }

    erlterm_ptr mErlTerm;
};

template <class T, class U>
inline bool operator==(const ErlTermPtr<T> &t1, const ErlTermPtr<U> &t2) {
    return *t1 == *t2;
}

template <class T, class U>
inline bool operator!=(const ErlTermPtr<T> &t1, const ErlTermPtr<U> &t2) {
    return *t1 != *t2;
}

} //namespace type
}

#endif // __ERLTERMPTR_HPP

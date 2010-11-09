/*
***** BEGIN LICENSE BLOCK *****

This file is part of the erlXPCOM (Erlang XPCOM binding) project.

Copyright (C) 2005 Hector Rivas Gandara <keymon@gmail.com>

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published replyby the Free Software Foundation; either
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
#ifndef __MISCUTIL_H_ 
#define __MISCUTIL_H_ 
template<class T> class array_ptr {
public:
	inline explicit array_ptr(T *p = 0): ptr(p) {}
	inline virtual ~array_ptr() {
		delete [] ptr;
	}

	inline T& operator[](const int position) const{
		return ptr[position];
	}
	inline T* get() const {
		return ptr;
	}
	
	inline T* release() {
		T* p = ptr;
		ptr = 0;
		return p;
	}

	inline void reset(T *p = 0) {
		delete [] ptr;
		ptr = p;
	}
private:
	T *ptr;
};
#endif 

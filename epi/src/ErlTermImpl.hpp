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

/*
 * This macros implements the type checking method for ErlTerms
 */
#define IMPL_TYPE_SUPPORT(classname, base_type) \
    virtual inline bool instanceOf(const TermType type) const { \
        return type == base_type; \
	}\
	virtual inline TermType termType() const {\
         return base_type;\
	}\
\
	static inline classname * cast(ErlTerm* t) throw (EpiBadArgument) {\
		if (t != 0 && t->instanceOf(base_type)) {\
			return (classname *) t;\
		} else {\
			t->addRef(); \
			t->release();\
			throw EpiBadArgument("Imposible cast");\
		}\
	}

#define IMPL_TYPE_SUPPORT2(classname, base_type, second_type) \
    virtual inline bool instanceOf(const TermType type) const { \
		return (type == base_type) || (type == second_type); \
	}\
	virtual inline TermType termType() const {\
		return base_type;\
	}\
	static inline classname * cast(ErlTerm* t) throw (EpiBadArgument) {\
		if (t != 0 && t->instanceOf(base_type)) {\
			return (classname *) t;\
		} else {\
			t->addRef(); \
			t->release();\
			throw EpiBadArgument("Imposible cast to classname *");\
		}\
	}

#define IMPL_TYPE_SUPPORT3(classname, base_type, second_type, third_type) \
    virtual inline bool instanceOf(const TermType type) const { \
        return (type == base_type) || (type == second_type) || \
               (type == third_type); \
	}\
	virtual inline TermType termType() const {\
        return base_type;\
	}\
	static inline classname * cast(ErlTerm* t) throw (EpiBadArgument) {\
		if (t != 0 && t->instanceOf(base_type)) {\
			return (classname *) t;\
		} else {\
			t->addRef(); \
			t->release();\
			throw EpiBadArgument("Imposible cast to classname *");\
		}\
	}


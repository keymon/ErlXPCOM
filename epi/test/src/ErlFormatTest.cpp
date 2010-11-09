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

#include <iostream>

#include "ErlTypes.hpp"

using namespace epi::type;
using namespace epi::error;

void proba () {
    ErlTermPtr<ErlTerm> e;

//    e.reset(ErlTerm::format("{1,2,3}"));
    //std::cout << e->toString() << "\n";
    e.reset(ErlTerm::format("{1,-2,[4,5, -6.0]}"));
    std::cout << e->toString() << "\n";
/*    e.reset(ErlTerm::format("[hola, mundo]"));
    std::cout << e->toString() << "\n";
    e.reset(ErlTerm::format("{~a, ~w}", "pepe", e.get()));
    std::cout << e->toString() << "\n";

    ErlTerm *t1 = new ErlAtom("hola1");
    ErlTerm *t2 = new ErlAtom("hola2");

    e.reset(new ErlTuple(t1, t2));*/
}

int main() {

    Debug( dc::notice.on() );
    Debug( dc::erlang.on() );
    Debug( dc::erlang_warning.on() );
    Debug( dc::erlang_memory.on() );
    Debug( libcw_do.on() );

    proba();
    return 0;

}

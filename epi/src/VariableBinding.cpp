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


#include "ErlTerm.hpp"
#include "VariableBinding.hpp"

using namespace epi::type;

std::ostream& epi::type::operator<< (std::ostream &out, VariableBinding &binding)
{
    for (VariableBinding::ErlTermMap::const_iterator it = binding.mErlTermMap.begin();
         it != binding.mErlTermMap.end(); it++)
    {
        out << (*it).first << " = " << (*it).second->toString() << "\n";
    }
    return out;
}

// /* Test code */
//
// #include <iostream>
// #include "ErlAtom.hpp"
// #include "ErlLong.hpp"
//
// int main() {
//
//     ErlTerm *atom = new ErlAtom("an atom");
//     ErlTerm *along = new ErlLong(123);
//
//     VariableBinding *binding1 = new VariableBinding();
//     binding1->bind("Atom", atom);
//     binding1->bind("Long", along);
//
//     VariableBinding *binding2 = new VariableBinding(*binding1);
//
//     binding2->bind("Other", atom);
//
//     std::cout << "Binding 1 =\n" << *binding1;
//     std::cout << "Binding 2 =\n" << *binding2;
//
//     binding1->merge(binding2);
//
//     delete binding2;
//
//     std::cout << "Binding 1 + Binding 2 =\n" << *binding1;
//
//     delete binding1;
//     return 0;
// }

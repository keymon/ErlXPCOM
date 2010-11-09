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
#include <sstream>
#include <memory>

#include "ErlTypes.hpp"

using namespace epi::error;
using namespace epi::type;
using namespace epi::node;

#include "ErlTypes.hpp"

//////////////////////////////////////////////////////////////////////////
// Test code
void test_terms(ErlTerm *t1, ErlTerm *t2) {
    std::cout << "Testing ErlTerm Generic operations:\n";
    std::cout << "\tTerm 1: " << t1->toString() << "\n";
    std::cout << "\tTerm 2: " << t2->toString() << "\n";

    ErlTermPtr<ErlTerm> termcopy = t1;
    std::cout << "\tterm 1 copy: " << termcopy->toString() << "\n";

    std::cout << "\tterm 1 == term 2: " << (*t1 == *t2) << "\n";
    std::cout << "\tterm 1 == term 1 copy: " << (*t1 == *termcopy) << "\n";
    std::cout << "\tterm 2 == term 1 copy: " << (*t2 == *termcopy) << "\n";


}

void test_atoms() {
    ErlTermPtr<ErlTerm> atom1(new ErlAtom("atom1"));
    ErlTermPtr<ErlTerm> atom2(new ErlAtom("atom2"));
    std::cout << "atom1=" << atom1->toString() <<"\n";
    test_terms(atom1.get(), atom2.get());
}

void test_long() {
    ErlTermPtr<ErlLong> long1(new ErlLong(123));
    ErlTermPtr<ErlLong> long2(new ErlLong(456));
    test_terms(long1.get(), long2.get());
}

void test_double() {
    ErlTermPtr<ErlTerm> double1(new ErlDouble(3.14));
    ErlTermPtr<ErlTerm> double2(new ErlDouble(5.16));
    test_terms(double1.get(), double2.get());
}

void test_string() {
    ErlTermPtr<ErlTerm> string1(new ErlString("hello"));
    ErlTermPtr<ErlTerm> string2(new ErlString("world"));
    test_terms(string1.get(), string2.get());
}

void test_binary() {
    char *data1 = "This is a binary data";
    char *data2 = "This is other binary data";

    ErlTermPtr<ErlBinary> binary1(new ErlBinary(data1, strlen(data1), false));
    ErlTermPtr<ErlBinary> binary2(new ErlBinary(data2, strlen(data1), true));

    std::cout << "Binary data 1 (not copied) from " << (void *) data1 <<
            " data ptr: " << binary1->binaryData() << "\n";
    std::cout << "Binary data 2 (copied) from " << (void *) data2 <<
            " data ptr: " << binary2->binaryData() << "\n";

    test_terms(binary1.get(), binary2.get());
}

void test_port() {
    ErlTermPtr<ErlTerm> port1(new ErlPort("a@node", 1, 0));
    ErlTermPtr<ErlTerm> port2(new ErlPort("a@node", 2, 0));
    test_terms(port1.get(), port2.get());
}

void test_pid() {
    ErlTermPtr<ErlTerm> pid1(new ErlPid("a@node", 1, 2, 0));
    ErlTermPtr<ErlTerm> pid2(new ErlPid("a@node", 3, 4, 0));
    test_terms(pid1.get(), pid2.get());
}

void test_ref() {
    unsigned int ids1[]={1,3,4};
    unsigned int ids2[]={1};

    ErlTermPtr<ErlTerm> ref1(new ErlRef("a@node", ids1, 0, true));
    ErlTermPtr<ErlTerm> ref2;
    try {
        ref2.reset(new ErlRef("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa@node", ids2, 0, false));
    } catch (EpiException &e) {
        std::cout << "Catched exception: " << e.getMessage() << "\n";
    }

    ref2.reset(new ErlRef("a@node", ids2, 0, false));

    test_terms(ref1.get(), ref2.get());
}

void test_tuple() {
    ErlTermPtr<ErlTuple> empty_tuple(new ErlTuple(0));

    ErlTermPtr<ErlTuple> tuple1(new ErlTuple(new ErlAtom("an_atom_in_a_tuple")));
    ErlTermPtr<ErlTuple> tuple2(new ErlTuple(new ErlAtom("an_atom_in_a_tuple"),
                                                new ErlLong(123)));
    ErlTermPtr<ErlTuple> tuple3(new ErlTuple(
                                   new ErlTuple(
                                       new ErlTuple(
                                           new ErlAtom("an_atom_in_a_tuple_in_a_tuple"),
                                           new ErlLong(123))),
                                       new ErlLong(456)));

    ErlTerm *v[10] = {
        new ErlAtom("an_atom_in_a_tuple_in_a_tuple"),
        new ErlLong(456),
        new ErlAtom("other_atom_in_a_tuple_in_a_tuple"),
        new ErlString("hola")
    };

    ErlTermPtr<ErlTuple> tuple4 (new ErlTuple(v, 3));
    std::cout << "tuple4="<< tuple4->toString() << "\n";


    for (int i = 0; i<tuple3->arity(); i++){
        std::cout << "tuple3["<< i << "]=" << tuple3->elementAt(i) << "\n";
    }

    test_terms(empty_tuple.get(), tuple1.get());
    test_terms(tuple1.get(), tuple2.get());
    test_terms(tuple3.get(), tuple2.get());
}

void test_list() {
    ErlTermPtr<ErlList> empty_list(new ErlEmptyList());
    ErlTermPtr<ErlConsList> list1(new ErlConsList(new ErlAtom("an_atom_in_a_list")));

    ErlTermPtr<ErlConsList> list2(new ErlConsList(new ErlAtom("an_atom_in_a_list"),
                                   new ErlLong(123)));
    ErlTermPtr<ErlConsList> list3(new ErlConsList(
            new ErlConsList(
            new ErlConsList(
            new ErlAtom("an_atom_in_a_list_in_a_list"),new ErlLong(123),
                new ErlLong(456), new ErlDouble(3.4))),
    new ErlLong(456)));

    ErlTermPtr<ErlConsList> list4 = (ErlConsList*)
            ((ErlConsList*)
              ((ErlConsList*)
                list3->elementAt(0))->elementAt(0))->tail(0);

    std::cout << "list3=" << list3->toString() << "\n";
    std::cout << "list4=" << list4->toString() << "\n";
    for (int i=0; i<list4->arity(); i++) {
        const ErlTerm *elem = list4->elementAt(i);
        std::cout << "list4[" << i << "]:" << elem << "=" << elem->toString() << "\n";
    }

    test_terms(empty_list.get(), list1.get());
    test_terms(list1.get(), list2.get());
    test_terms(list3.get(), list2.get());
}

void test_match(VariableBinding *binding, ErlTerm *t1, ErlTerm *t2)  {
    std::cout << "Matching " << t1->toString() << " = " << t2->toString() << "\n";
    if (t1->match(t2, binding)) {
        std::cout << "Matches!. Binding:\n" << *binding << "\n";
    } else {
        std::cout << "No matches: :(\n";
    }
}

void test_variables() {

    VariableBinding binding;

    /**  [atom, {value, X}, "hello world"] = [atom, {value, 100}| T] */
    ErlTermPtr<ErlConsList> list1 =
            new ErlConsList(new ErlAtom("atom"),
                            new ErlTuple(new ErlAtom("value"), new ErlVariable("X")),
                            new ErlString("hello world"));
    ErlTermPtr<ErlConsList> list2 = new ErlConsList();
    list2->addElement(new ErlAtom("atom"));
    list2->addElement(new ErlTuple(new ErlAtom("value"), new ErlLong(100)));
    list2->close(new ErlVariable("T"));

    test_match(&binding, list1.get(), list2.get());

    /**  Y = {X, X} */
    ErlTermPtr<ErlTerm> variable_y = new ErlVariable("Y");
    ErlTermPtr<ErlTerm> tuple_x_x = new ErlTuple(new ErlVariable("X"), new ErlVariable("X"));

    test_match(&binding, variable_y.get(), tuple_x_x.get());

    /**  {X, hello, [1,2,3]} = {2.10, hello, _} */
    ErlTermPtr<ErlTerm> tuple1 = new ErlTuple(new ErlVariable("X"), new ErlAtom("hello"),
                                              new ErlConsList(new ErlLong(1),new ErlLong(2),new ErlLong(3)));
    ErlTermPtr<ErlTerm> tuple2 = new ErlTuple(new ErlDouble(2.1), new ErlAtom("hello"),new ErlVariable());

    test_match(&binding, tuple1.get(), tuple2.get());

    /**  [Y|Z] = [{100,100}, 1,2,T,4] */
    ErlTermPtr<ErlConsList> list3 = new ErlConsList();
    list3->addElement(new ErlVariable("Y"));
    list3->close(new ErlVariable("Z"));
    ErlTermPtr<ErlConsList> list4 = new ErlConsList();
    list4->addElement(new ErlTuple(new ErlLong(100), new ErlLong(100)));
    list4->addElement(new ErlLong(1));
    list4->addElement(new ErlLong(2));
    list4->addElement(new ErlVariable("T"));
    list4->addElement(new ErlLong(4));
    list4->close();

    test_match(&binding, list3.get(), list4.get());


/*    ErlTermPtr<ErlTerm> atom = new ErlAtom("an atom");
    ErlTermPtr<ErlTerm> variable = new ErlVariable("X");

    if (atom->match(&binding, variable.get())) {
        std::cout << "Matches: X=" << variable->toString(&binding) << "\n";
    } else {
        std::cout << "No matches: :(\n";
}
*/
}


int main() {

     Debug( dc::notice.on() );
     Debug( dc::erlang.on() );
     Debug( dc::erlang_warning.on() );
     Debug( dc::erlang_memory.on() );
     Debug( libcw_do.on() );

     test_atoms();
     test_long();
     test_double();
     test_string();
     test_ref();
     test_pid();
     test_port();
     test_binary();
     test_tuple();
     test_list();
     test_variables();

     return 0;
}


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
#include <ostream>
#include <memory>

#include "ErlTypes.hpp"

#include "MiniCppUnit.hxx"

using namespace epi::error;
using namespace epi::type;
using namespace epi::node;

#include "ErlTypes.hpp"

class ErlTypesTest : public TestFixture<ErlTypesTest>
{
public:
	 TEST_FIXTURE( ErlTypesTest )
	 {
         TEST_CASE( basicTypesTest );
         TEST_CASE( tupleTest );
         TEST_CASE( listTest );
         TEST_CASE( variableTest );
     }

     void basicTypesTest() {
         std::ostream &out_stream = TestsListener::theInstance().messageLog();

         char *data1 = "Binary Data 1";
         char *data2 = "Binary Data 2";
         unsigned int ids1[]={1,3,4};
         unsigned int ids2[]={1};
         ErlTermPtr<ErlTerm> erlTermList[] = {
             new ErlAtom("atom1"),
             new ErlAtom("atom2"),
             new ErlLong(123),
             new ErlLong(456),
             new ErlDouble(3.1416),
             new ErlDouble(2.1313),
             new ErlString("string term"),
             new ErlString("other string term"),
             new ErlBinary(data1, strlen(data1)),
             new ErlBinary(data2, strlen(data2)),
             new ErlPid("a@node", 1, 2, 0),
             new ErlPid("a@node", 3, 4, 0),
             new ErlPort("a@node", 1, 0),
             new ErlPort("a@node", 2, 0),
             new ErlRef("a@node", ids1, 0, true),
             new ErlRef("a@node", ids2, 0, false),
             0
         };

         ErlTermPtr<ErlTerm> term1;
         ErlTermPtr<ErlTerm> term2;

         int i = 0;
         while (erlTermList[i].get()) {
             term1.reset(erlTermList[i].get());
             term2.reset(erlTermList[i+1].get());
             i+=2;

             ASSERT( term1 == term1 );
             ASSERT( term1 != term2 );

             out_stream << term1->toString() << "!=" << term2->toString() << "\n";
         }

     }

     void tupleTest() {

         ErlTermPtr<ErlTuple> empty_tuple(new ErlTuple((unsigned int) 0));

         ErlTermPtr<ErlTuple> tuple1(new ErlTuple(new ErlAtom("an_atom_in_a_tuple")));
         ErlTermPtr<ErlTuple> tuple2(new ErlTuple(new ErlAtom("an_atom_in_a_tuple"),
                                     new ErlLong(123)));
         ErlTermPtr<ErlTuple> tuple3(
                 new ErlTuple(
                    new ErlTuple(
                        new ErlTuple(
                            new ErlAtom("an_atom_in_a_tuple_in_a_tuple"),
                            new ErlLong(123))),
                        new ErlLong(456)));

         ASSERT( empty_tuple->toString() == "{}" );
         ASSERT( *tuple1->elementAt(0) == *tuple2->elementAt(0));
         ASSERT( *tuple1->elementAt(0) != *tuple2->elementAt(1));
         ASSERT( tuple3->toString() == "{{{an_atom_in_a_tuple_in_a_tuple,123}},456}");

         ErlTerm *v[10] = {
             new ErlAtom("atom1"),
             new ErlLong(111),
             new ErlAtom("atom2"),
             new ErlString("hello")
         };

         ErlTermPtr<ErlTuple> tuple4 (new ErlTuple(v, 3));

         ASSERT( tuple4->toString() == "{atom1,111,atom2}");

         ASSERT( empty_tuple != tuple1);
         ASSERT( tuple1 != tuple2);
         ASSERT( tuple2 != tuple3);
         ASSERT( tuple3 == tuple3);

     }

     void listTest() {
         ErlTermPtr<ErlList> empty_list(new ErlEmptyList());
         ErlTermPtr<ErlConsList> list1(new ErlConsList(
			new ErlAtom("an_atom_in_a_list")));

         ErlTermPtr<ErlConsList> list2(new ErlConsList(new ErlAtom("an_atom_in_a_list"),
                                       new ErlLong(123)));
         ErlTermPtr<ErlConsList> list3(
                 new ErlConsList(
                    new ErlConsList(
                        new ErlConsList(
                            new ErlAtom("an_atom_in_a_list_in_a_list"),new ErlLong(123),
                            new ErlLong(456), new ErlDouble(3.4))),
                    new ErlLong(456)));

         ErlTermPtr<ErlConsList> list4 =
                 (ErlConsList*)
                    ((ErlConsList*)
                        ((ErlConsList*)
                            list3->elementAt(0))->elementAt(0))->tail(0);

		 ErlTermPtr<ErlList> list5 = new ErlString("hola");
		 ErlTermPtr<ErlList> list6 = 
			new ErlConsList(
				new ErlLong(104),
				new ErlLong(111),
				new ErlLong(108),
				new ErlLong(97));

         ASSERT( empty_list->toString() == "[]");
         ASSERT( *list1->elementAt(0) == *list2->elementAt(0));
         ASSERT( *list1->elementAt(0) != *list2->elementAt(1));
         ASSERT( *empty_list.get() == *list1->tail(0));
         ASSERT( *empty_list.get() == *list2->tail(1));
         ASSERT( *empty_list.get() != *list2->tail(0));
         ASSERT( list4->toString() == "[123,456,3.4]");

         ASSERT( *empty_list.get() == *list5->tail(3));
         ASSERT( *list5 != *list4);
         ASSERT( *list5 == *list6);
     }

     void variableTest() {

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

         ASSERT( list1->match(list2.get(), &binding) );
         ASSERT( binding.search("X") != 0 );

         /**  Y = {X, X} */
         ErlTermPtr<ErlTerm> variable_y = new ErlVariable("Y");
         ErlTermPtr<ErlTerm> tuple_x_x = new ErlTuple(new ErlVariable("X"), new ErlVariable("X"));

         ASSERT( variable_y->match(tuple_x_x.get(), &binding) );

         /**  {X, hello, [1,2,3]} = {2.10, hello, _} */
         ErlTermPtr<ErlTerm> tuple1 = new ErlTuple(new ErlVariable("X"), new ErlAtom("hello"),
                 new ErlConsList(new ErlLong(1),new ErlLong(2),new ErlLong(3)));
         ErlTermPtr<ErlTerm> tuple2 = new ErlTuple(new ErlDouble(2.1), new ErlAtom("hello"),
                 new ErlVariable());

         ASSERT( !tuple1->match( tuple2.get(), &binding ) );

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

         ASSERT( list3->match( list4.get(), &binding ) );
         ASSERT( binding.search("Z")->toString() == "[1,2,[hello world],4]" );

     }


private:
    ErlTerm **mTermList;
};

REGISTER_FIXTURE( ErlTypesTest  )

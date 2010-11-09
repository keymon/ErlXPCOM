
// Recupera a clase
const XPInterfaceHelperClass=
    new Components.Constructor(
        "@lfcia.org/xpinterface/lfXPInterfaceHelper;1",
        "lfIXPInterfaceHelper");

// Crea unha instancia
var xpInterfaceHelper=new XPInterfaceHelperClass();

print("ITheSample instantiated. Methods:");

// Imprime todas as funcions
for(var list in xpInterfaceHelper) print("\t"+list);

print("Print test:");

// Proba o método de print
an_atom = xpInterfaceHelper.newAtom("an atom!");

print("Created atom \""+an_atom.toString()+"\"");
print(" value ="+an_atom.value);

// Imprime todas as funcions
for(var list in an_atom) print("\t"+list);

other_atom = xpInterfaceHelper.newAtom("other atom!");
other_other_atom = xpInterfaceHelper.newAtom("other atom!");

if (other_atom.equals(an_atom))
    print ("Equals");
else
    print ("Not equals");

if (other_atom.equals(other_atom))
    print ("Equals");
else
    print ("Not equals");

if (other_atom.equals(other_other_atom))
    print ("Equals");
else
    print ("Not equals");

an_long = xpInterfaceHelper.newLong(4);
print("Created long \""+an_long.toString()+"\"");
an_double = xpInterfaceHelper.newDouble(3.14);
print("Created double \""+an_double.toString()+"\"");
an_string = xpInterfaceHelper.newString("an string!");
print("Created string \""+an_string.toString()+"\"");

some_data = [1, 2, 3, 100, 101, 102, 426, 345 ,687, 3, 2, 1];
an_binary = xpInterfaceHelper.newBinary(some_data.length, some_data);
print("Created binary \""+an_binary.toString()+"\"");

an_pid = xpInterfaceHelper.newPid("node@somewhere", 1, 2, 3);
print("Created pid \""+an_pid.toString()+"\"");

an_ref = xpInterfaceHelper.newOldStyleRef("node@somewhere", 1, 3);
print("Created ref \""+an_ref.toString()+"\"");

other_ref = xpInterfaceHelper.newNewStyleRef("node@somewhere", 1, 2, 3, 4);
print("Created (new style) ref \""+other_ref.toString()+"\"");

an_port = xpInterfaceHelper.newPort("node@somewhere", 1, 2);
print("Created port \""+an_port.toString()+"\"");

an_stupid_tuple = xpInterfaceHelper.newTuple(0);
print("Created stupid tuple \""+an_stupid_tuple.toString()+"\"");

an_tuple = xpInterfaceHelper.newTupleFromArray(2,[an_atom, other_atom]);
print("Created tuple \""+an_tuple.toString()+"\"");

other_tuple = xpInterfaceHelper.newTuple(4);
other_tuple.initElement(an_atom);
other_tuple.initElement(an_double);
other_tuple.initElement(an_tuple);
other_tuple.initElement(an_binary);

print("Created other tuple \""+other_tuple.toString()+"\"");
hola = other_tuple.elementAt(0);
for (var x in [0,1,2,3])
	print("\t"+x+": \""+other_tuple.elementAt(x).toString()+"\"");


an_list = xpInterfaceHelper.newList(2,[an_atom, other_atom]);
print("Created a list \""+an_list.toString()+"\"");

other_list = xpInterfaceHelper.newConsList();
other_list.addElement(other_tuple);
other_list.addElement(an_atom);
other_list.addElement(an_list);
other_list.close(xpInterfaceHelper.newEmptyList());
print("Created other list \""+other_list.toString()+"\"");

for (var x in [0,1,2])
	print("\t"+x+": \""+other_list.elementAt(x).toString()+"\"");

print("head =\""+other_list.head().toString()+"\"");
print("tail =\""+other_list.tail().toString()+"\"");

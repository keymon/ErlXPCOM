// Comunication parameters 
localNodeName = "pepito@makako.local";
remoteNodeName = "pepita@makako.local";
cookie = "one_cookie";

////////////////////////////////////////////////////////////////////////////
// Get the class XPInterfaceHelper factory
const XPInterfaceHelperClass=
    new Components.Constructor(
        "@lfcia.org/xpinterface/lfXPInterfaceHelper;1",
        "lfIXPInterfaceHelper");

// Create a instance of this class
var xpInterfaceHelper = new XPInterfaceHelperClass();

print("lfIXPInterfaceHelper instantiated. Methods:");

// Show all functions
for(var list in xpInterfaceHelper) print("\t"+list);

// create a node and get a new mailbox
print ("Creating node "+localNodeName);
node = xpInterfaceHelper.newNode(localNodeName, cookie);
print ("Creating mailbox");
mailbox = node.createMailBox();
print ("Mailbox = "+mailbox.self);

operation="fibonacci";
x=10;
self_pid = mailbox.self;
operation_atom = xpInterfaceHelper.newAtom(operation);
value_long = xpInterfaceHelper.newLong(x);
operation_tuple = xpInterfaceHelper.newTupleFromArray(3, 
	[self_pid, operation_atom, value_long]);
mailbox.remoteRegSend(remoteNodeName, "xpinterface_example", operation_tuple);
term = mailbox.receiveWithTimeout(1000);
if (term == null) {
	print("Timedout! :(");
} else {
	print(term.value);
}

// Send a atom to a reply server
an_atom = xpInterfaceHelper.newAtom("an atom!");
an_pid = mailbox.self;
the_tuple = xpInterfaceHelper.newTupleFromArray(2, [an_pid, an_atom]);

print ("tuple = ", the_tuple.toString());

// Send the message and receive the response
try {
	mailbox.remoteRegSend(remoteNodeName, "reply_server", the_tuple);

	print ("Recibindo");
	term = mailbox.receive();

	print ("Recibido: "+term);

} catch( e if (e.result & 0xff) == 0x02) {
	print("\tException caught: " + e.toString());
	print("\tMessage: " + e.message);
	print("\tname: " + e.name);
	print("\tresult: " + e.result);
	print("\tdata " + e.location);
	print("\tJS Type of exception: " + typeof e + "\n");
	
	print("\tMethods: " + typeof e + "\n");
	for(var list in e) print("\t"+list);
}                   

node.ping(remoteNodeName, 1000);


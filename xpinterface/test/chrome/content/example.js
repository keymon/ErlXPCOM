function clearall() {
	document.getElementById("value").value ="0";
	document.getElementById("result").value = "?";
}

function calculate() {
	operation = document.getElementById("operation").selectedItem.id;
	thevalue = document.getElementById("value").value;
	ret = remote_operation(operation, thevalue);
	document.getElementById("result").value = ret;
}

////////////////////////////////////////////////////////////////////
const XPInterfaceHelperClass=
    new Components.Constructor(
        "@lfcia.org/xpinterface/lfXPInterfaceHelper;1",
        "lfIXPInterfaceHelper");

// Create a instance of this class
const xpInterfaceHelper = new XPInterfaceHelperClass();

var node = null;
var mailbox = null;

function create() {
	localNodeName = document.getElementById("localname").value;
	cookie = document.getElementById("cookie").value;

	node = xpInterfaceHelper.newNode(localNodeName, cookie);
	mailbox = node.createMailBox();
	
	document.getElementById("create-button").disabled = true;
	document.getElementById("ping-button").disabled = false;
	document.getElementById("exec-button").disabled = false;
}

function ping() {
	remoteNodeName = document.getElementById("remotename").value; 
	if (node.ping(remoteNodeName, 1000)) {
		document.getElementById("remote-state").value = "pong!";
	} else {
		document.getElementById("remote-state").value = "timedout :(";
	}
}

function remote_operation(operation, x) {
	remoteNodeName = document.getElementById("remotename").value; 
	try {
		self_pid = mailbox.self;
		operation_atom = xpInterfaceHelper.newAtom(operation);
		value_long = xpInterfaceHelper.newLong(x);
		operation_tuple = xpInterfaceHelper.newTupleFromArray(3, 
			[self_pid, operation_atom, value_long]);
		mailbox.remoteRegSend(remoteNodeName, "xpinterface_example", operation_tuple);
		term = mailbox.receiveWithTimeout(1000);
		if (term == null) {
			alert("Timedout! :(");
			return "?";
		} else {
			return term;
		}
	} catch( e ) {
		alert("Exception:"+e.message);
	}
}

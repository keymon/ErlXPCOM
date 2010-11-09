// Creamos o EventQueue para este thread
var EventQClass = Components.classes['@mozilla.org/event-queue-service;1'];
var EventQObj =  EventQClass.getService(Components.interfaces.nsIEventQueueService);
EventQObj.createThreadEventQueue();

// Obtemos o event queue
event_queue = EventQObj.getSpecialEventQueue(0);

// Recupera a clase
const ComponentClass = 
	new Components.Constructor("@lfcia.org/TestErlXPCOMComponent", 
							   "ITestErlXPCOMComponent");

// Crea unha instancia
var component = new ComponentClass();

print("ITestErlXPCOMComponent instantiated. Methods:");

// Imprime todas as funcions
for(var list in component) print("\t"+list);

component.Init("pepito@makako.local", "pepita@makako.local", "one_cookie", false);
// Poñemonos a procesar os eventos entrantes
event_queue.eventLoop();

// Procesar eventos pendientes e sair.
event_queue.processPendingEvents();

//////////////////////////////////////////////////////////////////////////////////////////
// Lanzando noutro thread. Para un exemplo de threads en javascript ver:
// http://lxr.mozilla.org/seamonkey/source/js/src/xpconnect/tests/js/old/threads.js

global acomponent;

// Definición da función de lanzado de ErlXPCOM
function launch_erlxpcom () {
		// Creamos o EventQueue para este thread
		var EventQClass = Components.classes['@mozilla.org/event-queue-service;1'];
		var EventQObj =  EventQClass.getService(Components.interfaces.nsIEventQueueService);
		EventQObj.createThreadEventQueue();
		
		// Obtemos o event queue
		event_queue = EventQObj.getSpecialEventQueue(0);
		
		// Recupera a clase
		const ComponentClass = 
			new Components.Constructor("@lfcia.org/TestErlXPCOMComponent", 
									   "ITestErlXPCOMComponent");
		// Crea unha instancia
		var component = new ComponentClass();
		acomponent = component;
		
		component.Init("pepito@makako.local", "pepita@makako.local", "one_cookie", false);
		// Poñemonos a procesar os eventos entrantes
		event_queue.eventLoop();
}

// Crea o thread
const nsIThread = Components.interfaces.nsIThread;
const ThreadContractID = "@mozilla.org/thread;1";
const ThreadClass = new Components.Constructor(ThreadContractID, "nsIThread", "init");

var the_thread = new ThreadClass({run: launch_erlxpcom }, 0, 
		                         nsIThread.PRIORITY_NORMAL,
                                 nsIThread.SCOPE_GLOBAL,
                                 nsIThread.STATE_JOINABLE);

//////////////////////////////////////////////////////////////////////////////////////////
// Probando un componeente implementado en erlang
const testClass = new Components.Constructor("@lfcia.org/erlang/TestErlXPCOMComponent", "ITestErlXPCOMComponent");
var object = new testClass();
object.PrintStringArray(3, ["hola", "mundo", "cruel"]);
var size = {};

for(var element in object.GetStrings(size)) 
	print("\t"+element);

for(var list in object) print("\t"+list);
object.aMethod();



//////////////////////////////////////////////////////////////////////////////////////////
// Colle o remote component
for(var list in acomponent) print("\t"+list);
remote_component = acomponent.getObject();

// Test de envio dunha execucion remota
// {erlxpcom, 'pepito@makako.local'}! {call_method, 0, 1, "aMethod", []}.
// {erlxpcom, 'pepito@makako.local'}! {call_method, 0, 1, "testShort", [1 | 1]}.
// {erlxpcom, 'pepito@makako.local'}! {call_method, 0, 1, "pepe", []}.

var InterfaceInfoManager = Components.classes['@mozilla.org/event-queue-service;1'];
var InterfaceInfo = InterfaceInfoManager.getInfoForName('nsIInterfaceInfo');
var index;
var methodInfo;
print  getMethodInfoForName ( 'interfaceInfo' , index , methodInfo ) 

var EventQObj =  EventQClass.getService(Components.interfaces.nsIEventQueueService);
EventQObj.createThreadEventQueue();


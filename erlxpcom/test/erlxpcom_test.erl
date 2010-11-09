{ok, ComponentManager} = erlxpcom_orb:get_component_manager().
{ok, [NewObject]} = xpcom_object:call_method(ComponentManager, 
	createInstance, ["37a524b7-e2ed-4b35-8560-0636148f5282", 
		null, "37a524b7-e2ed-4b35-8560-0636148f5282"]).

xpcom_object:call_method(NewObject, 'PrintStringArray', [["hola", "mundo", "cruel"]]).

xpcom_object:call_method(NewObject, aMethod, []).
xpcom_object:call_method(NewObject, testAll, 
						 [true, 22, -222, 222222, -2222, 
						   1, 2, 3, 3.14, 2.27, 
						   $a, 1234]).
						   
xpcom_object:call_method(NewObject, testInOutAll, [true, 22, -222, 222222, 1, 2, 3.14, 2.27, $a, 1234]).

TestObjectFunction(NewObject, testAll, 
	[true, 22, -222, 222222, -2222, 
		   1, 2, 3, 3.14, 2.27, 
		   $a, 1234]);


{ok, OtherObject} = testErlXPCOMComponent:create().

xpcom_object:call_method(NewObject, "setObject", [OtherObject]).
xpcom_object:call_method(NewObject, "getObject", []).

TheObject = erlxpcom_orb:get_remote_object_by_oid(erlxpcom, 1). 
xpcom_object:call_method(NewObject, "setObject", [OtherObject]).
xpcom_object:call_method(NewObject, "getObject", []).



%% Test factorias 
ComponentInfo = 
#ns_module_component_info{
		description = "Un obxeto 1", 
		cid = "37a524b7-e2ed-4b35-8560-0636148f5282", 
		contractID = "@lfcia.org/TestErlXPCOMComponent1", 
		constructor = 
			?NS_GENERIC_FACTORY_CONSTRUCTOR_FUNCTION(testErlXPCOMComponent, create, [])
	}.

Factory = ns_generic_factory:create(ComponentInfo);
xpcom_object:call_method(Factory, createInstance, 
						 [null, "7a524b7-e2ed-4b35-8560-0636148f5282"]).


%% Probando broadcasters
var observer = {
	observe: function(aSubject, aTopic, aData) {
		alert("From: "+aSubject + " saw: " + aTopic + " data: " + aData);
	},
	
	QueryInterface: function(iid) {
		if (iid.equals(Components.interfaces.nsIObserver) ||
			iid.equals(Components.interfaces.nsISupportsWeakReference)||
			iid.equals(Components.interfaces.nsISupports)) {
			return this;
		}
		throw Components.results.NS_NOINTERFACE;
		
	}
};

var CC = Components.classes, CI = Components.interfaces;
var cls, caster;

cls = CC["@mozilla.org/observer-service;1"];
caster = cls.getService(CI.nsIObserverService);

caster.notifyObservers(null, "marrower-user-online", "hola");

caster.addObserver(observer, "marrower-user-online", true);

%%

xpcom_object:query_interface(ComponentManager, "8bb35ed9-e332-462d-9155-4a002ab5c958").


{ok, [CString]} = xpcom_object:call_method(ComponentManager, 
createInstanceByContractID, ["@mozilla.org/supports-cstring;1", 
null, "d65ff270-4a1c-11d3-9890-006008962422"]).
xpcom_object:set_attribute(CString, data, "hola").

xpcom_object:call_method(ObserverService, notifyObservers, 
						 [CString, "my-event", ""]).


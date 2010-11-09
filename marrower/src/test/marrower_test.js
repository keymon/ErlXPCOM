
function printUser(user) {
	print(user.login + ": "+ user.state +", "+ user.alias +", "+ user.firstname +", "+ 
		 user.lastname +", "+ user.email +", "+ user.birthdate);
}

var ComponentClass = 
	new Components.Constructor("@lfcia.org/marrower;1", "lfIMarrower");

// Crea unha instancia
var component = new ComponentClass();

var size = {};
var userstates = [];
var userlogins = [];
var useraliases = [];
component.listAllUsers(size, userstates, userlogins, useraliases);
for (i = 0; i<size.value; i++) {
	print(userlogins.value[i]+": "+ useraliases.value[i]+", "+userstates.value[i]);
}

user = component.getUserInfo("victor");
printUser(user);

component.login("victor", "Pepe");
user = component.getUserInfo("victor");
printUser(user);

component.sendMessage("victor", "Hola tio");

component.logout();
user = component.getUserInfo("victor");
printUser(user);

//////////////////////////////
function printTask(task) {
	print(task.id + ": "+ task.name +", "+ task.description +", "+ task.duration +", "+ 
		 task.state +", "+ task.completion +", "+ task.priority);
}
enumerator = component.findAllTasks();

while (enumerator.hasMoreElements()) {
	task = enumerator.getNext().QueryInterface(
		Components.interfaces.lfITaskVO);
	printTask(task);
}


var size = {};
var tasks = component.findTaskByAssigned("chema", size);
for (i = 0; i<size.value; i++) {
	printTask(tasks[i]);
}

task = component.findTaskById(12);
printTask(task);
task.name = "Tarefa 12, modificada";
component.updateTask(task);

task.name = "Copia de tarefa 12";
newTask = component.createTask(task);
print("Novo id:"+newTask.id);

component.removeTask(newTask.id);
task = component.findTaskById(newTask.id);
printTask(task);

var size = {};
var predepends = component.findPredepends(15, size);
for (i = 0; i<size.value; i++) {
	print(predepends[i]);
}
var postdepends = component.findPostdepends(15, size);
for (i = 0; i<size.value; i++) {
	print(postdepends[i]);
}

component.addRelation(15, 3);
component.removeRelation(15, 17);

var postdepends = component.findPostdepends(15, size);
for (i = 0; i<size.value; i++) {
	print(postdepends[i]);
}


%%%
net_adm:ping('erlang@makako.local').
marrower_server:login("chema", "chemita", self());






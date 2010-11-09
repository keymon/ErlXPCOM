/** 
 * crea un item a partir dunha arbore 
 */
function createItemFromTask( task ) {

	var row = document.createElement('treerow');
	row.id = task.id;

	var state_properties = {};
	state_properties[Components.interfaces.lfITaskVO.STOPED] ="stopped";
	state_properties[Components.interfaces.lfITaskVO.INPROGRESS] ="inprogress";
	state_properties[Components.interfaces.lfITaskVO.COMPLETED] ="completed";
	state_properties[Components.interfaces.lfITaskVO.CANCELED] ="canceled";

	var stateChild = document.createElement('treecell');
	stateChild.setAttribute('properties', state_properties[task.state]);
	row.appendChild(stateChild);	

	keys = ["name", "description", "duration"]
	for (i in keys) {
		var child = document.createElement('treecell');
		child.setAttribute('label', task[keys[i]]);
		row.appendChild(child);
	}

	var completionChild = document.createElement('treecell');
	completionChild.setAttribute('mode', "normal");
	completionChild.setAttribute('value', task.completion);
	row.appendChild(completionChild);
			
	var treeitem = document.createElement('treeitem');
	treeitem.appendChild(row);

	return treeitem;
	
};

/** 
 * Crear un item da lista de usuarios
 */
function createItemFromUser( userlogin, userstate, useralias ) {

	var item = document.createElement('listitem');
	item.setAttribute('id', userlogin);
	
	var statecell = document.createElement('listcell');
	var image =  document.createElement('image');
	if (userstate == Components.interfaces.lfIUserVO.OFFLINE) {
		image.setAttribute('class', "offline-mark");
	} else {
		image.setAttribute('class', "online-mark");
	}	
	statecell.appendChild(image);
	item.appendChild(statecell);
	
	var aliascell = document.createElement('listcell');
	aliascell.setAttribute('label', useralias);
	item.appendChild(aliascell);
	return item;
}


//---------------------------------------------------------------------
/**
 * Contrue un novo controlador de Marrower, que actuará de interfaz entre
 * o GUI e o nucleo. 
 */
function MarrowerController() {

	var MarrowerFacadeClass = 
			new Components.Constructor("@lfcia.org/marrower;1", "lfIMarrower");

	this.TaskVOClass = 
			new Components.Constructor("@lfcia.org/marrower/taskVO;1", "lfITaskVO");
	
	// A fachada de marrower
	this.marrowerFacade = new MarrowerFacadeClass();

	/** Obxecto da árbore de tareas */
	this.task_tree = document.getElementById("task-tree");

	/** Obxecto da arbore coa lista de tareas */
	this.task_tree_list = document.getElementById("task-list-tree");

	/** Obxecto da lista de usuarios */
	this.user_list = document.getElementById("main-userList");
	
	/** Lista de tareas anteriores */
	this.predepends_list = document.getElementById("main-predepends");

	/** Lista de tareas posteriores */
	this.postdepends_list = document.getElementById("main-postdepends");

	/** Lista de asignacion de tareas */
	this.assigned_to_list = document.getElementById("main-assigned-to");
	
	/** Dialogo de conexión */
	this.connect_dialog = document.getElementById("main-connect-dialog");


	//---------------------------------------------------------------------
	/** 
		Mapa de usuarios, indexado por login. Conten:
		login = login do usuario
		alias = alias do usuario
		state = estado do usuario
		userVO = userVO do usuario. Pode ser null
		item = cela da lista de usuarios
	*/
	this.users = null;
	
	/** Array coas tarefas. Contén pares treeitem/taskVO */
	this.tasks = null;
	
	this.caster = Components.classes["@mozilla.org/observer-service;1"].
				getService(Components.interfaces.nsIObserverService);
				

}
	

	
//---------------------------------------------------------------------
MarrowerController.prototype.init = function() {
	this.getAllTasks();
	this.refreshTasks();
	this.getAllUsers();
	this.refreshUsers();

	this.caster.addObserver(this, "marrower-user-online", true);
	this.caster.addObserver(this, "marrower-user-offline", true);
	this.caster.addObserver(this, "marrower-task-updated", true);
}

MarrowerController.prototype.connect = function() {
	var login = document.getElementById("main-login").value;
	var alias = document.getElementById("main-alias").value;
	try {
		this.marrowerFacade.login(login, alias);
	} catch (e) {
		alert("Login inválido");
		return;
	}
	this.init();
	this.connect_dialog.setAttribute('hidden', "true");
}	
	
//---------------------------------------------------------------------
MarrowerController.prototype.getAllTasks = function() {
	var enumerator = this.marrowerFacade.findAllTasks();
	this.tasks = {};
	
	while (enumerator.hasMoreElements()) {
		var task = enumerator.getNext().QueryInterface(
			Components.interfaces.lfITaskVO);
		var id = task.id;
		this.tasks[id] = {};
		this.tasks[id]["taskVO"] = task;
	}	
};

MarrowerController.prototype.getUserTasks = function( login ) {
	var count = {};
	var tasks = this.marrowerFacade.findTaskByAssigned(login, count);

	this.tasks = {};
	for (var i = 0; i< count.value; i++) {
		var task = tasks[i];
		this.tasks[task.id] = {};
		this.tasks[task.id]["taskVO"] = task;
	}	
};

MarrowerController.prototype.getAllUsers = function() {
	var size = {}, userstates = {}, userlogins = {}, useraliases = {};
	
	this.marrowerFacade.listAllUsers(size, userstates, userlogins, useraliases);

	this.users = {};
	for (i = 0; i<size.value; i++) {
		var login = userlogins.value[i];
		this.users[login] = {};
		this.users[login]["login"] = userlogins.value[i];
		this.users[login]["state"] = userstates.value[i];
		this.users[login]["alias"] = useraliases.value[i];
	}	
};

//---------------------------------------------------------------------
MarrowerController.prototype.getCurrentTaskVO = function() {
	var treeitem = 
		this.task_tree.view.getItemAtIndex(this.task_tree.currentIndex);
	var id = treeitem.firstChild.id;
	return this.tasks[id]["taskVO"];
}

	
//---------------------------------------------------------------------
MarrowerController.prototype.refreshTasks = function() {
	clearTreeSelection(this.task_tree);
	clearList(this.task_tree_list);
	for (i in this.tasks) {
		task = this.tasks[i];
		var item = createItemFromTask(task["taskVO"]);
		task["item"] = item;
		this.task_tree_list.appendChild(item);
	}	
};

MarrowerController.prototype.refreshUsers = function() {
	clearHeadList(this.user_list);
	for (i in this.users) {
		var item = createItemFromUser(
			this.users[i]["login"], 
			this.users[i]["state"], 
			this.users[i]["alias"])
		this.users[i]["item"] = item;
		this.user_list.appendChild(item);
	}
};

//---------------------------------------------------------------------
MarrowerController.prototype.removeTask = function() {
	var task = this.getCurrentTaskVO();

	this.marrowerFacade.removeTask(task.id);

	var treeitem = this.tasks[task.id]["item"];
	this.task_tree_list.removeChild(treeitem);
	delete this.tasks[this.tasks[task.id]];

}

MarrowerController.prototype.editTask = function() {
	var task = this.getCurrentTaskVO();
	openDialog("edit_task.xul", "edittask", "chrome", "edit", this, task);
}

MarrowerController.prototype.editTaskForCreation = function() {
	var newtask = new this.TaskVOClass();
	newtask.id = 0
	newtask.name = "Nombre";
	newtask.description = "Descripcion"
	newtask.duration = "0h",
	newtask.state =  0;
	newtask.completion =  0;
	newtask.priority = 0;
	openDialog("edit_task.xul", "edittask", "chrome", "create", this, newtask);
}

MarrowerController.prototype.updateTask = function(task) {
	this.marrowerFacade.updateTask(task);
//	this.tasks[task.id]["taskVO"] = task;
//	this.refreshTasks();
}

MarrowerController.prototype.createTask = function(newtask) {
	newtask = this.marrowerFacade.createTask(newtask);
	this.tasks[newtask.id] = {};
	this.tasks[newtask.id]["taskVO"] = newtask;
	this.refreshTasks();
}

//---------------------------------------------------------------------
MarrowerController.prototype.showTaskInfo = function() {
	try {
		var task = this.getCurrentTaskVO();
	} catch (e) {
		clearList(this.predepends_list);
		clearList(this.postdepends_list);
		clearList(this.assigned_to_list);
		return;
	}
	var predepends_size = {};
	var predepends = this.marrowerFacade.findPredepends(task.id, predepends_size);
	var postdepends_size = {};
	var postdepends = this.marrowerFacade.findPostdepends(task.id, postdepends_size);
	
	clearList(this.predepends_list);
	for (var i = 0; i< predepends_size.value; i++) {
		var pretask = this.marrowerFacade.findTaskById(predepends[i]);
		var item = createListItem(pretask, ["name"]);
		this.predepends_list.appendChild(item);	
	} 	
	clearList(this.postdepends_list);
	for (i = 0; i< postdepends_size.value; i++) {
		var posttask = this.marrowerFacade.findTaskById(postdepends[i]);
		var item = createListItem(posttask, ["name"]);
		this.postdepends_list.appendChild(item);	
	} 	
	clearList(this.assigned_to_list);
	var assigned_size = {};
	var assigned = task.getAssignedTo(assigned_size);
	for (i = 0; i< assigned_size.value; i++) {
		var userVO = this.getUserInfo(assigned[i]);
		var item = createListItem(userVO, ["alias"]);
		this.assigned_to_list.appendChild(item);			
	} 	
};

MarrowerController.prototype.getUserInfo = function( login ) {
	var userVo = {};
	if (typeof this.users[login]["userVO"] != "undefined") { 
		userVo = this.users[login]["userVO"];
	} else {
		userVo = this.marrowerFacade.getUserInfo(login);
		this.users[login]["userVo"] = userVo;
	}
	return userVo;
}


MarrowerController.prototype.updateUserInfo = function( login ) {
	var userVo = this.getUserInfo(login);
	document.getElementById("user-info-name").value = login;
	document.getElementById("user-info-alias").value = userVo.alias;
	document.getElementById("user-info-firstname").value = userVo.firstname;
	document.getElementById("user-info-lastname").value = userVo.lastname;
	document.getElementById("user-info-email").value = userVo.email;
	document.getElementById("user-info-birthdate").value = userVo.birthdate;
};

//---------------------------------------------------------------------
/** O controlador actuará de observer dos eventos do modelo */
MarrowerController.prototype.QueryInterface = function(iid) {
	if (iid.equals(Components.interfaces.nsIObserver) ||
		iid.equals(Components.interfaces.nsISupportsWeakReference)||
		iid.equals(Components.interfaces.nsISupports)) {
		return this;
	}
	throw Components.results.NS_NOINTERFACE;
};

MarrowerController.prototype.observe = function(subject, topic, data) {
	switch (event) {
	case "marrower-user-online": 
		var userVO = subject.QueryInterface(Components.interfaces.lfIUserVO);
		this.setUser(userVO);
		this.refreshUsers();
		break;
	case "marrower-user-offline":
		var userVO = object.QueryInterface(Components.interfaces.lfIUserVO);
		var login = userVO.login;
		this.users[login] = {};
		this.users[login]["userVO"] = userVO;
		this.users[login]["login"] = login;
		this.users[login]["state"] = Components.interfaces.lfIUserVO.OFFLINE;
		this.users[login]["alias"] = userVO.alias;
		this.refreshUsers();
		alert("se pira:"+login);
		break;
	case "marrower-task-updated":
		task = object.QueryInterface(Components.interfaces.lfITaskVO)
		this.tasks[task.id]["taskVO"] = task;
		this.refreshTasks();
		break;
	
	}
};


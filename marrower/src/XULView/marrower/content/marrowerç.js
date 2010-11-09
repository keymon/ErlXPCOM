




/** 
 * Agrega un item a unha lista a partires  dun obxecto calquera e unha
 * lista de nomes de atributo que serán as celdas do item 
 * @param object obxecto desde o que leremos
 * @param keys lista de claves que obter do obxecto (["key1", "key2"...])
 * @param listId identificador da lista
 */
function addElementsToList( object, keys, listId ) {
	var item = document.createElement('listitem');
	for (i in keys) {
		var child = document.createElement('listcell');
		child.setAttribute('label',object[keys[i]]);
		item.appendChild(child);
	}
    var list=document.getElementById(listId);
	list.appendChild(item);
}


/** Obter todas as tareas e mostralas en pantalla */
function getAllTasks() {
	if (marrowerFacade == null) return;
	
	var enumerator = marrowerFacade.findAllTasks();

	taskArray = {};

	while (enumerator.hasMoreElements()) {
		task = enumerator.getNext().QueryInterface(
			Components.interfaces.lfITaskVO);
		taskArray[task.id] = task;
	}	
	showTasks();
}


/** Obter todas as tareas e mostralas en pantalla */
function getUserTasks( login ) {
	if (marrowerFacade == null) return;
	
	var count = {};
	
	var tasks = marrowerFacade.findTaskByAssigned(login, count);

	taskArray = {};
	var i;
	for (i = 0; i< count.value; i++) {
		task = tasks[i];
		taskArray[task.id] = task;
	}	
	showTasks();
}

function getCurrentTask() {
	var tree = document.getElementById("task-tree");
	var treeitem = tree.view.getItemAtIndex(tree.currentIndex);
	var id = treeitem.firstChild.id;
	return taskArray[id];
}

function removeSelectedTask() {
	var tree = document.getElementById("task-tree");

	var treeitem = tree.view.getItemAtIndex(tree.currentIndex);
	var id = treeitem.firstChild.id;

	var treelist = document.getElementById("task-list-tree");
	treelist.removeChild(treeitem);

	delete taskArray[id];
	
	marrowerFacade.removeTask(id);

}

function showTaskInfo() {
	var task = getCurrentTask();
	var predepends_size = {};
	var predepends = marrowerFacade.findPredepends(task.id, predepends_size);
	var postdepends_size = {};
	var postdepends = marrowerFacade.findPostdepends(task.id, postdepends_size);
	
	clearSimpleListBox("main-predepends");
	var i;
	for (i = 0; i< predepends_size.value; i++) {
		var pretask = marrowerFacade.findTaskById(predepends[i]);
		addElementsToList(pretask, ["name"], "main-predepends");
	} 	
	clearSimpleListBox("main-postdepends");
	for (i = 0; i< postdepends_size.value; i++) {
		var posttask = marrowerFacade.findTaskById(postdepends[i]);
		addElementsToList(posttask, ["name"], "main-postdepends");
	} 	
	clearSimpleListBox("main-assigned-to");
	
	var assigned_size = {};
	var assigned = task.getAssignedTo(assigned_size);
	for (i = 0; i< assigned_size.value; i++) {
		addElementsToList({login : assigned[i]}, ["login"], "main-assigned-to");
	} 	
	
}




/** Obter todos os usuarios e mostralos pantalla */
function getAllUsers() {
	clearListBox("main-userList");
	var size = {};
	var userstates = {};
	var userlogins = {};
	var useraliases = {};
	marrowerFacade.listAllUsers(size, userstates, userlogins, useraliases);
	
	for (i = 0; i<size.value; i++) {
		addUserToUserList(userstates.value[i],
						  userlogins.value[i], 
						  useraliases.value[i]);
	}	
}

/** 
 * Agrega unha tarefa á lista de tarefas 
 */
function addUserToUserList( userstate, userlogin, useralias ) {

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

	document.getElementById("main-userList").appendChild(item);

}

function updateUserInfo( login ) {
	userVo = marrowerFacade.getUserInfo(login);
	
	document.getElementById("user-info-name").
		setAttribute('value', login);
	document.getElementById("user-info-alias").
		setAttribute('value', userVo.alias);
	document.getElementById("user-info-firstname").
		setAttribute('value', userVo.firstname);
	document.getElementById("user-info-lastname").
		setAttribute('value', userVo.lastname);
	document.getElementById("user-info-email").
		setAttribute('value', userVo.email);
	document.getElementById("user-info-address").
		setAttribute('value', userVo.address);
	document.getElementById("user-info-birthdate").
		setAttribute('value', userVo.birthdate);
}


function editTask() {
	task = getCurrentTask();
	openDialog("edit_task.xul", "edittask", "chrome", task, updateTask);
}

function createTask() {
	task = getCurrentTask();
	openDialog("edit_task.xul", "edittask", "chrome", task, updateTask);
}


function updateTask( task ) {
	marrowerFacade.updateTask(task);
	showTasks();
}


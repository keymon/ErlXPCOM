




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


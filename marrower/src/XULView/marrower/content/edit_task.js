var task;
var action;
var controller;

function loadEditTaskDialog() {
	action = window.arguments[0];
	controller = window.arguments[1];
	task = window.arguments[2];
	populateForm();
}

function populateForm() {
	document.getElementById("task-name").
		setAttribute('value', task.name);
	document.getElementById("task-description").
		setAttribute('value', task.description);
	document.getElementById("task-duration").
		setAttribute('value', task.duration);
	document.getElementById("task-completion").
		setAttribute('value', task.completion);
	document.getElementById("task-state").
		selectedIndex = task.state;
	document.getElementById("task-priority").
		selectedIndex = task.priority;
}

function taskEditOk()
{
	task.name = document.getElementById("task-name").value;
	task.description = document.getElementById("task-description").value;
	task.duration = document.getElementById("task-duration").value;
	task.completion = document.getElementById("task-completion").value;
	task.state = document.getElementById("task-state").selectedIndex;
	task.priority = document.getElementById("task-priority").selectedIndex;

	if (action == "edit") {
		controller.updateTask(task);
	} else {
		controller.createTask(task);
	}
	return true;
}

function taskEditCancel()
{
  return true;
}

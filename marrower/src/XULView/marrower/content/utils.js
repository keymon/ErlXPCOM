/** Borrar todos os elementos dunha lista */
function clearList(objList) {
	while(objList.hasChildNodes()) {
		objList.removeChild(objList.lastChild);
	}
}

/** Borrar todos os elementos dunha lista, pero conservando as cabeceiras */
function clearHeadList(objListBox) {
  var headerCount = objListBox.getElementsByTagName('listheader').length;
  var baseRow = headerCount!=0? 2:1;
  while (objListBox.getRowCount() > 0) {
      objListBox.removeItemAt(baseRow);
  }		
}

/** Borrar a selección dun tree */
function clearTreeSelection(tree) {
	tree.view.selection.clearSelection();
}

/** 
 * crea un item para unha lista a partires  dun obxecto calquera e unha
 * lista de nomes de atributo que seran as celdas do item 
 * @param object obxecto desde o que leremos
 * @param keys lista de claves que obter do obxecto (["key1", "key2"...])
 * @param listId identificador da lista
 */
function createListItem( object, keys ) {
	var item = document.createElement('listitem');
	for (i in keys) {
		var child = document.createElement('listcell');
		child.setAttribute('label',object[keys[i]]);
		item.appendChild(child);
	}
	return item;
}



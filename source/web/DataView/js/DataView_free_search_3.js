var newCount = 1;
var rclickSelElementsVar;
var lclickSelElementsVar = new Array();
//���̃��X�g�̉��Ԗڂ̃X�p���l�������Ă��邩�ݒ�
var lListNum = { 
	listTableName: 1 ,
	listItemName: 2 ,
	listJapaneseName: 3 ,
	listId: 4 ,
	listS_point: 5,
	listSize: 6 ,
	listType: 7 ,
	id: 8
};
//�E�̃��X�g�̉��Ԗڂ̃X�p���l�������Ă��邩�ݒ�
var rListNum = { 
	lListTableName: 1 ,
	listItemName: 2 ,
	listJapaneseName: 3 ,
	listId : 4 ,
	listS_point: 5,
	listSize: 6 ,
	listType: 7 ,
	id: 8
};
//�폜�{�^���N���b�N�������̃C�x���g�𒣂�t����
function dispDelButton(){
	var nb = $('#dispDelButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , delClick);
	}
}
//�폜�{�^���̃C�x���g
function delClick(){
	//���b�Z�[�W������
	outputMessage("","errMessage");
	outputMessage("","successMessage");

	var delList = new Array();
	//�����I������Ă��Ȃ���΃��b�Z�[�W��\��
	if(rclickSelElementsVar == undefined){
		errJsCode = "ERRJ0035";
		outputMessage(errJsCode,"errMessage");
		return false;
	}

	//�폜�Ώۍs�ԍ���z��ɒǉ�
	//�i�����폜��z��̂��ߍs�ԍ���z��ɓ����d�l���������݂͎g�p���Ă��Ȃ��j
	var rightList = document.getElementById('rightList');
	for(var i=0;i < rightList.children.length;i++){
		if(rightList.children[i].innerHTML == rclickSelElementsVar.innerHTML){
			delList.push(i);
		}
	}
	
	//�폜�s����납��폜���Ă���
	if(delList.length > 0){
		//�z����~���ɕ��ׂ�
		delList.sort(
			function(a,b){
				if( a < b ) return 1;
				if( a > b ) return -1;
				return 0;
			}
		);
		//��납��폜
		for(var i=0;i < delList.length;i++){
			rightList.removeChild(rightList.children[delList[i]]); 
		}
	}
	//���I����Ԃ֖߂�
	rclickSelElementsVar = undefined;
	//������Ɗ��ŐF��ς���
	(function(){
		var targsList = document.getElementById('rightList');
		var elems = getElementsByClassName( targsList, 'listElem');
		var oddFlg = -1;
		for( var k=0;k < elems.length;k++ ){
			if(!elems[k].getAttribute('class').match('hidden')){
				removeClass(elems[k] , 'odd');
				
				oddFlg *= -1;
				if(oddFlg == -1){
					elems[k].className += ' odd';
				}
			}
		}
	})();
}

//�ǉ��{�^�������C�x���g�𒣂�t����
function dispAddButton(){
	var nb = $('#dispAddButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , addClick);
	}
}

//�ǉ��{�^�������C�x���g
//��ʍ����I���s�̏�����ʉE���̃O���[�v���ɓ����
function addClick(){
	//���b�Z�[�W������
	outputMessage("","errMessage");
	outputMessage("","successMessage");
	
	//���̃��X�g�������I������Ă��Ȃ���΃��b�Z�[�W��\��
	if(typeof lclickSelElementsVar == 'undefined' || lclickSelElementsVar.length == 0){
		errJsCode = "ERRJ0036";
		outputMessage(errJsCode,"errMessage");
		return false;
	}
	
	//�ǉ��Ώۂ̃��X�g���擾
	var disp = document.getElementById("rightList");

	//���ɓo�^����Ă��鍀�ڂł���΃��b�Z�[�W��\��
	for(var i=0;i < disp.children.length;i++){
		//�����̓N���b�N
		var targetIdhold = lclickSelElementsVar.children[lListNum['listId']].innerHTML;
		var targetIdTemp = disp.children[i].children[0].innerHTML ;
		var counter = 0;
		for(var j=0;j < disp.children[i].children.length;j++){
			if(disp.children[i].children[j].nodeType == 1){
				if(counter == lListNum['listId']){
					targetIdTemp = disp.children[i].children[j].innerHTML;
				}
				counter++;
			}
		}
		
		if(!disp.children[i].getAttribute('class').match('hidden')){
			if(targetIdhold == targetIdTemp){
				errJsCode = "ERRJ0051";
				outputMessage(errJsCode,"errMessage");
				return false;
			}
		}
	}
	
	//�E�̃��X�g�ɏ���ǉ�
	var row1 = document.createElement("div");
	s0 = document.createElement('span');
	s0.className = 'hidden bgChItems';
	s0.innerHTML = '';
	row1.appendChild(s0);
	
	lselectTableName = document.getElementById('lTableName');
	s1 = document.createElement('span');
	s1.className = 'lListTableName bgChItems';
	s1.title     = '�^ : ' + lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	s1.innerHTML = lselectTableName.value;
	row1.appendChild(s1);
	
	s2 = document.createElement('span');
	s2.className = 'listItemName bgChItems';
	s2.title     = '�^ : ' + lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	s2.innerHTML = lclickSelElementsVar.children[lListNum['listItemName']].innerHTML;
	row1.appendChild(s2);

	s3 = document.createElement('span');
	s3.className = 'listJapaneseName bgChItems';
	s3.title     = '�^ : ' + lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	s3.innerHTML = lclickSelElementsVar.children[lListNum['listJapaneseName']].innerHTML;
	row1.appendChild(s3);

	s4 = document.createElement('span');
	s4.className = 'hidden listId bgChItems';
	s4.innerHTML = lclickSelElementsVar.children[lListNum['listId']].innerHTML;
	row1.appendChild(s4);
	
	s5 = document.createElement('span');
	s5.className = 'listS_point right bgChItems';
	s5.title     = '�^ : ' + lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	s5.innerHTML = lclickSelElementsVar.children[lListNum['listS_point']].innerHTML;
	row1.appendChild(s5);
	
	s6 = document.createElement('span');
	s6.className = 'listSize right bgChItems';
	s6.title     = '�^ : ' + lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	s6.innerHTML = lclickSelElementsVar.children[lListNum['listSize']].innerHTML;
	row1.appendChild(s6);
	
	s7 = document.createElement('span');
	s7.className = 'hidden listType bgChItems';
	s7.innerHTML = lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	row1.appendChild(s7);
	
	s8 = document.createElement('span');
	s8.className = 'hidden rightChildLast id bgChItems';
	s8.innerHTML = lclickSelElementsVar.children[lListNum['id']].innerHTML;
	row1.appendChild(s8);
	
	row1.className += "listElem insert add";

	//�����ɒǉ����邽�߂̏���
	var rightChildLast = $(".rightChildLast:last");
	if(rightChildLast.length==0){
		disp.insertBefore(row1,(disp.hasChildNodes()) ? disp.childNodes[0] : null);
	}else{
		var parentDiv = rightChildLast[0].parentNode;
		disp.insertBefore(row1,parentDiv.nextSibling);
	}
	
	//�s���Ƃ̐F�ς��p�N���X����ǉ�
	(function(){
		var targsList = document.getElementById('rightList');
		var elems = getElementsByClassName( targsList, 'listElem');
		var oddFlg = -1;
		for( var k=0;k < elems.length;k++ ){
			if(!elems[k].getAttribute('class').match('hidden')){
				removeClass(elems[k] , 'odd');
				
				oddFlg *= -1;
				if(oddFlg == -1){
					elems[k].className += ' odd';
				}
			}
		}
	})();
	//�ǉ������s�ɃN���b�N�C�x���g�\��t��
	addEvent("click",row1,setClickElems);
}
addEvent("load",window,dispDelButton);
addEvent("load",window,dispAddButton);


//���փ{�^���������̃C�x���g�𒣂�t����
var nextButton = function () {
	var nb = $('.nextButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , next);
	}
}

//���փ{�^���������̃C�x���g
//�t�H�[�����쐬����POST���M����
//�I������Ă���O���[�v�ƁA����ɂɒǉ��E�폜���ꂽ���[�U���𑗂�
var next = function () {

	//���b�Z�[�W������
	outputMessage("","errMessage");
	outputMessage("","successMessage");

	//�t�H�[���쐬
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./DataView_free_search_4.php');
	
	//�I���e�[�u����
	var selectedTables = document.getElementsByName('selectedTables[]');
	for(var i=0;i < selectedTables.length ; i++){
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name',selectedTables[i].name);
		sendInp.setAttribute('value',selectedTables[i].value);
		sendForm.appendChild( sendInp );
	}
	
	//��������
	var selectedBonds = document.getElementsByName('selectedBonds[]');
	for(var i=0;i < selectedBonds.length ; i++){
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name',selectedBonds[i].name);
		sendInp.setAttribute('value',selectedBonds[i].value);
		sendForm.appendChild( sendInp );
	}
	
	//�\�����ڂ̃��X�g
	var sendSelect = document.createElement('select');
	sendSelect.setAttribute('name','selectedItemNames[]');
	sendSelect.multiple = true;
	sendForm.appendChild( sendSelect );
	dispCount = 0;
	var rightList = document.getElementById('rightList');
	for(var i=0;i < rightList.children.length ; i++){
		var sendOption = document.createElement('option');
		var sendText = '';
		sendText += trim(rightList.children[i].children[rListNum['lListTableName']].innerHTML);
		sendText += "~" + trim(rightList.children[i].children[rListNum['listItemName']].innerHTML);
		if(trim(rightList.children[i].children[rListNum['listJapaneseName']].innerHTML) == "&nbsp;"){
			sendText += "~";
		}else{
			sendText += "~" + trim(rightList.children[i].children[rListNum['listJapaneseName']].innerHTML);
		}
		sendText += "~" + trim(rightList.children[i].children[rListNum['listId']].innerHTML);
		sendText += "~" + trim(rightList.children[i].children[rListNum['listS_point']].innerHTML);
		sendText += "~" + trim(rightList.children[i].children[rListNum['listSize']].innerHTML);
		sendText += "~" + trim(rightList.children[i].children[rListNum['listType']].innerHTML);
		//�e�[�u�����ǉ�
		
		sendOption.setAttribute('value',sendText);
		sendSelect.appendChild(sendOption);
		sendOption.selected = true;
		
		dispCount++;
	}

	//�\�����ڂ��P�ȏ�Ȃ��ƃG���[
	if(dispCount < 1){
		errJsCode = "ERRJ0037";
		outputMessage(errJsCode,"errMessage");
		$(sendForm).remove();//�t�H�[�����c���Ă��܂����ߍ폜
		return false;
	}

	//�t�H�[�����s
	document.body.appendChild( sendForm );
	sendForm.submit();
	return false;
}
addEvent('load',window,nextButton);

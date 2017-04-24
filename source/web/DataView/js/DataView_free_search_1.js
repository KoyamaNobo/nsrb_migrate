//���̃��X�g�̉��Ԗڂ̃X�p���l�������Ă��邩�ݒ�
var lListNum = { 
	listTableName: 1 ,
	listId: 2 ,
	id: 3
};
//�E�̃��X�g�̉��Ԗڂ̃X�p���l�������Ă��邩�ݒ�
var rListNum = { 
	listTableName: 1 ,
	listId: 2 ,
	id: 3
};

//�폜�{�^���N���b�N�������̃C�x���g�𒣂�t����
function delButton(){
	var nb = $('#tableDelButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , delClick);
	}
}

//�E�I���s�폜�N���b�N���̃C�x���g
function delClick(){
	var viewElem = new Array();
	var delList = new Array();
	
	//���b�Z�[�W������
	outputMessage("","errMessage");
	outputMessage("","successMessage");
	
	//�����I������Ă��Ȃ���΃��b�Z�[�W��\������return
	if(rclickSelElementsVar == undefined){
		errJsCode = "ERRJ0027";
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

//�E�֒ǉ��{�^�������C�x���g�𒣂�t����
function addButton(){
	var nb = $('#tableAddButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , addClick);
	}
}

//�E�֒ǉ��{�^�������C�x���g
//��ʍ����I���s�̏�����ʉE���̃O���[�v���ɓ����
function addClick(){
	//���b�Z�[�W������
	outputMessage("","errMessage");
	outputMessage("","successMessage");

	//���̃��X�g����s���I������Ă��Ȃ����return
	if(lclickSelElementsVar == undefined){
		errJsCode = "ERRJ0025";
		outputMessage(errJsCode,"errMessage");
		return false;
	}
	
	//�E�̑S���X�g�擾
	var disp = document.getElementById("rightList");
	
	//�o�^�������e�[�u���Ɗ��ɓo�^����Ă���e�[�u�����r
	for(var i=0;i < disp.children.length;i++){
		var targetIdhold = lclickSelElementsVar.children[lListNum['id']].innerHTML;
		var targetIdTemp = disp.children[i].children[rListNum['id']].innerHTML;
		
		if(targetIdhold == targetIdTemp){
			//���ɓo�^�ς݂̏ꍇ���b�Z�[�W��\��
			errJsCode = "ERRJ0026";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
	}
	
	//�o�^����v�f���쐬
	var row1 = document.createElement("div");                     //�ǉ��p��div�v�f�쐬
	row1.innerHTML = lclickSelElementsVar.innerHTML;
	row1.children[lListNum['id']].className += " rightListLast "; //�ŏI�s�𔻒肷�邽��Last�Ƃ���
	row1.className += "listElem insert add";

	//�����̍s���擾
	var rightListLast = $(".rightListLast:last");
	if(rightListLast.length==0){
		//�E�̃��X�g�ɉ����o�^����Ă��Ȃ��Ƃ�
		//�擪�֒ǉ�
		disp.insertBefore(row1,(disp.hasChildNodes()) ? disp.childNodes[0] : null);
	}else{
		//�E�̃��X�g�Ɋ��ɉ����o�^����Ă��鎞
		var parentDiv = rightListLast[0].parentNode;
		//�����֒ǉ�
		disp.insertBefore(row1,parentDiv.nextSibling);
	}
	
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
	//�ǉ������s�֑I���i�N���b�N�j�����Ƃ��̃C�x���g�𒣂�t����
	addEvent("click",row1,setClickElems);
}

addEvent("load",window,delButton);
addEvent("load",window,addButton);



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
	var selectCount = 0;
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./DataView_free_search_2.php');
	
	var sendSelect = document.createElement('select');
	sendSelect.setAttribute('name','selectedTables[]');
	sendSelect.multiple = true;
	sendForm.appendChild( sendSelect );
	
	var rightList = document.getElementById('rightList');
	for(var i=0;i < rightList.children.length ; i++){
		var sendOption = document.createElement('option');
		var sendText = '';
		//�e�[�u�����ǉ�
		sendText = rightList.children[i].children[lListNum['id']].innerHTML;
		sendOption.setAttribute('value',sendText);
		sendSelect.appendChild(sendOption);
		sendOption.selected = true;
		selectCount++;
	}
	
	//�I�������e�[�u�����Q�ȏ�Ȃ��ƃG���[
	if(selectCount < 2){
		errJsCode = "ERRJ0028";
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




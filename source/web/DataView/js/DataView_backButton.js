//�߂�{�^���Ƀq�X�g���[�o�b�N�̃C�x���g�𒣂�t����
function clickBack() {
	var targetElements = $('.backButton');
	for(var i = 0; i < targetElements.length ;i++){
		addEvent("click", targetElements[i] , back);
	}
}

//�߂�{�^���������̃C�x���g
//�t�H�[�����쐬����POST���M����
var back = function () {
	
	//��ʂ̖߂��擾(�߂�{�^���̂Q�Ԗڂ̃N���X���Ɋi�[���Ă���ׁj
	var backButton = document.getElementsByName('backButton');
	bBclassName = this.className.split(" ")
	
	//�t�H�[���쐬
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./' + bBclassName[1] + ".php");
	
	//�I���e�[�u��
	var selectedTables = document.getElementsByName('selectedTables[]');
	if(selectedTables.length > 0){
		for(var i=0;i < selectedTables.length ; i++){
			var sendInp = document.createElement('input');
			sendInp.setAttribute('name',selectedTables[i].name);
			sendInp.setAttribute('value',selectedTables[i].value);
			sendForm.appendChild( sendInp );
		}
	}
	
	//��������
	var selectedBonds = document.getElementsByName('selectedBonds[]');
	if(selectedBonds.length > 0){
		for(var i=0;i < selectedBonds.length ; i++){
			var sendInp = document.createElement('input');
			sendInp.setAttribute('name',selectedBonds[i].name);
			sendInp.setAttribute('value',selectedBonds[i].value);
			sendForm.appendChild( sendInp );
		}
	}
	
	//�\������
	var selectedItemNames = document.getElementsByName('selectedItemNames[]');
	if(selectedItemNames.length > 0){
		for(var i=0;i < selectedItemNames.length ; i++){
			var sendInp = document.createElement('input');
			sendInp.setAttribute('name',selectedItemNames[i].name);
			sendInp.setAttribute('value',selectedItemNames[i].value);
			sendForm.appendChild( sendInp );
		}
	}
	
	//���o�����̃��X�g
	var selectedFilters = document.getElementsByName('selectedFilters[]');
	if(selectedFilters.length > 0){
		for(var i=0;i < selectedFilters.length ; i++){
			var sendInp = document.createElement('input');
			sendInp.setAttribute('name',selectedFilters[i].name);
			sendInp.setAttribute('value',selectedFilters[i].value);
			sendForm.appendChild( sendInp );
		}
	}
	
	//���בւ��̃��X�g
	var selectedSorts = document.getElementsByName('selectedSorts[]');
	if(selectedSorts.length > 0){
		for(var i=0;i < selectedSorts.length ; i++){
			var sendInp = document.createElement('input');
			sendInp.setAttribute('name',selectedSorts[i].name);
			sendInp.setAttribute('value',selectedSorts[i].value);
			sendForm.appendChild( sendInp );
		}
	}
	
	//�t�H�[�����s
	document.body.appendChild( sendForm );
	sendForm.submit();
	return false;
}




addEvent('load',window,clickBack);
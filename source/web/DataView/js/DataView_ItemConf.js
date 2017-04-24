var newCount = 1;
var sendFlg = false;
var errJsCode = '';

//�X�V�{�^�����������Ƃ��̏���
function itemUpdate() {
	if(!sendFlg){

		//���b�Z�[�W������
		errJsCode = "";
		outputMessage("","errMessageModal");
		outputMessage("","successMessage");
	
		//���̓`�F�b�N
		if(!input_check_item_conf()){
			//errJsCode��input_check_item_conf�ɂĐݒ肳��Ă���
			outputMessage(errJsCode,"errMessageModal");
			return false;
		}
		
		//�����I������Ă��Ȃ���΃��b�Z�[�W��\��
		if(lclickSelElementsVar == undefined){
				errJsCode = "ERRJ0022";
				outputMessage(errJsCode,"errMessageModal");
				return false;
		}

		//�t�H�[���쐬
		var sendForm = document.createElement('form');
		sendForm.className = "hidden";
		sendForm.setAttribute('method','post');
		sendForm.setAttribute('action','./DataView_item_conf.php');
		
		//�e�[�u�����擾
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name','TableName');
		sendInp.setAttribute('value',lclickSelElementsVar.children[1].innerHTML);
		sendForm.appendChild( sendInp );
		
		//���ږ��擾
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name','ItemName');
		sendInp.setAttribute('value',lclickSelElementsVar.children[2].innerHTML);
		sendForm.appendChild( sendInp );
/*
		//�a���擾
		var Japanese_Name = $('#Japanese_Name');
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name','JapaneseName');
		sendInp.setAttribute('value',Japanese_Name[0].value.replace(/\s+/g, "")); //���p�X�y�[�X���������Ă���
		sendForm.appendChild( sendInp );

		//�\���E��\���擾
		var tmp;
		var NonDisp_Flg = $('#NonDisp_Flg');
		if($("#NonDisp_Flg:checked").val()) {
			tmp = "1";
		}else{
			tmp = "0";
		}*/
		
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name','JapaneseName');
		sendInp.setAttribute('value',document.forms[(document.forms.length)-1].Japanese_Name.value.replace(/\s+/g, "")); //���p�X�y�[�X���������Ă���
		sendForm.appendChild( sendInp );
		
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name','NonDisp_Flg');
		sendInp.setAttribute('value',document.forms[(document.forms.length)-1].NonDisp_Flg.value);
		sendForm.appendChild( sendInp );
		
		//�J�n�ʒu�擾
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name','S_point');
		sendInp.setAttribute('value',lclickSelElementsVar.children[5].innerHTML);
		sendForm.appendChild( sendInp );
		
		//�T�C�Y�擾
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name','Size');
		sendInp.setAttribute('value',lclickSelElementsVar.children[6].innerHTML);
		sendForm.appendChild( sendInp );
		
		//�^�擾
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name','Type');
		if(lclickSelElementsVar.children[7].innerHTML == '&nbsp;'){
			sendInp.setAttribute('value','');
		}else{
			sendInp.setAttribute('value',lclickSelElementsVar.children[7].innerHTML);
		}
		sendForm.appendChild( sendInp );
		
		//�t�H�[�����s
		document.body.appendChild( sendForm );
		sendFlg = true;
		sendForm.submit();
	}
	return false;

}

//�X�V�{�^���������̃C�x���g�𒣂�t����
var itemUpdateButton = function () {
	var nb = $('.itemUpdateButton');
	var ii = 0;
	for(ii = 0 ; ii < nb.length;ii++){
		addEvent("click", nb[ii] , itemUpdate);
	}
}
addEvent('load',window,itemUpdateButton);




//�a���o�^����ۂ̓��̓`�F�b�N
function input_check_item_conf() {

	//�a���擾
	Japanese_Name = document.getElementById("Japanese_Name").value;
	Japanese_Name = Japanese_Name.replace(/\s+/g, ""); //���p�X�y�[�X����������

	//�������`�F�b�N
	if(!input_check_length(Japanese_Name,0,20)){ //20�����܂łƂ���i�������\����W�����ȏ�ɂȂ�Ɨ����E�E�E�j
		errJsCode = "ERRJ0020";
		return false;
	}
	//�֎~�����`�F�b�N
	if(!input_check_symbol(Japanese_Name)){ 
		errJsCode = "ERRJ0021";
		return false;
	}
	return true;
}


var newCount = 1;
var nbsp = String.fromCharCode( 160 );

//���X�g�̉��Ԗڂ̃X�p���ɒl�������Ă��邩�ݒ�
var lListNum = { 
	listTableName: 1 ,
	listItemName: 2 ,
	listJapaneseName : 3 ,
	listId: 4 ,
	listS_point: 5,
	listSize: 6 ,
	listType: 7 ,
	id: 8
};

//���o�����ǉ��{�^���������̃C�x���g�𒣂�t����
var filterButton = function () {
	var nb = $('.filterButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , filter);
	}
}
//���o�����ǉ��{�^���������̃C�x���g
//�t�H�[�����쐬����POST���M����
//�I������Ă��錋���e�[�u���A���������A�\�����ځA���ݒǉ�����Ă��钊�o�����𑗂�
var filter = function () {

	var value = document.getElementById('value');//�l�擾
	var operator = document.getElementById('operator');//���Z�q�擾

	//���b�Z�[�W������
	outputMessage("","errMessage");
	outputMessage("","successMessage");
	
	//���ڂ̃��X�g����s���I������Ă��Ȃ���΃G���[�\��
	if(lclickSelElementsVar == undefined){
		errJsCode = "ERRJ0038";
		outputMessage(errJsCode,"errMessage");
		return false;
	}
	//���Z�q�������Ă��Ȃ��Ƃ�
	if(operator.value == ""){
		errJsCode = "ERRJ0039";
		outputMessage(errJsCode,"errMessage");
		return false;
	}
	
	
	//�l�����͂���Ă��Ȃ��Ƃ�
	if(trim(value.value) == ""){
		errJsCode = "ERRJ0040";
		outputMessage(errJsCode,"errMessage");
		return false;
	}
	
	//�l�̓��͒l�ɂ���
	if(lclickSelElementsVar.children[lListNum['listType']].innerHTML == "9" || lclickSelElementsVar.children[lListNum['listType']].innerHTML == "99"){
		//���l���ǂ���
		if(!input_check_Numeric(trim(value.value))){
			errJsCode = "ERRJ0041";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
		//���������������ǂ���
		if(!input_check_length(trim(value.value),0,lclickSelElementsVar.children[lListNum['listSize']].innerHTML)){
			errJsCode = "ERRJ0042";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
	}else if (lclickSelElementsVar.children[lListNum['listType']].innerHTML == "C9"){
		//���l���ǂ���
		if(!input_check_Numeric(trim(value.value))){
			errJsCode = "ERRJ0041";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
		
		//�����Z�o
		max = parseInt(lclickSelElementsVar.children[lListNum['listSize']].innerHTML);
		max = (max * 2) - 1;
		
		//���������������ǂ���
		if(!input_check_length(trim(value.value),0,max)){
			errJsCode = "ERRJ0042";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
	}else if (lclickSelElementsVar.children[lListNum['listType']].innerHTML == "S9"){
		//���l���ǂ����i�}�C�i�X���j
		if(!input_check_Numeric_Minus(trim(value.value))){
			errJsCode = "ERRJ0043";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
		
		//�}�C�i�X�̏ꍇ�}�C�i�X�������i�����v�Z�̂��߁j
		var tmp = '';
		if(trim(value.value).substr(0,1) == "-"){
			tmp = trim(value.value).substr(1);
		}else{
			tmp = trim(value.value)
		}
		
		//���������������ǂ���
		if(!input_check_length(tmp,0,lclickSelElementsVar.children[lListNum['listSize']].innerHTML)){
			errJsCode = "ERRJ0042";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
	}else if (lclickSelElementsVar.children[lListNum['listType']].innerHTML == "SC9"){
		//���l���ǂ����i�}�C�i�X���j
		if(!input_check_Numeric_Minus(trim(value.value))){
			errJsCode = "ERRJ0043";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
		
		//�}�C�i�X�̏ꍇ�}�C�i�X�������i�����v�Z�̂��߁j
		var tmp = '';
		if(trim(value.value).substr(0,1) == "-"){
			tmp = trim(value.value).substr(1);
		}else{
			tmp = trim(value.value)
		}
		
		//�����Z�o
		max = parseInt(lclickSelElementsVar.children[lListNum['listSize']].innerHTML);
		max = (max * 2) - 1;
		//���������������ǂ���
		if(!input_check_length(tmp,0,max)){
			errJsCode = "ERRJ0042";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
	}else if (lclickSelElementsVar.children[lListNum['listType']].innerHTML == "N"){
		//���̍��ځi�Q�o�C�g�����j�ɑ΂��Ă͌����͋����Ȃ�
		errJsCode = "ERRJ0044";
		outputMessage(errJsCode,"errMessage");
		return false;
	}else if (lclickSelElementsVar.children[lListNum['listType']].innerHTML == "X" ||
	          lclickSelElementsVar.children[lListNum['listType']].innerHTML == ""){
		//���p�ȊO�s��
		if(!input_check_Alphanumeric_ex(trim(value.value))){
			errJsCode = "ERRJ0045";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
		//�֎~������͕s��
		if(!input_check_symbol(trim(value.value))){
			errJsCode = "ERRJ0046";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
	}

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
	
	//�\������
	var selectedItemNames = document.getElementsByName('selectedItemNames[]');
	for(var i=0;i < selectedItemNames.length ; i++){
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name',selectedItemNames[i].name);
		sendInp.setAttribute('value',selectedItemNames[i].value);
		sendForm.appendChild( sendInp );
	}
	
	//���o�����̊����Őݒ肳��Ă��郊�X�g
	var selectedFilters = document.getElementsByName('selectedFilters[]');
	for(var i=0;i < selectedFilters.length ; i++){
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name',selectedFilters[i].name);
		sendInp.setAttribute('value',selectedFilters[i].value);
		sendForm.appendChild( sendInp );
	}
	
	
	
	//���o�����̉��̃��X�g
	var sendInp = document.createElement('input');
	var sendText = "";
	var value = document.getElementById('value');
	var operator = document.getElementById('operator');
	var andOr1 = document.getElementById('andOr1');
	var andOr2 = document.getElementById('andOr2');
	
	sendInp.setAttribute('name','selectedFilter[]');
	if(andOr1.checked){
		sendText += andOr1.value;
	}else{
		sendText += andOr2.value;
	}
	
	
	sendText += "~" + trim(lclickSelElementsVar.children[lListNum['listTableName']].innerHTML);
	sendText += "~" + trim(lclickSelElementsVar.children[lListNum['listItemName']].innerHTML);
	if(trim(lclickSelElementsVar.children[lListNum['listJapaneseName']].innerHTML) == "&nbsp;"){
		sendText += "~";
	}else{
		sendText += "~" + trim(lclickSelElementsVar.children[lListNum['listJapaneseName']].innerHTML);
	}
	sendText += "~" + trim(lclickSelElementsVar.children[lListNum['listS_point']].innerHTML);
	sendText += "~" + trim(lclickSelElementsVar.children[lListNum['listSize']].innerHTML);
	if(trim(lclickSelElementsVar.children[lListNum['listType']].innerHTML) == "&nbsp;"){
		sendText += "~";
	}else{
		sendText += "~" + trim(lclickSelElementsVar.children[lListNum['listType']].innerHTML);
	}
	sendText += "~" + operator.value;
	sendText += "~" + trim(value.value);
	sendText += "~" + trim(lclickSelElementsVar.children[lListNum['id']].innerHTML);
	sendInp.setAttribute('value',sendText);
	sendForm.appendChild( sendInp );
	
	
	
	
	//�t�H�[�����s
	document.body.appendChild( sendForm );
	sendForm.submit();
	return false;
}
addEvent('load',window,filterButton);


//addEvent("load",window, function(){
//							return setHigerHeight('leftID', 'rightSelectID');
//						});

//addEvent("load",window, function(){
//							return setHeight('lcontents','contents' ,'ccontents');
//						});


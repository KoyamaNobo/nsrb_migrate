var newCount = 1;
var nbsp = String.fromCharCode( 160 );

var rclickSelElementsVar;

//�폜�{�^���N���b�N�������̃C�x���g�𒣂�t����
function filterDelButton(){
	var nb = $('#filterDelButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , delClick);
	}
}

//��I���s�폜�N���b�N���̃C�x���g�͂��
function delClick(){
	var delList = new Array();
	
	//���b�Z�[�W������
	outputMessage("","errMessage");
	outputMessage("","successMessage");
	
	//�����I������Ă��Ȃ���΃��b�Z�[�W��\������return
	if(rclickSelElementsVar == undefined){
		errJsCode = "ERRJ0047";
		outputMessage(errJsCode,"errMessage");
		return false;
	}

	//�폜�Ώۍs�ԍ���z��ɒǉ�
	//�i�����폜��z��̂��ߍs�ԍ���z��ɓ����d�l���������݂͎g�p���Ă��Ȃ��j
	var filterList = document.getElementById('filterList');
	for(var i=0;i < filterList.children.length;i++){
		if(filterList.children[i].innerHTML == rclickSelElementsVar.innerHTML){
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
			filterList.removeChild(filterList.children[delList[i]]);
		}
	}
	//���I����Ԃ֖߂�
	rclickSelElementsVar = undefined;
	//������Ɗ��ŐF��ς���
	(function(){
		var targsList = document.getElementById('filterList');
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
addEvent("load",window,filterDelButton);

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
	//�t�H�[���쐬
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./DataView_free_search_6.php');
	
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
	
	//���o�����̃��X�g
	var sendSelect = document.createElement('select');
	sendSelect.setAttribute('name','selectedFilters[]');
	sendSelect.multiple = true;
	sendForm.appendChild( sendSelect );
	var filterList = document.getElementById('filterList');
	for(var i=0;i < filterList.children.length ; i++){
		var sendOption = document.createElement('option');
		var sendText = '';
		//�e�[�u�����ǉ�
		sendText = trim(filterList.children[i].children[10].innerHTML);
		sendText = sendText.replace( nbsp, " " );
		sendOption.setAttribute('value',sendText);
		sendSelect.appendChild(sendOption);
		sendOption.selected = true;
	}

	//�t�H�[�����s
	document.body.appendChild( sendForm );
	sendForm.submit();
	return false;
}
addEvent('load',window,nextButton);


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
	//�t�H�[���쐬
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./DataView_free_search_5.php');
	
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
	
	//���o�����̃��X�g
	var sendSelect = document.createElement('select');
	sendSelect.setAttribute('name','selectedFilters[]');
	sendSelect.multiple = true;
	sendForm.appendChild( sendSelect );
	var filterList = document.getElementById('filterList');
	for(var i=0;i < filterList.children.length ; i++){
		var sendOption = document.createElement('option');
		var sendText = '';
		var counter = 0;
		//�e�[�u�����ǉ�
		sendText = trim(filterList.children[i].children[10].innerHTML);
		sendText = sendText.replace( nbsp, " " );
		sendOption.setAttribute('value',sendText);
		sendSelect.appendChild(sendOption);
		sendOption.selected = true;
	}

	//�t�H�[�����s
	document.body.appendChild( sendForm );
	sendForm.submit();
	return false;
}
addEvent('load',window,filterButton);



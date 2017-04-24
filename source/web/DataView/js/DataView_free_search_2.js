var newCount = 1;

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
	listTableName: 1 ,
	listItemName: 2 ,
	listJapaneseName: 3 ,
	listId: 4 ,
	listS_point: 5,
	listSize: 6 ,
	listType: 7 ,
	id: 8
};

var bListNum = { 
	lListTableName: 1 ,
	lListItemName: 2 ,
	lListJapaneseName: 3 ,
	lListS_point: 4 ,
	lListSize: 5,
	lListType: 6 ,
	listBond: 7 ,
	rListTableName: 8,
	rListItemName: 9,
	rListJapaneseName: 10,
	rListS_point: 11,
	rListSize: 12,
	rListType: 13,
	key: 14,
	id: 15
};

//�폜�{�^���N���b�N�������̃C�x���g�𒣂�t����
function bondDelButton(){
	var nb = $('#bondDelButton');
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
	if(bclickSelElementsVar == undefined){
		errJsCode = "ERRJ0029";
		outputMessage(errJsCode,"errMessage");
		return false;
	}

	//�폜�Ώۍs�ԍ���z��ɒǉ�
	//�i�����폜��z��̂��ߍs�ԍ���z��ɓ����d�l���������݂͎g�p���Ă��Ȃ��j
	var bondList = document.getElementById('bondList');
	for(var i=0;i < bondList.children.length;i++){
		if(bondList.children[i].innerHTML == bclickSelElementsVar.innerHTML){
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
			bondList.removeChild(bondList.children[delList[i]]);
		}
	}
	//���I����Ԃ֖߂�
	bclickSelElementsVar = undefined;
	//�s���Ƃ̐F��
	(function(){
		var targsList = document.getElementById('bondList');
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


//�����{�^�������C�x���g�𒣂�t����
function bondAddButton(){
	var nb = $('#bondAddButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , addClick);
	}
}

//�����{�^�������C�x���g
//��ʍ����I���s�̏�����ʉE���̃O���[�v���ɓ����
function addClick(){

	//���b�Z�[�W������
	outputMessage("","errMessage");
	outputMessage("","successMessage");

	//�������ʃ��X�g����s���I������Ă��Ȃ����return
	if(lclickSelElementsVar == undefined || rclickSelElementsVar == undefined ){
		errJsCode = "ERRJ0030";
		outputMessage(errJsCode,"errMessage");
		return false;
	}
	
	//���E�̃e�[�u�����擾
	var lselectTableName = document.getElementById('lTableName');
	var rselectTableName = document.getElementById('rTableName');
	//�ǉ��Ώۂ̃��X�g���擾
	var bondList = document.getElementById("bondList");
	
	//�����e�[�u�����m�̌����͕s��
	if(lselectTableName.value == rselectTableName.value){
		errJsCode = "ERRJ0031";
		outputMessage(errJsCode,"errMessage");
		return false;
	}
	
	//�E�̃e�[�u���ƍ��̃e�[�u���̑g�ݍ��킹�����Ɍ��������̒��ɂ���ƃG���[
	for(var i=0;i < bondList.children.length;i++){
		if((lselectTableName.value == bondList.children[i].children[bListNum['lListTableName']].innerHTML &&
		    rselectTableName.value == bondList.children[i].children[bListNum['rListTableName']].innerHTML ) ||
		   (lselectTableName.value == bondList.children[i].children[bListNum['rListTableName']].innerHTML && 
		    rselectTableName.value == bondList.children[i].children[bListNum['lListTableName']].innerHTML )){
				errJsCode = "ERRJ0032";
				outputMessage(errJsCode,"errMessage");
				return false;
		}
	}
	
	//���ꂩ��o�^����e�[�u���P�ƃe�[�u���Q�̂ǂ��炩�������ɓo�^���Ă��錋���������̃e�[�u���ɑ��݂��Ă��邱��
	//��jA join B , C join D �̗l�Ƀe�[�u�����A������Ă��Ȃ��̂ŃG���[�Ƃ���
	var tableArray = new Array();
	for(var i=0;i < bondList.children.length;i++){
		tableArray.push(bondList.children[i].children[bListNum['lListTableName']].innerHTML);
		tableArray.push(bondList.children[i].children[bListNum['rListTableName']].innerHTML);
	}
	if( tableArray.length != 0 && tableArray.indexOf(lselectTableName.value) < 0 && tableArray.indexOf(rselectTableName.value) < 0 ){
		errJsCode = "ERRJ0033";
		outputMessage(errJsCode,"errMessage");
		return false;
	}
	
	
	//���������̃��X�g�Ƀe�[�u���P���ǉ�
	var row1 = document.createElement("div");
	s0 = document.createElement('span');
	s0.className = 'hidden bgChItems';
	s0.innerHTML = '';
	row1.appendChild(s0);
	
	s1 = document.createElement('span');
	s1.className = 'lListTableName bgChItems';
	s1.title     = '�J�n�ʒu : ' + lclickSelElementsVar.children[lListNum['listS_point']].innerHTML + '\n�T�C�Y : ' + lclickSelElementsVar.children[lListNum['listSize']].innerHTML + '\n�^ : ' + lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	s1.innerHTML = lselectTableName.value;
	row1.appendChild(s1);
	
	s2 = document.createElement('span');
	s2.className = 'lListItemName bgChItems';
	s2.title     = '�J�n�ʒu : ' + lclickSelElementsVar.children[lListNum['listS_point']].innerHTML + '\n�T�C�Y : ' + lclickSelElementsVar.children[lListNum['listSize']].innerHTML + '\n�^ : ' + lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	s2.innerHTML = lclickSelElementsVar.children[lListNum['listItemName']].innerHTML;
	row1.appendChild(s2);
	
	s3 = document.createElement('span');
	s3.className = 'hidden lListJapaneseName bgChItems';
	s3.innerHTML = lclickSelElementsVar.children[lListNum['listJapaneseName']].innerHTML;
	row1.appendChild(s3);
	
	s4 = document.createElement('span');
	s4.className = 'hidden lListS_point bgChItems';
	s4.innerHTML = lclickSelElementsVar.children[lListNum['listS_point']].innerHTML;
	row1.appendChild(s4);
	s5 = document.createElement('span');
	s5.className = 'hidden lListSize bgChItems';
	s5.innerHTML = lclickSelElementsVar.children[lListNum['listSize']].innerHTML;
	row1.appendChild(s5);
	s6 = document.createElement('span');
	s6.className = 'hidden lListType bgChItems';
	s6.innerHTML = lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	row1.appendChild(s6);

	//�������@
	var bond = document.getElementById("bond1");
	s8 = document.createElement('span');
	//s8.id = 'bondChild';
	s8.className = 'listBond bgChItems';
	if(bond.checked){
		s8.innerHTML = "�������Ɍ���";
	}else{
		s8.innerHTML = "�݂��Ɉ�v�̂�";
	}
	row1.appendChild(s8);

	//���������̃��X�g�Ƀe�[�u���Q���ǉ�
	s10 = document.createElement('span');
	s10.className = 'rListTableName bgChItems';
	s10.title     = '�J�n�ʒu : ' + rclickSelElementsVar.children[rListNum['listS_point']].innerHTML + '\n�T�C�Y : ' + rclickSelElementsVar.children[rListNum['listSize']].innerHTML + '\n�^ : ' + rclickSelElementsVar.children[rListNum['listType']].innerHTML;
	s10.innerHTML = rselectTableName.value;
	row1.appendChild(s10);
	
	s11 = document.createElement('span');
	s11.className = 'rListItemName bgChItems';
	s11.title     = '�J�n�ʒu : ' + rclickSelElementsVar.children[rListNum['listS_point']].innerHTML + '\n�T�C�Y : ' + rclickSelElementsVar.children[rListNum['listSize']].innerHTML + '\n�^ : ' + rclickSelElementsVar.children[rListNum['listType']].innerHTML;
	s11.innerHTML = rclickSelElementsVar.children[rListNum['listItemName']].innerHTML;
	row1.appendChild(s11);
	
	s12 = document.createElement('span');
	s12.className = 'hidden rListJapaneseName bgChItems';
	s12.innerHTML = rclickSelElementsVar.children[rListNum['listJapaneseName']].innerHTML;
	row1.appendChild(s12);
	
	s13 = document.createElement('span');
	s13.className = 'hidden rListS_point bgChItems';
	s13.innerHTML = rclickSelElementsVar.children[rListNum['listS_point']].innerHTML;
	row1.appendChild(s13);
	s14 = document.createElement('span');
	s14.className = 'hidden rListSize bgChItems';
	s14.innerHTML = rclickSelElementsVar.children[rListNum['listSize']].innerHTML;
	row1.appendChild(s14);
	s15 = document.createElement('span');
	s15.className = 'hidden rListType bgChItems';
	s15.innerHTML = rclickSelElementsVar.children[rListNum['listType']].innerHTML;
	row1.appendChild(s15);

	//�e�[�u���P�ƃe�[�u���Q��ID���������ĐV����KEY�����蓖�Ă�
	s17 = document.createElement('span');
	s17.className = 'hidden key bgChItems';
	s17.innerHTML = lselectTableName.value + '.' + lclickSelElementsVar.children[lListNum['listItemName']].innerHTML + "~" + 
	                rselectTableName.value + '.' + rclickSelElementsVar.children[rListNum['listItemName']].innerHTML;
	row1.appendChild(s17);
	
	//�ϊ�
	if(trim(lclickSelElementsVar.children[lListNum['listJapaneseName']].innerHTML) == "&nbsp;"){
		ltmpJpaneseName = "";
	}else{
		ltmpJpaneseName = lclickSelElementsVar.children[lListNum['listJapaneseName']].innerHTML;
	}
	
	//�e�[�u���P�ƃe�[�u���Q��ID���������ĐV����id�����蓖�Ă�
	s18 = document.createElement('span');
	s18.className = 'bondChildLast hidden id bgChItems';
	s18.innerHTML = lselectTableName.value + '.' + lclickSelElementsVar.children[lListNum['listItemName']].innerHTML + "~" +
	                                               ltmpJpaneseName + "~" +
	                                               lclickSelElementsVar.children[lListNum['listS_point']].innerHTML + "~" +
	                                               lclickSelElementsVar.children[lListNum['listSize']].innerHTML + "~" +
	                                               lclickSelElementsVar.children[lListNum['listType']].innerHTML ;
	if(bond.checked){
		s18.innerHTML = s18.innerHTML + "~" + "LEFTJOIN" + "~";
	}else{
		s18.innerHTML = s18.innerHTML + "~" + "INNERJOIN" + "~";
	}
	
	//�ϊ�
	if(trim(rclickSelElementsVar.children[rListNum['listJapaneseName']].innerHTML) == "&nbsp;"){
		rtmpJpaneseName = "";
	}else{
		rtmpJpaneseName = rclickSelElementsVar.children[rListNum['listJapaneseName']].innerHTML;
	}
	
	s18.innerHTML = s18.innerHTML + rselectTableName.value + '.' + rclickSelElementsVar.children[rListNum['listItemName']].innerHTML + "~" +
	                                                               rtmpJpaneseName + "~" +
	                                                               rclickSelElementsVar.children[rListNum['listS_point']].innerHTML + "~" +
	                                                               rclickSelElementsVar.children[rListNum['listSize']].innerHTML + "~" +
	                                                               rclickSelElementsVar.children[rListNum['listType']].innerHTML ;
	row1.appendChild(s18);

	row1.className += "listElem insert add";

	//�����ɒǉ����邽�߂̏���
	var bondChildLast = $(".bondChildLast:last");
	if(bondChildLast.length==0){
		bondList.insertBefore(row1,(bondList.hasChildNodes()) ? bondList.childNodes[0] : null);
	}else{
		var parentDiv = bondChildLast[0].parentNode;
		bondList.insertBefore(row1,parentDiv.nextSibling);
	}
	
	//�s���Ƃ̐F�ς��p�N���X����ǉ�
	(function(){
		var targsList = document.getElementById('bondList');
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

addEvent("load",window,bondDelButton);
addEvent("load",window,bondAddButton);


//���փ{�^���������̃C�x���g�𒣂�t����
var nextButton = function () {
	var nb = $('.nextButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , function(event){ return next(event);});
	}
}


//���փ{�^���������̃C�x���g
//�t�H�[�����쐬����POST���M����
//�I������Ă���O���[�v�ƁA����ɂɒǉ��E�폜���ꂽ���[�U���𑗂�
var next = function (event) {

	//���b�Z�[�W������
	outputMessage("","errMessage");
	outputMessage("","successMessage");

	var selectedTables = document.getElementsByName('selectedTables[]');

	//���̓`�F�b�N�i���ׂẴe�[�u�����g�p���Ă��邩�j
	var bondList = document.getElementById("bondList");
	var tableArray = new Array();
	for(var i=0;i < bondList.children.length;i++){
		tableArray.push(bondList.children[i].children[bListNum['lListTableName']].innerHTML);
		tableArray.push(bondList.children[i].children[bListNum['rListTableName']].innerHTML);
	}
	for(var i=0;i < selectedTables.length ; i++){
		if(tableArray.indexOf(selectedTables[i].value) < 0){
			errJsCode = "ERRJ0034";
			outputMessage(errJsCode,"errMessage");
			return false;
		}
	}

	//�t�H�[���쐬
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./DataView_free_search_3.php');
	
	//�I���e�[�u����
	for(var i=0;i < selectedTables.length ; i++){
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name',selectedTables[i].name);
		sendInp.setAttribute('value',selectedTables[i].value);
		sendForm.appendChild( sendInp );
	}
	
	//���������̃��X�g
	var sendSelect = document.createElement('select');
	sendSelect.setAttribute('name','selectedBonds[]');
	sendSelect.multiple = true;
	sendForm.appendChild( sendSelect );
	var bondList = document.getElementById('bondList');
	for(var i=0;i < bondList.children.length ; i++){
		var sendOption = document.createElement('option');
		var sendText = '';
		var counter = 0;
		//�e�[�u�����ǉ�
		sendText = bondList.children[i].children[bListNum['id']].innerHTML;
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



var newCount = 1;

var rclickSelElementsVar;

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

//���̃��X�g�̉��Ԗڂ̃X�p���l�������Ă��邩�ݒ�
var rListNum = { 
	lListTableName: 1 ,
	listItemName: 2 ,
	listJapaneseName: 3 ,
	listId: 4 ,
	listSort: 5 ,
	listS_point: 6 ,
	listSize: 7 ,
	listType: 8 ,
	listNext: 9 ,
	id: 10 
};

//�폜�{�^���N���b�N�������̃C�x���g�𒣂�t����
function sortDelButton(){
	var nb = $('#sortDelButton');
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
		errJsCode = "ERRJ0048";
		outputMessage(errJsCode,"errMessage");
		return false;
	}

	//�폜�Ώۍs�ԍ���z��ɒǉ�
	//�i�����폜��z��̂��ߍs�ԍ���z��ɓ����d�l���������݂͎g�p���Ă��Ȃ��j
	var sortList = document.getElementById('sortList');
	for(var i=0;i < sortList.children.length;i++){
		if(sortList.children[i].innerHTML == rclickSelElementsVar.innerHTML){
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
			sortList.removeChild(sortList.children[delList[i]]);
		}
	}
	//���I����Ԃ֖߂�
	rclickSelElementsVar = undefined;
	//������Ɗ��ŐF��ς���
	(function(){
		var targsList = document.getElementById('sortList');
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
function sortAddButton(){
	var nb = $('#sortAddButton');
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
	if(lclickSelElementsVar == undefined){
		errJsCode = "ERRJ0049";
		outputMessage(errJsCode,"errMessage");
		return false;
	}
	var disp = document.getElementById("sortList");    //�ǉ��Ώۂ̃e�[�u�����擾
	
	//�o�^�������\�[�g���Ɗ��ɓo�^����Ă��郆�[�U���r
	//���ɓo�^����Ă��郆�[�U�ł���΃��b�Z�[�W��\��
	for(var i=0;i < disp.children.length;i++){
		var targetIdhold = lclickSelElementsVar.children[rListNum['listId']].innerHTML;
		var targetIdTemp = disp.children[i].children[0].innerHTML ;
		var counter = 0;
		for(var j=0;j < disp.children[i].children.length;j++){
			if(disp.children[i].children[j].nodeType == 1){
				if(counter == rListNum['listId']){
					targetIdTemp = disp.children[i].children[j].innerHTML;
				}
				counter++;
			}
		}
		if(!disp.children[i].getAttribute('class').match('hidden')){
			if(targetIdhold == targetIdTemp){
				errJsCode = "ERRJ0050";
				outputMessage(errJsCode,"errMessage");
				return false;
			}
		}
	}
	
	//�E�̃��X�g�ɏ��ǉ�
	var row1 = document.createElement("div");
	s0 = document.createElement('span');
	s0.className = 'hidden bgChItems';
	s0.innerHTML = '';
	row1.appendChild(s0);
	
	s1 = document.createElement('span');
	s1.className = 'lListTableName bgChItems';
	if(lclickSelElementsVar.children[lListNum['listType']].innerHTML == "&nbsp;"){
		s1.title     = '�^ :  ';
	}else{
		s1.title     = '�^ : '+ lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	}
	s1.title     += '\n�J�n�ʒu �F'+ lclickSelElementsVar.children[lListNum['listS_point']].innerHTML;
	s1.title     += '\n�T�C�Y �F'+ lclickSelElementsVar.children[lListNum['listSize']].innerHTML;
	s1.innerHTML = lclickSelElementsVar.children[lListNum['listTableName']].innerHTML;
	//s1.innerHTML = lselectTableName.value;
	row1.appendChild(s1);
	
	s2 = document.createElement('span');
	s2.className = 'listItemName bgChItems';
	if(lclickSelElementsVar.children[lListNum['listType']].innerHTML == "&nbsp;"){
		s2.title     = '�^ :  ';
	}else{
		s2.title     = '�^ : '+ lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	}
	s2.title     += '\n�J�n�ʒu �F'+ lclickSelElementsVar.children[lListNum['listS_point']].innerHTML;
	s2.title     += '\n�T�C�Y �F'+ lclickSelElementsVar.children[lListNum['listSize']].innerHTML;
	s2.innerHTML = lclickSelElementsVar.children[lListNum['listItemName']].innerHTML;
	row1.appendChild(s2);
	
	s3 = document.createElement('span');
	s3.className = 'hidden listJapaneseName bgChItems';
	if(lclickSelElementsVar.children[lListNum['listType']].innerHTML == "&nbsp;"){
		s3.title     = '�^ :  ';
	}else{
		s3.title     = '�^ : '+ lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	}
	s3.title     += '\n�J�n�ʒu �F'+ lclickSelElementsVar.children[lListNum['listS_point']].innerHTML;
	s3.title     += '\n�T�C�Y �F'+ lclickSelElementsVar.children[lListNum['listSize']].innerHTML;
	s3.innerHTML = lclickSelElementsVar.children[lListNum['listJapaneseName']].innerHTML;
	row1.appendChild(s3);
	
	s4 = document.createElement('span');
	s4.className = 'hidden listId bgChItems';
	if(lclickSelElementsVar.children[lListNum['listType']].innerHTML == "&nbsp;"){
		s4.title     = '�^ :  ';
	}else{
		s4.title     = '�^ : '+ lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	}
	s4.title     += '\n�J�n�ʒu �F'+ lclickSelElementsVar.children[lListNum['listS_point']].innerHTML;
	s4.title     += '\n�T�C�Y �F'+ lclickSelElementsVar.children[lListNum['listSize']].innerHTML;
	s4.innerHTML = lclickSelElementsVar.children[lListNum['listId']].innerHTML;
	row1.appendChild(s4);
	
	s5 = document.createElement('span');
	s5.className = 'listSort bgChItems';
	if(lclickSelElementsVar.children[lListNum['listType']].innerHTML == "&nbsp;"){
		s5.title     = '�^ :  ';
	}else{
		s5.title     = '�^ : '+ lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	}
	s5.title     += '\n�J�n�ʒu �F'+ lclickSelElementsVar.children[lListNum['listS_point']].innerHTML;
	s5.title     += '\n�T�C�Y �F'+ lclickSelElementsVar.children[lListNum['listSize']].innerHTML;
	var sort = document.getElementById("sort");
	if(sort.value=="asc"){
		s5.innerHTML = "����";
	}else{
		s5.innerHTML = "�~��";
	}
	row1.appendChild(s5);
	
	s6 = document.createElement('span');
	s6.className = 'hidden listS_point right bgChItems';
	if(lclickSelElementsVar.children[lListNum['listType']].innerHTML == "&nbsp;"){
		s6.title     = '�^ :  ';
	}else{
		s6.title     = '�^ : '+ lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	}
	s6.title     += '\n�J�n�ʒu �F'+ lclickSelElementsVar.children[lListNum['listS_point']].innerHTML;
	s6.title     += '\n�T�C�Y �F'+ lclickSelElementsVar.children[lListNum['listSize']].innerHTML;
	s6.innerHTML = lclickSelElementsVar.children[lListNum['listS_point']].innerHTML;
	row1.appendChild(s6);
	
	s7 = document.createElement('span');
	s7.className = 'hidden listSize right bgChItems';
	if(lclickSelElementsVar.children[lListNum['listType']].innerHTML == "&nbsp;"){
		s7.title     = '�^ :  ';
	}else{
		s7.title     = '�^ : '+ lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	}
	s7.title     += '\n�J�n�ʒu �F'+ lclickSelElementsVar.children[lListNum['listS_point']].innerHTML;
	s7.title     += '\n�T�C�Y �F'+ lclickSelElementsVar.children[lListNum['listSize']].innerHTML;
	s7.innerHTML = lclickSelElementsVar.children[lListNum['listSize']].innerHTML;
	row1.appendChild(s7);
	
	s8 = document.createElement('span');
	s8.className = 'hidden listType bgChItems';
	s8.innerHTML = lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	row1.appendChild(s8);
	
	//���̉�ʂֈ����p���p
	s9 = document.createElement('span');
	s9.className = 'hidden listNext bgChItems';
	//s9.innerHTML = lselectTableName.value;
	s9.innerHTML = lclickSelElementsVar.children[lListNum['listTableName']].innerHTML;
	s9.innerHTML = s9.innerHTML + "~" + lclickSelElementsVar.children[lListNum['listItemName']].innerHTML;
	if(trim(lclickSelElementsVar.children[lListNum['listJapaneseName']].innerHTML) == "&nbsp;"){
		s9.innerHTML = s9.innerHTML + "~";
	}else{
		s9.innerHTML = s9.innerHTML + "~" + lclickSelElementsVar.children[lListNum['listJapaneseName']].innerHTML;
	}
	s9.innerHTML = s9.innerHTML + "~" + sort.value;
	s9.innerHTML = s9.innerHTML + "~" + lclickSelElementsVar.children[lListNum['listS_point']].innerHTML;
	s9.innerHTML = s9.innerHTML + "~" + lclickSelElementsVar.children[lListNum['listSize']].innerHTML;
	if(lclickSelElementsVar.children[lListNum['listType']].innerHTML == "&nbsp;"){
		s9.innerHTML = s9.innerHTML + "~";
	}else{
		s9.innerHTML = s9.innerHTML + "~" + lclickSelElementsVar.children[lListNum['listType']].innerHTML;
	}
	s9.innerHTML = s9.innerHTML + "~" + lclickSelElementsVar.children[lListNum['listTableName']].innerHTML;
	row1.appendChild(s9);
	
	s10 = document.createElement('span');
	s10.className = 'hidden sortChildLast id bgChItems';
	s10.innerHTML = lclickSelElementsVar.children[lListNum['listId']].innerHTML;
	row1.appendChild(s10);
	
	row1.className += "listElem insert add";
	
	//�����ɒǉ����邽�߂̏���
	var sortChildLast = $(".sortChildLast:last");
	if(sortChildLast.length==0){
		disp.insertBefore(row1,(disp.hasChildNodes()) ? disp.childNodes[0] : null);
	}else{
		var parentDiv = sortChildLast[0].parentNode;
		disp.insertBefore(row1,parentDiv.nextSibling);
	}
	
	//�s���Ƃ̐F�ς��p�N���X����ǉ�
	(function(){
		var targsList = document.getElementById('sortList');
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
addEvent("load",window,sortDelButton);
addEvent("load",window,sortAddButton);

//�����J�n�{�^���������̃C�x���g�𒣂�t����
var nextButton = function () {
	var nb = $('.nextButton');
	for(var i = 0; i < nb.length ;i++){
		addEvent("click", nb[i] , next);
	}
}
//�����J�n�{�^���������̃C�x���g
//�t�H�[�����쐬����POST���M����
//�I������Ă���O���[�v�ƁA����ɂɒǉ��E�폜���ꂽ���[�U���𑗂�
var next = function () {
	
	//�t�H�[���쐬
	var sendForm = document.createElement('form');
	sendForm.className = "hidden";
	sendForm.setAttribute('method','post');
	sendForm.setAttribute('action','./DataView_disp.php');
	
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
	
	//���o����
	var selectedFilters = document.getElementsByName('selectedFilters[]');
	for(var i=0;i < selectedFilters.length ; i++){
		var sendInp = document.createElement('input');
		sendInp.setAttribute('name',selectedFilters[i].name);
		sendInp.setAttribute('value',selectedFilters[i].value);
		sendForm.appendChild( sendInp );
	}
	
	//���בւ��ݒ�̃��X�g
	var sendSelect = document.createElement('select');
	sendSelect.setAttribute('name','selectedSorts[]');
	sendSelect.multiple = true;
	sendForm.appendChild( sendSelect );
	var sortList = document.getElementById('sortList');
	for(var i=0;i < sortList.children.length ; i++){
		var sendOption = document.createElement('option');
		var sendText = '';
		var counter = 0;
		//�e�[�u�����ǉ�
		sendText += trim(sortList.children[i].children[rListNum['listNext']].innerHTML);
		sendOption.setAttribute('value',sendText);
		sendSelect.appendChild(sendOption);
		sendOption.selected = true;
	}
	
	//�v���O�������擾�i���݂̉�ʖ��j
	var pgName = document.getElementById('pgName');
	var sendInp = document.createElement('input');
	sendInp.setAttribute('name',pgName.name);
	sendInp.setAttribute('value',pgName.value);
	sendForm.appendChild( sendInp );
	
	//�t�H�[�����s
	document.body.appendChild( sendForm );
	sendForm.submit();
	return false;
}
addEvent('load',window,nextButton);

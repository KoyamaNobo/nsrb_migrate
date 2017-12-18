//�C���v�b�g�{�b�N�X�̃N���X�ɂ���ē��̓`�F�b�N�𕪂���
//  �uSNUMERC�v�̏ꍇ�FsNumericCheck���Ăяo��
//  �uNUMERC�v�̏ꍇ�FnumericCheck���Ăяo��
//   �߂�l true :���̓`�F�b�N������ �܂��� ���̓`�F�b�N���s��Ȃ��Ƃ�
//          false:���̓`�F�b�N���s��
function inpCheck(evt){
	return elementInpCheck(evtToElement(evt));
}

//�C���v�b�g�{�b�N�X�̃N���X�ɂ���ē��̓`�F�b�N�𕪂���
//�uSNUMERC�v�̏ꍇ�FsNumericCheck���Ăяo��
//�uNUMERC�v�̏ꍇ�FnumericCheck���Ăяo��
//�߂�l true :���̓`�F�b�N������ �܂��� ���̓`�F�b�N���s��Ȃ��Ƃ�
//      false:���̓`�F�b�N���s��
function elementInpCheck(targElem) {
	tmp = $(targElem)[0].className;
	if (tmp) {
		if (tmp.match(/SNUMERC/)) {
			if (!sNumericCheck(targElem.value)) {
				// ���̓`�F�b�N�ɃG���[������Ƃ�
				return false;
			}
		} else if (tmp.match(/NUMERC/)) {
			if (!numericCheck(targElem.value)) {
				// ���̓`�F�b�N�ɃG���[������Ƃ�
				return false;
			}
		}
	}
	return true;
}

//���l���`�F�b�N����i���̂݁j
//  �߂�l true :���̓`�F�b�N������
//         false:���̓`�F�b�N���s��
function numericCheck(val){
	if (val.match(/^( )*-?[0-9]*.?[0-9]*( )*$/)) {
		$("#status6").html( "<span></span>");
		return true;
	} else {
		$("#status6").html( "<span>NUMERC</span>");
		userSetting();
		return false;
	}
}

//���l���`�F�b�N����i���E���Ƃ��ɋ��p�j
//  �߂�l true :���̓`�F�b�N������
//         false:���̓`�F�b�N���s��
function sNumericCheck(val){
	if (val.match(/^( )*-?[0-9]*.?[0-9]*( )*$/)) {
		$("#status6").html( "<span></span>");
		return true;
	} else {
		$("#status6").html( "<span>NUMERC</span>");
		userSetting();
		return false;
	}
}

//�C���v�b�g�{�b�N�X�̃N���X��SNUMERC��NUMERC�̂Ƃ�
//���͒l�̃t�H�[�}�b�g�ϊ�������i�����_��t������j
//  ��j�uSNUMERC5V2�v-1 �� -0.01
//  ��j�uNUMERC5V2�v 10 ��  0.1
function inpFormat(evt){
	return elementInpFormat(evtToElement(evt));
}


//�C���v�b�g�{�b�N�X�̃N���X��SNUMERC��NUMERC�̂Ƃ�
//���͒l�̃t�H�[�}�b�g�ϊ�������i�����_��t������j
//��j�uSNUMERC5V2�v-1 �� -0.01
//��j�uNUMERC5V2�v 10 ��  0.1
function elementInpFormat(targElem){
	var res = targElem.value;
	var minusFlg = false;

	formatFlg = false;

	//���͂��Ȃ��ꍇ�͉������Ȃ�
	if(res.length == 0){
		return res;
	}

	tmp = $(targElem)[0].className;

	var found1 = tmp.match(/SNUMERC[\d]+V[\d]+/);
	if (found1){
		className = found1[0].substr(7);
		formatFlg = true;
	}else{
		var found2 = tmp.match(/NUMERC[\d]+V[\d]+/);
		if (found2) {
			className = found2[0].substr(6);
			formatFlg = true;
		}
	}

	if(formatFlg){
		return res;
	}
	return res;
}

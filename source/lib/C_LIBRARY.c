/* header files */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <libcob.h>
#include "confheader.h"

#ifndef MT_COMMON
#define MT_COMMON 1
#define MAP_SRC_FUNC_LEN 50
char map_source_func[MAP_SRC_FUNC_LEN]="";
int MT_Initialize();
#endif


#define min(a,b) ((a) < (b) ? (a) : (b))

/* �������cobol�ɓ]�ʂ���֐� */
void C_move_to_cob(char *cob_dat, const char *dat)
{
    int len = strlen(cob_dat);                      // data length in cob
    memset(cob_dat, ' ', min(len, strlen(dat)));    // clear with spaces
    memcpy(cob_dat, dat, min(len, strlen(dat)));    // data copy
    return;
}

///* �u������B buf �̒��� dlm �� ato �ɂ���B����=0 ���s=1 */
//int Replace(char *buf, char *dlm, char *ato)
//{
//    char *mituke;
//    size_t maelen, atolen;
//    
//    maelen = strlen(dlm);
//    atolen = strlen(ato);
//    if (maelen == 0 || (mituke = strstr(buf, dlm)) == NULL) return 1;
//    memmove(mituke + atolen, mituke + maelen, strlen(buf) - (mituke + maelen - buf ) + 1);
//    memcpy(mituke, ato, atolen);
//    return 0;
//}


//// SELECT��̑��u�w�肨��у{�����[�����A�{���� �[���ʂ��ԍ�
//// �܂��̓��f�B�A���ʖ����A���s���ɕύX���܂��B
//int PRMASGN(char *arg1,char *arg2,char *arg3)
//{
//	int ret = 0;
//	
//	return ret;
//}
//
//// ���[�U�̒�`�����ϊ��\�ɂ��������āA�^����ꂽ�f�[�^���R�[�h�ϊ����܂��B
//// �����P�F�ϊ��Ώہ^�ϊ�����
//// �����Q�F����
//// �����R�F�ϊ��\
//int CBLCODE(char *arg1,char *arg2,char *arg3)
//{
//	int ret = 0;
//	
//	return ret;
//}

// �σp�����[�^�ŁA�o�l���邢�͂i�r���N�����܂��B
// �����P�F�ϊ��Ώہ^�ϊ�����
// �����Q�F����(���g�p)
int CBLRUN(char *arg1,char *arg2){
	int ret = 0,i = 0;
	char *str;
	char command[2048]="";
	
	//�֐��������L�ϐ��ɃZ�b�g
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," Error C [%02d]: CBLRUN is not match Argument count \n",99);
	
	if(cob_call_params != 2){
		fprintf(stderr,map_source_func);
		exit(1);
	}
	i = 0;
	memcpy(command,arg1,cob_current_module->cob_procedure_parameters[0]->size);
	
	//�X�y�[�X�ɒu�����镶��
	char dlm1[] = "#_/>;";
	char ato = ' ';
	char split = 0x27;
	
	// �����񂩂�"#"��" "�ɒu��
	str = command;
	while(strchr(str,(int) *(dlm1 + 0)) != NULL){
		str = strchr(str,(int) *(dlm1 + 0));
		*str = ato;
	}
	
	// �����񂩂�"_"��" "�ɒu��
	str = command;
	while(strchr(str,(int) *(dlm1 + 1)) != NULL){
		str = strchr(str,(int) *(dlm1 + 1));
		*str = ato;
	}
	
	// �����񂩂�"/"��" "�ɒu��
	str = command;
	while(strchr(str,(int) *(dlm1 + 2)) != NULL){
		str = strchr(str,(int) *(dlm1 + 2));
		*str = ato;
	}
	
	// �����񂩂�">"��"\0"�ɒu��
	str = command;
	while(strchr(str,(int) *(dlm1 + 3)) != NULL){
		str = strchr(str,(int) *(dlm1 + 3));
		*str = split;
	}
	
//	
//	str = command;
//	while(strchr(str,(int) *(dlm1 + 4)) != NULL){
//		str = strchr(str,(int) *(dlm1 + 4));
//		*str = split;
//	}
	
	// ������";"�̑O�ɃX�y�[�X������ ";"��"'"�ɒu��
	str = command;
	if(strchr(str,(int) *(dlm1 + 4)) != NULL){
		char tmpP = ' ';  //�ЂƂO���i�[
		char tmpN = ' ';  //�����i�[
		str = (char *)strchr(str,(int) *(dlm1 + 4));
		while(tmpN != '\0'){
			if(*str == *(dlm1 + 4)){
				tmpP = *str;
				*str = ' ';
			}else{
				tmpN = tmpP;
				tmpP = *str;
				
				if(tmpN == *(dlm1 + 4)){
					*str = split;
				}else{
					*str = tmpN;
				}
			}
			
			str++;
		}
	
		// �R�}���h���s
		char debugcmd[2048]="";
		sprintf(debugcmd,"logger -i \"%s\"",command);
		system(debugcmd);
		ret = system(command);
	}else{
		ret = 1;
	}
	
	//
	if((ret != 0)){
		ret = 1;
	}else{
		ret = 0;
	}
	//���L�ϐ������ɖ߂�
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	
	return ret;
}


// �v���O���������삵�Ă��郏�[�N�X�e�[�V�����́A���[�N�X�e�[�V��������ʒm���܂��B
int CBLSTNNO(char *arg1,char *arg2){
	int ret = 0;
	
	//�֐��������L�ϐ��ɃZ�b�g
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func," Error C [%02d]: CBLSTNNO is not match Argument count \n",99);
	
	if(cob_call_params != 2){
		fprintf(stderr,map_source_func);
		exit(1);
	}
	
	
	C_move_to_cob(arg1,arg2);
	
	//���L�ϐ������ɖ߂�
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	
	return ret;
}

// ���N���[�Y�i�t�@�C�����x���X�V�j �s�K�v
int _CBLTCLS(char *arg1){
	int ret = 0;
	
	return ret;
}

// �C���^�[�o���^�C�}�̋@�\������܂��B�w�肳�ꂽ����(0.1�b�P��)�����A���ԑ҂����s���܂��B
int CBLTIMER(char *msc){
	int ret = 0;
	int time = atoi(msc) * 100000;
	
	usleep(time);
	
	return ret;
}
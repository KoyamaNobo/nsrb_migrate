/* COBOL ScreenDescription Version 0.1 */
/* Create 20140210  Author koyama */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>    /*atoi�𗘗p*/
#include <stdarg.h>    /*va_list���g�����߂ɕK�v*/
#include <ctype.h>     /* toupper�ɕK�v */
#include <time.h>      /* localtime�ɕK�v 20150828 */
#include <libxml/xmlreader.h>    /*conf�t�@�C����xml��ǂނ���*/
#include <libxml/xpath.h>        /*conf�t�@�C����xml��ǂނ���*/
#include <libcob.h>
#include <sys/stat.h>
#include "confheader.h"

#ifndef SD_CONST
#define SD_CONST 1
#define HRIZONTAL_LENGTH 1024 //��ʂ̉���+""+'\0'
#define SD_LINE_MAX 24
#define SD_COL_MAX 80
#define COB_USING 1
#define COB_FROM 2
#define BUFF_SIZE 2048
#define WAIT_SLEEP 3000000


#ifndef CONF_PATH
#define CONF_PATH
const char* CONF_FLIEPATH = "./conf.xml";
const char* CONF_SHREPATH = "/usr/local/share/map/conf.xml";
const char* CONF_DEFPATH  = "//conf/*/text()";    //�g�b�v���x������꒼����\��
#endif

//DB�̃^�O��
#ifndef CONF_SD_TAG
#define CONF_SD_TAG 1
const char* CONF_SCRPATH = "screenDefPath";
const char* CONF_SCREXT = "screenDefExt";
#endif

static int (*func)(char *errno, const char *errmsg);
//const char* SD_SIGNED_N = "S9";
const char* SD_SIGNED_N = "S";
const char* SD_SUPPRESS = "Z";
const char* SD_FORMAT_S = "-";
const char* SD_BLANK = "B";
//#define SIGNED_N "S9";

#endif

#ifndef MT_COMMON
#define MT_COMMON 1
#define MAP_SRC_FUNC_LEN 50
char map_source_func[MAP_SRC_FUNC_LEN]="";
int MT_Initialize();
#endif

//conf����擾����debug_flg.cob-mysql�Ő錾
//static int myConfDebugFlg;

static char strTime[] = "0000/00/00 00:00:00.000000";
static char SD_screenDefPath[256];
static char SD_screenDefExt[32];
//header��extern���Ő錾
int SD_cobScreenLen = 0;                 /*���Ɏg�p�\�ȃI�u�W�F�N�g�̔ԍ�������*/
struct screenObject *SD_cobScreen;      /*�g�b�v���x�����`�H*/

//////////////////Function liet AND prototype Start (id��Error Output��������̂̂�)
//Id=00 RTrim����
int isNullOrEmpty(char *targ);
//ID=02 �ݒ�t�@�C���̃p�X���擾
char *getConfFilename(char *strConfPath);
//Id=03 ����̕������������Ȃ���Ȃ���
void remTargChar(char *origText,char targ);
//ID=04 FROM��̑Ή�
int SD_From(char *cobArgName,char *argPoint,char *cSize,char *cArgc,...);
//targObj�̗�ԍ���Ԃ�
int getColTargetObject(struct screenObject *targObj);
//targObj�̍s�ԍ���Ԃ�
int getLineTargetObject(struct screenObject *targObj);
//cob_field��packed�Ƃ��Đ������l���m�F
int SD_isPackedFormat(cob_field *);
//////////////////Function liet AND prototype End

//----------------------------------------------------------�ُ�I������
//author:n.koyama
//date  :20140530
//�ُ�I�������̂Ƃ��ɉ�������K�v�����鎞�̂���
//
void executeEnd(){
	exit(1);
}

//���ݎ��Ԃ��t�H�[�}�b�g(YY/MM/dd hh:mm:ss.mmmmmm)�ŕԂ�
//in/out :retStr �Ԃ��Ώە�����̃|�C���^
//author : koyama
char *local_server_time(char *retStr){
	struct timeval timer;
	struct tm *date;

	gettimeofday(&timer, NULL);
	date = localtime(&timer.tv_sec);
	sprintf(retStr,"%2d/%02d/%02d %02d:%02d:%02d.%06d"
		,date->tm_year+1900
		,date->tm_mon+1
		,date->tm_mday
		,date->tm_hour
		,date->tm_min
		,date->tm_sec
		,timer.tv_usec);
	return retStr;
}

//RTrim����(�󔒕����݂̂��ǂ����𔻒�)
//(���ꂼ���Rtim������̂ł����ł���p����������)
//date:20160520
//auth: koyama
int isNullOrEmpty(char *targ){
	char *strstart;
	char *strend;
	int num=0;
	strstart = targ;
	//�Ō�̕������画��Ώ�
	strend = targ + strlen(strstart);
	for(strend--;strend >= strstart;strend--){
		if(*strend == ' '){
			num++;
		}
	}
	//�S�Ă��X�y�[�X�Ȃ�
	if(num == strlen(strstart)){
		num = 1;
	}else{
		num = 0;
	}
	return num;
}

//�ݒ�t�@�C���̃p�X���擾
//date:20150828
//auth: koyama
//�J�����gdir��conf�������J���Ƃǂ��łł����s�ł��Ȃ��̂�
char *getConfFilename(char *strConfPath){
	FILE *fpFileExist;      //20150828 add koyama
	char fileName[1024] = "";
	int funcId = 2;

	strcpy(fileName,CONF_FLIEPATH);
	if((fpFileExist = fopen(fileName, "r")) == NULL){
		strcpy(fileName,CONF_SHREPATH);
		if((fpFileExist = fopen(fileName, "r")) == NULL){
			cob_runtime_error("Error C [%x]:conf read Error %s \n",mytoolgetErrorId(LIB_ID_SCREEN,funcId,1), local_server_time(strTime));
			exit (1);
		}else{
			strcpy(strConfPath,CONF_SHREPATH);
		}
	}else{
		strcpy(strConfPath,CONF_FLIEPATH);
	}
	fclose(fpFileExist);

	return strConfPath;
}

//-----------------------------------------------------------������x����y��
//author:n.koyama
//date  :20140609
//p:�Ώە�����
//x:�u���Ώ�
//y:�u������
char *chrchange(char *p,char x,char y){
	int i = 0;
	char  s[128];
	char* t;

	strncpy(s,p,strlen(p));

	while(t = strrchr(s,x)){
		*t = (char)y;
	}

	memcpy(p,s,(strlen(s) + 1));

	return (p);                  /* ������̐擪�A�h���X��Ԃ� */
}

//-----------------------------------------------------------����̕������������Ȃ���Ȃ���
//author:n.koyama
//date  :20151222
//origText:�Ώە�����
//targ:�폜����
//return �Ȃ�
void remTargChar(char *origText,char targ){
	char *origTextEnd;
	char *copyText;
	int addlen = 0;

	copyText  = origText;
	//\0�܂ł��R�s�[������
	origTextEnd = origText + strlen(origText) + 1;
	//
	for(;origText < origTextEnd;origText++){
		//���s�������΂�
		if(*copyText == targ ){
			//������ɓ������Ƃ��͗]����+
			copyText++;
		}
		//�����̃R�s�[
		*origText = *copyText;
		copyText++;
	}
}

//-----------------------------------------------------------����̕����Ō��l��
//author:n.koyama
//date  :20151222
//origText:�Ώە�����
//targ:�폜����
//return �Ȃ�
void addAfterTargChar(char *origText,char targ,int length){
	char *origTextEnd;
	char *copyText;
	int addlen = 0;

	copyText  = origText + strlen(origText);
	//\0�܂ł��R�s�[������
	origTextEnd = origText + strlen(origText) + length;
	//�Ōォ��X�^�[�g
	for(;copyText < origTextEnd;copyText++){
		//�����̃R�s�[
		*copyText = targ;
	}
	//�Ō�ɏI�[�L�������ďI��
	*copyText = '\0';
}

//��ʃI�u�W�F�N�g�̔z�񂩂疼�O�ɑΉ�����I�u�W�F�N�g�̃|�C���^��Ԃ�
//author:n.koyama
//date  :20150812
//
struct screenObject *searchTargetObject(struct screenObject *screenObjArray,char *targName,int arrayLen){
	struct screenObject *targObject;
	int ii = 0;

	//�֐��������L�ϐ��ɃZ�b�g
	char strStack[MAP_SRC_FUNC_LEN];
	memcpy(strStack,map_source_func,MAP_SRC_FUNC_LEN);
	memset(map_source_func,'\0',MAP_SRC_FUNC_LEN + 1);
	sprintf(map_source_func,"searchTargetObject :%.10s",targName);

	targObject = NULL;
	for(ii=0;ii < arrayLen;ii++){
		if(strcmp(screenObjArray[ii].cobargname,targName) == 0){
			//strncmp(screenObjArray[ii].cobargname,targName,strlen(targName))
			targObject = &screenObjArray[ii];
			break;
		}
	}

	if(!targObject){
		fprintf(stderr," Error C [%02d]: Not Found argument name [%s] : %s \n",99,targName,map_source_func);
		exit(1);
	}
	//���L�ϐ������ɖ߂�
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return targObject;
}

//��ʃI�u�W�F�N�g�̔z�񂩂�s�ϐ�(YName)�ɑΉ�����I�u�W�F�N�g�̃|�C���^��Ԃ�
//author:n.koyama
//date  :20150812
//
void setYNameTargetObject(struct screenObject *screenObjArray,char *targName,int setValue,int arrayLen){
	struct screenObject *targObject;
	int ii = 0;
	for(ii=0;ii < arrayLen;ii++){
		if(strcmp(screenObjArray[ii].yname,targName) == 0){
			screenObjArray[ii].y = setValue;
		}
	}
}

//��ʃI�u�W�F�N�g�̔z�񂩂��ϐ�(XName)�ɑΉ�����I�u�W�F�N�g�̃|�C���^��Ԃ�
//author:n.koyama
//date  :20150812
//
void setXNameTargetObject(struct screenObject *screenObjArray,char *targName,int setValue,int arrayLen){
	struct screenObject *targObject;
	int ii = 0;
	for(ii=0;ii < arrayLen;ii++){
		if(strcmp(screenObjArray[ii].xname,targName) == 0){
			screenObjArray[ii].x = setValue;
		}
	}
}
//cobtype�������̂Ƃ������_�ȉ��̒����𔻒�
//author:koyama
//in :: digits :�S�̂̒���,editForm:�ΏۂƂ��镶����, delim:�����_�̋�؂�
int SD_calc_editing_scale(int digits,char *editForm,char delim){
	int cnt = 0;
	char *strToStart,*strToEnd;

	strToEnd   = editForm + (strlen(editForm) - 1);
	strToStart = strchr(editForm,delim);
	for(;strToEnd > strToStart;strToEnd--){
		switch(*strToEnd){
		case '/' :
		case 'P' :
		case '+' :
		case '-' :
			//���ꕶ���̎��̓J�E���g���Ȃ�
			break;
		case 'R':
			//DB�̓L�[���[�h
			if(*(strToEnd - 1) == 'C'){
				strToEnd--;
				break;
			}
		case 'B':
			//DB�̓L�[���[�h
			if(*(strToEnd - 1) == 'D'){
				strToEnd--;
				break;
			}
		case '9' :
		case '0' :
		case 'Z' :
		case '*' :
		case 0x5c :
			//�~�L����Esc�ɂȂ�̂�
		default  :
			//���ꕶ��,�L�[���[�h�ł͂Ȃ����̂��J�E���g
			cnt++;
		}

	}

	return cnt;
}

//cobtype�������̂Ƃ������̕����̒����𔻒�
//PIC�����鎞��digits�͈̔͂�����,��������digits�͈̔͂�����������
//author:koyama
int cobtype_calc_digits(char *cobtype){
	char *start_cobtype;
	char *end_cobtype;
	int hyphen_flg = 0;
	int retVal     = 0;

	start_cobtype = cobtype;
	//����܂�
	end_cobtype = cobtype + strlen(cobtype) +1;

	for(;start_cobtype < end_cobtype;start_cobtype++){
		if(*start_cobtype == cob_current_module->decimal_point){
			break;
		}
		if(*start_cobtype == '9' || *start_cobtype == 'Z' || *start_cobtype == '-'){
			//�n�C�t�����܂܂�鎞�̓n�C�t������Ȃ��l�ɂȂ�
			if(*start_cobtype == '-'){
				hyphen_flg = 1;
			}else{
				retVal++;
			}
		}

	}
	return retVal;
}

//
//editingFormat�̐������ڂ��J�E���g
//author:koyama
char *setFormatEdit(struct screenObject *targObj,char *strReturn){
	char size_e;
	char *tPoint;
	char *ePoint;
	char currChar;
	int i=0,j=0;
	char tempCobtype[64]="";
	char strFromat[2048]="";
	int targCharCnt = 0,currCharCnt=0;

	//type��������ꎞ�ϐ��ɃR�s�[
	if(targObj->cobtype[0] != 'R'){
		strcpy(tempCobtype,targObj->cobtype);
	}else{
		//1�����ڂ�R�Ȃ�Ƃ΂�
		strcpy(tempCobtype,(targObj->cobtype + 1));
	}

	//R��reverse�̎w��Ȃ̂Ŗ���������
	if(strlen(tempCobtype) == 1 || strchr(tempCobtype,'S') != NULL){
		char setChar;
		if(strchr(tempCobtype,'S') == NULL){
			setChar = tempCobtype[0];
		}else{
			setChar = '-';
		}
		for(i=0;i < targObj->length;i++){
			//����̕����̘A��
			strFromat[i] = setChar;
		}
	}else{
		j=0;
		for(i=0;i < strlen(tempCobtype);i++){
			if(*(tempCobtype + i) != 'R' && *(tempCobtype + i) != 'b'){
				strFromat[j] = *(tempCobtype + i);
				j++;
			}
		}
	}

	currChar = 0x00;
	tPoint = strFromat;
	ePoint = strFromat + strlen(strFromat);


	strReturn = malloc(sizeof(size_e) * ((strlen(strFromat) * 5) + 1) );
	memset(strReturn,0x00,((strlen(strFromat) * 5) + 1));

	for(;tPoint < ePoint;tPoint++){
		if(currChar != *tPoint){
			//�ŏ��͓���Ȃ����߂�
			if(targCharCnt != 0){
				*(strReturn + (((targCharCnt - 1) * 5) + 1) )= (unsigned char)currCharCnt;
			}
			currChar = *tPoint;
			*(strReturn + (((targCharCnt) * 5)) ) = currChar;
			//�����ň�����������Ƃ�
			currCharCnt=1;
			targCharCnt++;
		}else{
			currCharCnt++;
		}
	}
	//�Ō�̎w��̒���������
	*(strReturn + (((targCharCnt - 1) * 5) + 1) )= (unsigned char)currCharCnt;


//	free(strFromat);
	return strReturn;
}

//
//targObj�̍s�ԍ���Ԃ�
//author:koyama
int getLineTargetObject(struct screenObject *targObj){
	return (int)(targObj->y + targObj->lineShift );
}


//
//targObj�̗�ԍ���Ԃ�
//author:koyama
int getColTargetObject(struct screenObject *targObj){
	return (int)(targObj->x + targObj->ColShift );
}

//
//cob_field��packed�Ƃ��Đ������l���m�F
//author:koyama
int SD_isPackedFormat(cob_field *checkTarg){
	int ret=1,counter=0;
	//pack�̍Ō��0.5�o�C�g�Ƀt���O������̂Ő��������ǂ����𔻒�
	char checkChar = *(checkTarg->data + (checkTarg->size - 1)) & 0x0F;
	switch (checkChar){
		case 0x0c:
		case 0x0d:
		case 0x0F:
			break;
		default:
			ret = 0;
			break;
	}

	return ret;
}


//
//editingFormat�̐������ڂ��J�E���g
//author:koyama
int getNumCount(char *text){
	char *strStart;
	char *strEnd;
	char *strTmp;
	int length=0;

	strStart = text;
	strEnd = text + strlen(text);

	for(strTmp=strStart;strTmp < strEnd;strTmp++){
		switch(*strTmp){
		case '9':
		case '+':
		case '-':
		case 'Z':
		case '*':
		case 'B':
		case 0x5c:
			length++;
			break;
		default:
			break;
		}
	}

	return length;
}


//
void SD_AcceptStdin(struct screenObject *temporaryObj,char *term_buff){
	int counter = 0;
	//���͂����߂�
	printf("INP (%02d,%02d)(,%02d) %s \"%s\"_\r\n",
		getLineTargetObject(temporaryObj), getColTargetObject(temporaryObj),
		(getColTargetObject(temporaryObj) + temporaryObj->length), temporaryObj->cobtype, temporaryObj->cobargname);
	//�����Ƃ��܂ŌJ��Ԃ��Ă���
	counter = 0;
	while(fgets((char *)term_buff, BUFF_SIZE, stdin) == NULL){
		//�l��������Ή������Ȃ�                        check 20140508 koyama
		//���l��������
		usleep(WAIT_SLEEP);    //0.000001��sleep?
		//
	}
	printf("NIN (%02d,%02d)(,%02d) %s \"%s\"_\r\n",
		getLineTargetObject(temporaryObj), getColTargetObject(temporaryObj),
		(getColTargetObject(temporaryObj) + temporaryObj->length), temporaryObj->cobtype, temporaryObj->cobargname);
}



//
//2�o�C�g������������
//author:koyama
int memset2byte(char *text,char *setChar,int length){
	int ret=0;
	char *pt,*ept;
	pt = text;
	ept = text + (length);

	//
	while(pt < ept){
		strncpy(pt,setChar,2);
		pt += 2;
	}

	return ret;
}

//---------------------------------------------------------------------------------------------SD_getFromVarPnt
//�Y��������ϐ��̌��݂̒l�̃|�C���^��Ԃ�
//author:n.koyama
//date  :20140611
//
char *SD_getFromVarPnt(struct fromObject fromVar,int length){

	char t;   //�ϐ��̃T�C�Y�p
	char *currentP;    //���ݗL�����Ǝv����f�[�^�̃|�C���^
	char tmp1[128] = "";
	char tmp2[128] = "";
	char tmp3[128] = "";
	int dim1;
	int dim2;
	int dim3;

	if(fromVar.bodyPnt != NULL){
		currentP = fromVar.bodyPnt->data;
	}

	switch(fromVar.iDim){
	case 0:
		fromVar.curPnt = currentP;
		break;
	case 1:
		if(fromVar.actualFlg1 == -99){
			//�|�C���^�̎��̓|�C���^�̐���ꎞ�ۑ��A����𐔒l��
			memcpy(tmp1,fromVar.fromVar1,fromVar.var1Length);
			//�z��̓Y������1����n�܂�̂�
			dim1 = (fromVar.var1Size * sizeof(t)) * (atoi(tmp1) - 1);
		}else{
			dim1 = (fromVar.var1Size * sizeof(t)) * (fromVar.actualFlg1 - 1);
		}
		fromVar.curPnt = currentP + dim1;
		break;
	case 2:
		if(fromVar.actualFlg1 == -99){
			//�|�C���^�̎��̓|�C���^�̐���ꎞ�ۑ��A����𐔒l��
			memcpy(tmp1,fromVar.fromVar1,fromVar.var1Length);
			//�z��̓Y������1����n�܂�̂�
			dim1 = (fromVar.var1Size * sizeof(t)) * (atoi(tmp1) - 1);
		}else{
			dim1 = (fromVar.var1Size * sizeof(t)) * (fromVar.actualFlg1 - 1);
		}
		if(fromVar.actualFlg2 == -99){
			//�|�C���^�̎��̓|�C���^�̐���ꎞ�ۑ��A����𐔒l��
			memcpy(tmp2,fromVar.fromVar2,fromVar.var2Length);
			//�z��̓Y������1����n�܂�̂�
			dim2 = (fromVar.var2Size * sizeof(t)) * (atoi(tmp2) - 1);
		}else{
			dim2 = (fromVar.var2Size * sizeof(t)) * (fromVar.actualFlg2 - 1);
		}
		//0����n�܂�̂� - 1?
		fromVar.curPnt = currentP + dim1 + dim2 ;
		break;
	case 3:
		if(fromVar.actualFlg1 == -99){
			//�|�C���^�̎��̓|�C���^�̐���ꎞ�ۑ��A����𐔒l��
			memcpy(tmp1,fromVar.fromVar1,fromVar.var1Length);
			//�z��̓Y������1����n�܂�̂�
			dim1 = (fromVar.var1Size * sizeof(t)) * (atoi(tmp1) - 1);
		}else{
			dim1 = (fromVar.var1Size * sizeof(t)) * (fromVar.actualFlg1 - 1);
		}
		if(fromVar.actualFlg2 == -99){
			//�|�C���^�̎��̓|�C���^�̐���ꎞ�ۑ��A����𐔒l��
			memcpy(tmp2,fromVar.fromVar2,fromVar.var2Length);
			//�z��̓Y������1����n�܂�̂�
			dim2 = (fromVar.var2Size * sizeof(t)) * (atoi(tmp2) - 1);
		}else{
			dim2 = (fromVar.var2Size * sizeof(t)) * (fromVar.actualFlg2 - 1);
		}
		if(fromVar.actualFlg3 == -99){
			//�|�C���^�̎��̓|�C���^�̐���ꎞ�ۑ��A����𐔒l��
			memcpy(tmp3,fromVar.fromVar3,fromVar.var3Length);
			//�z��̓Y������1����n�܂�̂�
			dim3 = (fromVar.var3Size * sizeof(t)) * (atoi(tmp3) - 1);
		}else{
			dim3 = (fromVar.var3Size * sizeof(t)) * (fromVar.actualFlg3 - 1);
		}
		//0����n�܂�̂� - 1?
		fromVar.curPnt = currentP + dim1 + dim2 + dim3;
		break;
	default:
		break;
	}


	return fromVar.curPnt;
}

//---------------------------------------------------------------------------------------------SD_getInputVarPnt
//�Y��������ϐ��̌��݂̒l�̃|�C���^��Ԃ�(input)
//author:n.koyama
//date  :20140611
//
char *SD_getInputVarPnt(struct inputObject inputVar,int length){

	char t;   //�ϐ��̃T�C�Y�p
	char *currentP;    //���ݗL�����Ǝv����f�[�^�̃|�C���^
	char tmp1[128] = "";
	char tmp2[128] = "";
	char tmp3[128] = "";
	int dim1;
	int dim2;
	int dim3;

	if(inputVar.bodyPnt != NULL){
		currentP = inputVar.bodyPnt->data;
	}
	switch(inputVar.iDim){
	case 0:
		inputVar.curPnt = currentP;
		break;
	case 1:
		if(inputVar.actualFlg1 == -99){
			//�|�C���^�̎��̓|�C���^�̐���ꎞ�ۑ��A����𐔒l��
			memcpy(tmp1,inputVar.var1,inputVar.var1Length);
			dim1 = (inputVar.var1Size * sizeof(t)) * (atoi(tmp1) - 1);
		}else{
			//�Y�����ԍ����ЂƂ���Ă��邩��
			dim1 = (inputVar.var1Size * sizeof(t)) * inputVar.actualFlg1;
		}
		inputVar.curPnt = currentP + dim1;
		break;
	case 2:
		if(inputVar.actualFlg1 == -99){
			//�|�C���^�̎��̓|�C���^�̐���ꎞ�ۑ��A����𐔒l��
			memcpy(tmp1,inputVar.var1,inputVar.var1Length);
			dim1 = (inputVar.var1Size * sizeof(t)) * (atoi(tmp1) - 1);
		}else{
			dim1 = (inputVar.var1Size * sizeof(t)) * inputVar.actualFlg1;
		}
		if(inputVar.actualFlg2 == -99){
			//�|�C���^�̎��̓|�C���^�̐���ꎞ�ۑ��A����𐔒l��
			memcpy(tmp2,inputVar.var2,inputVar.var2Length);
			dim2 = (inputVar.var2Size * sizeof(t)) * (atoi(tmp2) - 1);
		}else{
			dim2 = (inputVar.var2Size * sizeof(t)) * inputVar.actualFlg2;
		}
		inputVar.curPnt = currentP + dim1 + dim2;
		break;
	case 3:
		if(inputVar.actualFlg1 == -99){
			//�|�C���^�̎��̓|�C���^�̐���ꎞ�ۑ��A����𐔒l��
			memcpy(tmp1,inputVar.var1,inputVar.var1Length);
			dim1 = (inputVar.var1Size * sizeof(t)) * (atoi(tmp1) - 1);
		}else{
			dim1 = (inputVar.var1Size * sizeof(t)) * inputVar.actualFlg1;
		}
		if(inputVar.actualFlg2 == -99){
			//�|�C���^�̎��̓|�C���^�̐���ꎞ�ۑ��A����𐔒l��
			memcpy(tmp2,inputVar.var2,inputVar.var2Length);
			dim2 = (inputVar.var2Size * sizeof(t)) * (atoi(tmp2) - 1);
		}else{
			dim2 = (inputVar.var2Size * sizeof(t)) * inputVar.actualFlg2;
		}
		if(inputVar.actualFlg3 == -99){
			//�|�C���^�̎��̓|�C���^�̐���ꎞ�ۑ��A����𐔒l��
			memcpy(tmp3,inputVar.var3,inputVar.var3Length);
			dim3 = (inputVar.var3Size * sizeof(t)) * (atoi(tmp3) - 1);
		}else{
			dim3 = (inputVar.var3Size * sizeof(t)) * inputVar.actualFlg3;
		}
		inputVar.curPnt = currentP + dim1 + dim2 + dim3;
		break;
	default:
		break;
	}


	return inputVar.curPnt;
}

void SD_ObjectExistsInitData(cob_field *targObj){
	//PACKED�������ڂ̎��X�y�[�X����
	//COB_TYPE_NUMERIC_PACKED��COMP-3
	if(targObj->attr->type == COB_TYPE_NUMERIC_PACKED){
		int notSpaceExist = 0;
		int ii = 0;
		for(ii=0;ii < targObj->size;ii++){
			if(*(targObj->data + ii) != 0x20){
				notSpaceExist = 1;
			}
		}
		if(notSpaceExist == 0){
			cob_set_packed_zero(targObj);
		}
	}
}

//--------------------------------------------------------------------------------------------------------SD_Initialize

int SD_Initialize(){
	int i;
	struct screenObject tmp;
	SD_cobScreen = (struct screenObject *)malloc(sizeof(tmp) * SCR_OBJ_MAX);
	char strConfPath[1024]; //20150828 add koyama

	//�֐��������L�ϐ��ɃZ�b�g
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"SD_Initialize");

	memset(SD_cobScreen,'\0',sizeof(tmp) * SCR_OBJ_MAX);

	//�\���̃o�b�t�@�����Ȃ��ݒ�
	setvbuf( stdout, NULL, _IOLBF, 0 );

	//�����Ǘ��̊֐��Ăяo��
	if(MT_Initialize() == 1){
		return 1;
	}

	//�t�@�C���l�[�������Ƀ��[�_�|�C���^���쐬   //�t�@�C������ϐ��ɕύX 20150828
	xmlTextReaderPtr reader = xmlNewTextReaderFilename(getConfFilename(strConfPath));
	//���[�_�����[�h�ł����Ԃ�
	xmlTextReaderRead(reader);
	//���݂̃m�[�h�̃|�C���^���Z�b�g�H
	xmlTextReaderExpand(reader);
	//���݂̃m�[�h����DOM�����o���Ă���?
	xmlDocPtr doc = xmlTextReaderCurrentDoc(reader);
	if (!doc) return 1;
	//�h�L�������g����R���e�L�X�g()
	xmlXPathContextPtr ctx = xmlXPathNewContext(doc);
    if (!ctx) return 1;
	//xpath�Ŏw�肵���m�[�h���X�g���擾
	xmlXPathObjectPtr xpobj = xmlXPathEvalExpression((xmlChar *)CONF_DEFPATH, ctx);
    if (!xpobj) return 1;
	//�m�[�h���X�g���m�[�h�̔z��̂悤�Ȃ��̂�
	xmlNodeSetPtr nodes = xpobj->nodesetval;
	//�m�[�h���̎擾(�擾�ł��Ȃ��Ȃ�0)
	int size = (nodes) ? nodes->nodeNr : 0;
	//�m�[�h���X�g����l��\��
	for (i = 0; i < size; ++i) {
		if (!xmlXPathNodeSetIsEmpty(nodes)) {
			xmlNodePtr node = xmlXPathNodeSetItem(nodes, i);
			if (node->content) {
				//�ݒ�t�@�C�������ʒ�`�̃p�X���擾
				if(strcmp(node->parent->name,CONF_SCRPATH) == 0){
					strcpy(SD_screenDefPath,node->content);
				}
				//�ݒ�t�@�C������g���q���擾
				if(strcmp(node->parent->name,CONF_SCREXT) == 0){
					strcpy(SD_screenDefExt,node->content);
				}
				//debug�t���O������Ȃ�擾
				if(strcmp(node->parent->name,DEBUG_FLGNAME) == 0){
					//�Ԋ҂ł��Ȃ��������0�ɂȂ�͂�
					myConfDebugFlg = atoi(node->content);
				}
			} else {
				xmlXPathFreeObject(xpobj);
				xmlXPathFreeContext(ctx);
				xmlFreeDoc(doc);
				xmlFreeTextReader(reader);
				xmlCleanupParser();
				return 1;
			}
		}
	}
	xmlXPathFreeObject(xpobj);
	xmlXPathFreeContext(ctx);
	xmlFreeDoc(doc);
	xmlFreeTextReader(reader);
	xmlCleanupParser();
	//���L�ϐ������ɖ߂�
	unsetCommonFunctionName(map_source_func,strStack);
	return 0;
}

//cobol�̕ϐ���c���ŊǗ����邽�߂ɒ�`������
//
int SD_Init(char *argname,char *type,char *ychar,char *xchar,char *length,char *prevObjName,char *parentObjName){
	int i=0;
	char stry[128]="",strx[128]="";

	//�֐��������L�ϐ��ɃZ�b�g
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"SD_Init :%.10s",argname);

	//�����̌Œ�l�����������������ĂȂ��Ƃ��̓G���[��
	if(cob_call_params < 7){
		fprintf(stderr," Error C [%02d]: Option Missing  set option %d, need option is 7 target %s \n",99,cob_call_params,map_source_func);
		executeEnd();
	}

	//�����񂪕����萔���Ɩ��ɂȂ�̂�
	strcpy(stry,ychar);
	//�����񂪕����萔���Ɩ��ɂȂ�̂�
	strcpy(strx,xchar);

	if(SD_cobScreenLen > SCR_OBJ_MAX){
		fprintf(stderr," Error C [%02d]: I exceeds the maxsize screenObj \n",99);
		exit(1);
	}

	//�����ō�����ϐ��̏�����
	SD_cobScreen[SD_cobScreenLen].lineShift  = 0;
	SD_cobScreen[SD_cobScreenLen].ColShift  = 0;
	//�����ō�����|�C���^�̏�����
	SD_cobScreen[SD_cobScreenLen].nextObj   = (void *)0;
	SD_cobScreen[SD_cobScreenLen].parentObj = (void *)0;
	SD_cobScreen[SD_cobScreenLen].childObj  = (void *)0;
	//cobol�̕ϐ����i�[(�����ł�NULL�Œ�`���Ĉ�x�ł��g��ꂽ��i�[����)
	SD_cobScreen[SD_cobScreenLen].bodyPnt  = (void *)0;
	SD_cobScreen[SD_cobScreenLen].inputVar.iVarSize = 0;
	SD_cobScreen[SD_cobScreenLen].fromVar.iVarSize = 0;
	SD_cobScreen[SD_cobScreenLen].onViewFlg = 0;

	//�ϐ����̑��
	strcpy(SD_cobScreen[SD_cobScreenLen].cobargname,argname);

	//�s�ԍ��̑��
	if(((int)ychar[0] >= 48) && ((int)ychar[0] < 58)){
		SD_cobScreen[SD_cobScreenLen].y = atoi(ychar);
	}else{
		if(strstr(stry,"PLUS ") != 0&& strrchr(stry,' ') != NULL){
			//PLUS�����݂���Ƃ��Ă��炷
			SD_cobScreen[SD_cobScreenLen].lineShift = atoi((strrchr(stry,' ') + 1));
			SD_cobScreen[SD_cobScreenLen].y = 0;
			strcpy(SD_cobScreen[SD_cobScreenLen].yname,chrchange(stry,' ','\0'));
		}else{
			SD_cobScreen[SD_cobScreenLen].y = 0;
			strcpy(SD_cobScreen[SD_cobScreenLen].yname,stry);
		}
	}

	if(((int)xchar[0] >= 48) && ((int)xchar[0] < 58)){

		SD_cobScreen[SD_cobScreenLen].x = atoi(xchar);
	}else{
		if(strstr(strx,"PLUS ") != 0 && strrchr(strx,' ') != NULL){
			//PLUS�����݂���Ƃ��Ă��炷 [arg] PLUS [VAL]�Ȃ̂œ�ڂ̃X�y�[�X
			SD_cobScreen[SD_cobScreenLen].ColShift = atoi((strrchr(strx,' ') + 1));
			SD_cobScreen[SD_cobScreenLen].x = 0;
			strcpy(SD_cobScreen[SD_cobScreenLen].xname,chrchange(strx,' ','\0'));
		}else{
			SD_cobScreen[SD_cobScreenLen].x = 0;
			strcpy(SD_cobScreen[SD_cobScreenLen].xname,strx);
		}
	}

	//�^�̊i�[
	strcpy(SD_cobScreen[SD_cobScreenLen].cobtype,type);

	SD_cobScreen[SD_cobScreenLen].length = atoi(length);


	//�O�̃I�u�W�F�N�g������ꍇ�A�O�̃I�u�W�F�N�g��T���Ă���next�̃|�C���^���i�[
//	printf("prev:%s  parent:%s scrLen:%d \n",prevObjName,parentObjName,SD_cobScreenLen);
	if(strcmp(prevObjName," ") != 0){
		//��납�猩�邱�Ƃœ������O�̎��͒��߂̂��̂��݂�
		for(i=SD_cobScreenLen;i >= 0;i--){
			if(strcmp(SD_cobScreen[i].cobargname,prevObjName) == 0){
//				printf("%s %s\n",SD_cobScreen[i].cobargname,prevObjName);
				SD_cobScreen[i].nextObj = &SD_cobScreen[SD_cobScreenLen];
				break;
			}
		}
	}

	//�e�̃I�u�W�F�N�g������ꍇ�A�O�̃I�u�W�F�N�g��T���Ă���next�̃|�C���^���i�[
	if(strcmp(parentObjName," ") != 0){
		//��납�猩�邱�Ƃœ������O�̎��͒��߂̂��̂��݂�
		for(i=SD_cobScreenLen;i >= 0;i--){
			if(strcmp(SD_cobScreen[i].cobargname,parentObjName) == 0){
				if(strcmp(SD_cobScreen[i].cobargname,SD_cobScreen[(SD_cobScreenLen - 1)].cobargname) == 0){
					SD_cobScreen[SD_cobScreenLen].parentObj = &SD_cobScreen[i];
					SD_cobScreen[i].childObj = &SD_cobScreen[SD_cobScreenLen];
					break;
				}else{
					//�e�q�֌W�̑Ή�
					//�e�w��͕K�����O�ł���R�[�f�B���O�̂͂��Ȃ̂�-1�ƈ�v���Ȃ��̂͂��������͂��H
					fprintf(stderr," Error C [%02d]: Parent-child relationship : %s's parent is %s ? \n",99,SD_cobScreen[SD_cobScreenLen].cobargname,SD_cobScreen[(SD_cobScreenLen - 1)].cobargname);
					executeEnd();
				}
			}
		}
	}
	SD_cobScreenLen += 1;

	//���L�ϐ������ɖ߂�
	unsetCommonFunctionName(map_source_func,strStack);
	return 0;
}

//�s���ς̏ꍇ(���g�p)
int SD_Init_Linename(char *argname,char *linename){
	int i=0;
	//�Ώۂ̃I�u�W�F�N�g������ꍇ�A�O�̃I�u�W�F�N�g��T���Ă���next�̃|�C���^���i�[
	for(i=0;i < SD_cobScreenLen;i++){
		if(strcmp(SD_cobScreen[i].cobargname,argname) == 0){
			strcpy(SD_cobScreen[i].yname,linename);
			break;
		}
	}
	return 0;
}

//�s���ς̏ꍇ
int SD_Init_Colname(char *argname,char *colname){
	int i=0;
	//�Ώۂ̃I�u�W�F�N�g������ꍇ�A�O�̃I�u�W�F�N�g��T���Ă���next�̃|�C���^���i�[
	for(i=0;i < SD_cobScreenLen;i++){
		if(strcmp(SD_cobScreen[i].cobargname,argname) == 0){
			strcpy(SD_cobScreen[i].xname,colname);
			break;
		}
	}
	return 0;
}


//��ʒ�`���o�͌`���ɍ��킹�ďo��
//scrSrcPath:�Ώۂ̉�ʒ�`�̃t���p�X
int SD_Screen_Output(char *scrSrcPath){
	FILE *fp;
	char fpath[512];
	char filname[256] = "";    //�t�@�C�����̎󂯎��p�ɕK�v
	char s[256];

	//�t�@�C���̃p�X���R�s�[
	strcpy(fpath,SD_screenDefPath);

	//�啶���ŊJ�����s
	strcat(filname,scrSrcPath);

	//��ʒ�`�̖��O��ǉ�
	strcat(fpath,filname);
	//�g���q��ǉ�
	strcat(fpath,SD_screenDefExt);

	if((fp = fopen(fpath,"r")) == NULL){
		//�t�@�C���̃p�X���R�s�[
		strcpy(fpath,SD_screenDefPath);
		//�����������ɕϊ���retry
		StrToLowerCpy(filname,scrSrcPath);
		//��ʒ�`�̖��O��ǉ�
		strcat(fpath,filname);
		//�g���q��ǉ�
		strcat(fpath,SD_screenDefExt);

		if((fp = fopen(fpath,"r")) == NULL){
			//�t�@�C���̃I�[�v�����ł��Ȃ�������֐����I��
			printf(" Error C [%02d]:can't open screen file %s \n",99,fpath);
			return 1;
		}
	}

	//  (4)�t�@�C���̓ǂ݁i�����j
	while (fgets(s, 256, fp) != NULL) {
		char *lastChar = NULL;
		// �����ł�fgets()�ɂ��P�s�P�ʂœǂݏo��
		//76�����ڈȍ~�͔F�����Ȃ����Ƃɂ���
		s[75] = '\0';
		//_or)���Ō�ɂȂ�
		lastChar      = (char *)strrchr(s,'_');
		if(lastChar == NULL){
			lastChar      = (char *)strrchr(s,')');
		}
		//_��)�������Ƃ���75�����ڂ܂ł����̂܂܏o��
		if(lastChar != NULL){
			*lastChar     = '\n';
			*(lastChar+1) = '\0';
		}
		printf("%s", s);
	}
	fclose(fp);   // (5)�t�@�C���̃N���[�Y
	return 0;
}

//��ʂ���̒l��ϐ��ɑ���֐�(cob�ϐ���)
//in : targScreenObj:�R�s�[�Ώ�,inputArg:�R�s�[��������,recursiveLever:�����Ăяo����0��0�Ȃ�Z������Ȃ�
//author:koyama 20151019
void setDataRelation(struct screenObject *targScreenObj,cob_field *inputArg,char *status,int recursiveLever){
	struct screenObject *temporaryObj;
	temporaryObj = targScreenObj;

	while(temporaryObj != NULL){
		//�q������Ȃ�q���ċA�I��
		if(temporaryObj->childObj != NULL){
			setDataRelation(temporaryObj->childObj,inputArg,status,(recursiveLever + 1));
		}
		if(temporaryObj->inputVar.iVarSize != 0 || temporaryObj->usingVar.iVarSize != 0){
			//�L�����Z���n�̏����̎��͒��g�������ׂ��H   check koyama ::
//			if(strncmp(status,"06",strlen("06")) != 0){
				//attr�͐錾
				cob_field_attr  a_from = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
				cob_field_attr  a_to = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
				//cob_move�ňړ����Ȃ��Ƃ��ꂽ���͑Ή��ł��Ȃ�
				cob_field       d_to;
				cob_field       d_from;
				struct inputObject tempVar;
				a_from.type = inputArg->attr->type;
				a_from.digits=inputArg->attr->digits;
				a_from.flags =inputArg->attr->flags;
				a_from.scale =inputArg->attr->scale;

				if(temporaryObj->inputVar.iVarSize != 0){
					tempVar = temporaryObj->inputVar;
				}else{
					tempVar = temporaryObj->usingVar;
				}

				a_from.digits = temporaryObj->length;

				d_from.size   = temporaryObj->length;
				d_from.data   = inputArg->data;
				//��œ����ꂽ���̂𗘗p
				d_from.attr   = &a_from;

				d_to.size = tempVar.bodyPnt->size;
				d_to.data = SD_getInputVarPnt(tempVar,temporaryObj->length);
				d_to.attr = tempVar.bodyPnt->attr;

				cob_move(&d_from,&d_to);
//			}
		}

		//�ċA0�i�ڂȂ玟�ɍs���Ȃ�
		if(recursiveLever == 0){
			break;
		}

		temporaryObj = temporaryObj->nextObj;
	}
}

//��ʂ���̒l��ϐ��ɑ���֐�(cob�ϐ���)
//in : buff:�R�s�[��������,targObj:�R�s�[��I�u�W�F�N�g,setArg:
//author:koyama 20150310 �d�l�ύX���K�v�Ȃ��ߍ�蒼��
void setDataCobarg(char *buff,struct screenObject *targScreenObj,cob_field *inputArg,char *status){
	char *term_buff;
	char t[1];
	cob_field *targObj;
	targObj = targScreenObj->bodyPnt;

	//�֐��������L�ϐ��ɃZ�b�g
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"setDataCobarg ");

	//������
	term_buff = buff;

	cob_field_attr  a_from = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
	cob_field_attr  a_to   = {COB_TYPE_NUMERIC, 0, 0, 0, NULL};
	cob_field       d_from;
	cob_field       d_to;

	//���̕ϐ��̑������R�s�[
	a_to.type  = targObj->attr->type;
	a_to.flags = targObj->attr->flags;
	a_to.digits= targObj->attr->digits;
	a_to.scale = targObj->attr->scale;
	a_to.pic   = targObj->attr->pic;

	//���茳��type��COB_TYPE_ALPHANUMERIC�Ȃ̂ŕς��Ȃ�
	//���茳�̒����͈ꉞ�����񒷂Őݒ�
	//���g��������
	a_from.flags = 0;
	a_from.scale = 0;
	a_from.digits = strlen(term_buff);
	if(a_to.type == COB_TYPE_NUMERIC){
//		a_from.type = COB_TYPE_NUMERIC;
		//�h�b�g��T������΂���ȍ~�������_��
		if(strchr(term_buff,0x2e) != 0){
			//\0�Ƃ̔�r�Ȃ̂�-1
			a_from.type = COB_TYPE_NUMERIC_DISPLAY;
			a_from.scale = (strchr(term_buff,0x00) - strchr(term_buff,0x2e)) - 1;
			remTargChar(term_buff,0x2e);
		}else{
			a_from.scale = a_to.scale;
			//���͂ɏ����_�̕���������0���� ���߂�K�v�͂Ȃ� upd koyama 20170829
			// addAfterTargChar(term_buff,'0',(int)a_to.scale);
		}
	}
	if(strncmp(status,"01",strlen("01")) == 0){
		//HTab
		a_from.digits=strlen(term_buff);
	}else if(strncmp(status,"06",strlen("06")) == 0){
		//skip
		if(a_to.type == COB_TYPE_NUMERIC){
			a_from.type = COB_TYPE_NUMERIC_DISPLAY;
		}
		a_from.digits=targObj->size;
	}

	//�����ɃR�s�[
	d_to.size   = targObj->size;
	d_to.data   = inputArg->data;
	d_to.attr   = &a_to;
	d_from.size = a_from.digits;
	d_from.data = term_buff;
	d_from.attr = &a_from;
	cob_move(&d_from,&d_to);
	//���ꂽ����attr�������ċA��
	inputArg->size = d_to.size;
	inputArg->attr = &a_to;
	//���L�ϐ������ɖ߂�
	unsetCommonFunctionName(map_source_func,strStack);

}


//�󕶎���̎��A���̒l�����̂��̂��Z�b�g
//in : temporaryObj:���̒l�����o���I�u�W�F�N�g,targObj:�R�s�[��I�u�W�F�N�g,setArg:
//author:koyama 20150310 �d�l�ύX���K�v�Ȃ��ߍ�蒼��
int influenceWhenEmptyString(char *inputArg,struct screenObject *temporaryObj,cob_field *d_from){
	int retVal = 0;
	char *strPic;
	//��ɂ��鏈���ɕύX
	//�f�[�^�̑}���ʒu���n�_����ǂꂾ�����炷��
	cob_field       d_null;
	cob_field       d_to;
	//attr�͋��L����̂ŊO�Ő錾
	cob_field_attr  a_null = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
	cob_field_attr  a_to = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
	//����쐬(d_from��body�������Ă���ꍇ����̂ŁA�쐬������
	a_null.digits  = temporaryObj->length;
	d_null.data    = malloc(temporaryObj->length + 1);
	memset(d_null.data,'\0',(temporaryObj->length + 1));
	memset(d_null.data,' ',temporaryObj->length);
	d_null.size    = temporaryObj->length;
	d_null.attr    = &a_null;
	//�T�C�Y����Ȃ�R�s�[����Ȃ��̂�
	d_to.size = 0 ;
	//�l�̔��f��input��using�ɍs��
	if(temporaryObj->fromVar.iVarSize != 0){
		d_to.size    = temporaryObj->fromVar.iVarSize;
		a_to.digits  = temporaryObj->fromVar.iVarSize;
		d_to.data    = SD_getFromVarPnt(temporaryObj->fromVar,temporaryObj->fromVar.iVarSize);
		d_to.attr    = &a_to;
	}else if(temporaryObj->inputVar.iVarSize != 0){
		// d_to.size    = temporaryObj->inputVar.iVarSize;
		// a_to.digits  = temporaryObj->inputVar.iVarSize;
		// d_to.data    = SD_getInputVarPnt(temporaryObj->inputVar,temporaryObj->inputVar.iVarSize);
		// d_to.attr    = &a_to;
	}else if(temporaryObj->usingVar.iVarSize != 0){
		d_to.size    = temporaryObj->usingVar.iVarSize;
		a_to.digits  = temporaryObj->usingVar.iVarSize;
		d_to.data    = SD_getInputVarPnt(temporaryObj->usingVar,temporaryObj->usingVar.iVarSize);
		if(temporaryObj->usingVar.bodyPnt != NULL){
			d_to.attr    = temporaryObj->usingVar.bodyPnt->attr;
		}else{
			d_to.attr    = &a_to;
		}
	}
	//�f�[�^move�Ń��Z�b�g���鏈��.d_from�̓|�C���^���Ȃ̂�
	cob_move(&d_null,d_from);
	cob_move(d_from,&d_to);

	free(d_null.data);

	//�Ō�ɒl���ʂ��ďI��
	memcpy(inputArg,d_to.data,d_to.size);
	return retVal;
}

//��ʂ���̒l��ϐ��ɑ���֐�
//in : buff:�R�s�[��������,targObj:�R�s�[��I�u�W�F�N�g,setArg:
//author:koyama 20150310 �d�l�ύX���K�v�Ȃ��ߍ�蒼��
void setDataRecursive(char *buff,struct screenObject *targObj,char *inputArg,char *status){
	struct screenObject *temporaryObj;
	char *term_buff;
	char t[1];

	//�֐��������L�ϐ��ɃZ�b�g
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"setDataRecursive ");

	//������
	term_buff = buff;
	temporaryObj = targObj;

	//�f�[�^�̑}���ʒu���n�_����ǂꂾ�����炷��
	cob_field       d_from;
	cob_field       d_to;
	//attr�͋��L����̂ŊO�Ő錾
	cob_field_attr  a_from = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
	cob_field_attr  a_to = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};

	//���ƂŔėp�I�Ɏg�����߂ɃZ�b�g
	if(targObj->bodyPnt == NULL){
		d_from.size  = targObj->length;
		d_from.data  = inputArg;
		d_from.attr  = &a_from;
	}else{
		d_from.size  = targObj->bodyPnt->size;
		d_from.data  = targObj->bodyPnt->data;
		a_from.type  = targObj->bodyPnt->attr->type;
		a_from.digits= targObj->bodyPnt->attr->digits;
		a_from.scale = targObj->bodyPnt->attr->scale;
		a_from.pic   = targObj->bodyPnt->attr->pic;
		d_from.attr  = &a_from;
	}

	//ENTER or TAB�ȊO�̎��͓��͂Ƃ݂Ȃ��Ȃ�
	if((strncmp(status,"01",strlen("01")) == 0 && strncmp(status,"06",strlen("06")) == 0)){
		//���L�ϐ������ɖ߂�
		unsetCommonFunctionName(map_source_func,strStack);
		return ;
	}
	//(�����̒�����0)�Ⴕ���͑S�Ă���(Enter�Ŋm�肳�ꂽ���ȊO) (TODO:�X�y�[�X�݂̂���͂���P�[�X������ꍇout)
	if((strlen(term_buff) == 0 || isNullOrEmpty(term_buff) == 1) && strncmp(status,"01",strlen("01")) != 0){
		//���g��������
		influenceWhenEmptyString(inputArg,targObj,&d_from);
		//���L�ϐ������ɖ߂�
		unsetCommonFunctionName(map_source_func,strStack);
		return ;
	}
	if(targObj->bodyPnt != NULL){
		//cobarrg�����݂���Ȃ�
		setDataCobarg(term_buff,targObj,&d_from,status);
		//���Ŏg�����߂ɒl�R�s�[
		a_to.type   = targObj->bodyPnt->attr->type;
		a_to.digits = targObj->bodyPnt->attr->digits;
		a_to.scale  = targObj->bodyPnt->attr->scale;
		a_to.flags  = targObj->bodyPnt->attr->flags;

	}else if(strchr(temporaryObj->cobtype,'Z') != 0 || strchr(temporaryObj->cobtype,'-') != 0){
		//���l��������(�t�H�[�}�b�g�t��������)
		//���g��������
		memset(inputArg, ' ', temporaryObj->length);
		//���l�̂Ƃ��͑}���̊J�n�ʒu�����炷(��낪�����Ȃ̂ł��̂܂�-)
		memcpy((inputArg + (temporaryObj->length - strlen(term_buff))), term_buff, strlen(term_buff));

	}else if(strchr(temporaryObj->cobtype,'9') != 0){
		//����(Z,-���܂܂Ȃ�������)
		//�^�ɍ��킹��
		if(strchr(temporaryObj->cobtype,'S') == 0){
			//S9�ł͂Ȃ�
			a_to.type = (unsigned char)COB_TYPE_NUMERIC;
			//�����Ȃ��Ȃ̂�0
			a_to.flags=0;
		}else{
			//��ʃI�u�W�F�N�g��COMP3�͑��݂ł��Ȃ��͂�
			//�����t����(S9)
			a_to.type = (unsigned char)COB_TYPE_NUMERIC;
			//�����Ȃ��Ȃ̂�0
			a_to.flags=1;
		}
		//�����̒�����type���擾
		a_to.digits=temporaryObj->length;
		a_to.pic = NULL;

		//���茳��type��COB_TYPE_ALPHANUMERIC�Ȃ̂ŕς��Ȃ�
		//���茳�̒����͈ꉞ�����񒷂Őݒ�
		a_from.digits=strlen(term_buff);
		a_from.flags = 0;
		a_from.scale = 0;

		//���g��������
		memset(inputArg, '0', temporaryObj->length);

		//�����ɃR�s�[
		d_from.size = strlen(term_buff);
		d_from.data = term_buff;
		// ��ɃZ�b�g�����̂�
		// d_from.attr = &a_from;
		d_to.size   = temporaryObj->length;
		d_to.data   = inputArg;
		d_to.attr   = &a_to;
		cob_move(&d_from,&d_to);
	}else if(strchr(temporaryObj->cobtype,'N') != 0||strchr(temporaryObj->cobtype,'X') != 0){
		//�^�������
		a_from.type = COB_TYPE_NATIONAL;
		a_to.type = COB_TYPE_NATIONAL;

		//�����̒�����type���擾
		a_to.digits = temporaryObj->length;
		a_to.pic    = NULL;

		//���茳��type��COB_TYPE_ALPHANUMERIC�Ȃ̂ŕς��Ȃ�
		//���茳�̒����͈ꉞ�����񒷂Őݒ�
		a_from.digits = strlen(term_buff);
		a_from.flags  = 0;
		a_from.scale  = 0;

		//���g��������
		if(strncmp(status,"01",strlen("01")) == 0){
			//HTab
			memset2byte(inputArg, "�@", strlen(term_buff));
		}else if(strncmp(status,"06",strlen("06")) == 0){
			//skip
			memset2byte(inputArg, "�@", temporaryObj->length);
		}

		//�����ɃR�s�[
		d_from.size = strlen(term_buff);
		d_to.size   = temporaryObj->length;
		d_to.data   = inputArg;
		d_from.data = term_buff;
		d_to.attr   = &a_to;
		// d_from.attr = &a_from;    //��ɃZ�b�g�����̂�
		cob_move(&d_from,&d_to);
	}else{
		//���g��������
		memset(inputArg, ' ', temporaryObj->length);
		memcpy(inputArg, term_buff,  strlen(term_buff));
	}

	setDataRelation(temporaryObj,&d_from,status,0);
	//���L�ϐ������ɖ߂�
	unsetCommonFunctionName(map_source_func,strStack);
}



//�X�e�[�^�X�̃Z�b�g�ƕ����̕s�v�����̏�������֐�
//in:(ref)term_buff,(ref)eStatus
void setStatusASplit(char *term_buff,char *eStatus){
	char *split_start=NULL;
	//��strchr�������Ɩ�肪����̂Ń|�C���^���擾
	//`�����݂�,��������̒�����`�v���X2����+���s�ȓ�
	if(strrchr(term_buff,'`') != 0 && (strlen(strrchr(term_buff,'`')) < 5  || strcmp(strrchr(term_buff,'`'),"`esc") == 1)){
		split_start = strrchr(term_buff,'`');
	}
	//���ɃX�e�[�^�X���Ȃ��Ƃ��͉��s����������
	if(split_start != NULL){
		if(strcmp((split_start + 1),"N") == 0){
			//NOC
			strcpy(eStatus,"00");
		}else if(strncmp((split_start + 1),"h",strlen("h")) == 0){
			//HTB
			strcpy(eStatus,"01");
		}else if(strncmp((split_start + 1),"c1",strlen("c1")) == 0){
			//C1
			strcpy(eStatus,"02");
		}else if(strncmp((split_start + 1),"c2",strlen("c2")) == 0){
			//C2
			strcpy(eStatus,"03");
		}else if(strncmp((split_start + 1),"a",strlen("a")) == 0){
			//ADV
			strcpy(eStatus,"04");
		}else if(strncmp((split_start + 1),"f",strlen("f")) == 0){
			//FUK
			strcpy(eStatus,"05");
		}else if(strncmp((split_start + 1),"s",strlen("s")) == 0){
			//SKP
			strcpy(eStatus,"06");
		}else if(strncmp((split_start + 1),"b",strlen("b")) == 0){
			//BTB
			strcpy(eStatus,"09");
		}else if(strncmp((split_start + 1),"5",strlen("5")) == 0){
			//PF5
			strcpy(eStatus,"P5");
		}else if(strncmp((split_start + 1),"6",strlen("6")) == 0){
			//PF6
			strcpy(eStatus,"P6");
		}else if(strncmp((split_start + 1),"7",strlen("7")) == 0){
			//PF7
			strcpy(eStatus,"P7");
		}else if(strncmp((split_start + 1),"8",strlen("8")) == 0){
			//PF8
			strcpy(eStatus,"P8");
		}else if(strncmp((split_start + 1),"9",strlen("9")) == 0){
			//PF9
			strcpy(eStatus,"P9");
		}else if(strncmp((split_start + 1),"10",strlen("10")) == 0){
			//PF10
			strcpy(eStatus,"PA");
		}else if(strncmp((split_start + 1),"14",strlen("14")) == 0){
			//PF14
			strcpy(eStatus,"PE");
		}else if(strncmp((split_start + 1),"17",strlen("17")) == 0){
			//PF17
			strcpy(eStatus,"FW");
		}else if(strncmp((split_start + 1),"18",strlen("18")) == 0){
			//PF18
			strcpy(eStatus,"BW");
		}else if(strncmp((split_start + 1),"21",strlen("21")) == 0){
			//PF21
			strcpy(eStatus,"UP");
		}else if(strncmp((split_start + 1),"22",strlen("22")) == 0){
			//PF22
			strcpy(eStatus,"DW");
		}else if(strncmp((split_start + 1),"1",strlen("1")) == 0){
			//PF1
			strcpy(eStatus,"P1");
		}else if(strncmp((split_start + 1),"2",strlen("2")) == 0){
			//PF2
			strcpy(eStatus,"P2");
		}else if(strncmp((split_start + 1),"3",strlen("3")) == 0){
			//PF3
			strcpy(eStatus,"P3");
		}else if(strncmp((split_start + 1),"4",strlen("4")) == 0){
			//PF4
			strcpy(eStatus,"P4");
		}else if(strncmp((split_start + 1),"d",strlen("d")) == 0){
			//DW
			strcpy(eStatus,"DW");
		}else if(strncmp((split_start + 1),"esc",strlen("esc")) == 0){
			//DW
			strcpy(eStatus,"ESC");
		}
		// �X�v���b�^����������O�ɉ��s�R�[�h������
		if(strchr(term_buff,'\n') != 0){
			term_buff[(strchr(term_buff,'\n') - term_buff)] = '\0';
		}
		//���Ղ�������폜(���̎��ɉ��s�R�[�h�Ȃǂ����������)
		memset(split_start ,'\0',strlen(strchr(term_buff,'`')));
	}else{
		//���Ղ�������폜(���̎��ɉ��s�R�[�h�Ȃǂ����������)
		//�X�e�[�^�X�̋L�q���Ȃ���΃G���^�[���Ƃ������Ƃɂ���
		strcpy(eStatus,"01");
		if(strchr(term_buff,'\n') != 0){
			memset(strchr(term_buff,'\n'),'\0',strlen(term_buff));
		}
	}
}

//��ʂ���̎󂯎�����l�������̒l�ƍ�����(status��SKIP�̂Ƃ�)
//in : temporaryObj �������ʃI�u�W�F�N�g,inArgPos ���͎��̃f�t�H���g,input_term����
//author : koyama 20170314
int mergeInputValueConditionSkip(struct screenObject *temporaryObj,char *inArgPos,char *input_term){
	int varLength=0,intLengthDiff=0;
	char copyStr[2048]="";
	char *tempStr = copyStr;
	int  CompFlg = 0;
	cob_field_attr  a_from = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
	cob_field_attr  a_to = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
	cob_field       d_from = {0,NULL,NULL};
	cob_field       d_to = {0,NULL,NULL};
	cob_field       *from_base=NULL;

	d_from.size = temporaryObj->length;
	if(temporaryObj->fromVar.iVarSize != 0){
		inArgPos      = SD_getFromVarPnt(temporaryObj->fromVar,temporaryObj->length);
		varLength     = temporaryObj->fromVar.iVarSize;
		//comp�Ȃ�R�s�[�̎���
		if(temporaryObj->fromVar.bodyPnt != NULL
			&& temporaryObj->fromVar.bodyPnt->attr->type == COB_TYPE_NUMERIC_PACKED){
			from_base   = temporaryObj->fromVar.bodyPnt;
			d_from.size = temporaryObj->fromVar.iVarSize;
		}
	}else if(temporaryObj->usingVar.iVarSize != 0){
		inArgPos = SD_getInputVarPnt(temporaryObj->usingVar,temporaryObj->length);
		varLength = temporaryObj->usingVar.iVarSize;
		//comp�Ȃ�R�s�[�̎���
		if(temporaryObj->usingVar.bodyPnt != NULL
			&& temporaryObj->usingVar.bodyPnt->attr->type == COB_TYPE_NUMERIC_PACKED){
			from_base   = temporaryObj->usingVar.bodyPnt;
			d_from.size = temporaryObj->usingVar.iVarSize;
		}
	}else{
		memset(inArgPos,' ',strlen(inArgPos));
		varLength   = temporaryObj->length;
	}
	//������size��form�̃f�[�^���ɕ����Ȃ���΃_���H
	a_to.digits = varLength;
	d_to.size   = temporaryObj->length;
	d_to.data   = tempStr;
	d_to.attr   = &a_to;
	d_from.data = inArgPos;
	d_from.attr = &a_from;
	if(from_base != NULL){
		//COMP3�͕K�������Ȃ̂�
		a_to.type     = COB_TYPE_NUMERIC_DISPLAY;
		//input OR From�Ȃ̂Ō��̑����ɂ��킹��(type�𐔎��ɂ����̂ŁA�K�v�ɂȂ�)
		a_from.type     = from_base->attr->type;
		a_from.digits = from_base->attr->digits;
		a_from.scale  = from_base->attr->scale;
		a_from.flags  = from_base->attr->flags;
		//COMP3�̏ꍇ�o�C�g���ƃf�[�^�̒������Ⴄ�̂�
		// varLength     = from_base->attr->digits;
		varLength     = temporaryObj->length;
		if(SD_isPackedFormat(&d_from) == 0){
			//COMP3�Ȃ̂�comp3�Ƃ��Đ������l�ł͂Ȃ�������
			cob_set_packed_zero (&d_from);
		}
	}
	cob_move(&d_from,&d_to);
	// strncat(tempStr,inArgPos,varLength);

	//���͂Əo�͂̃T�C�Y���Ⴄ��
	intLengthDiff = varLength - temporaryObj->length;
	//intLengthDiff���Ӗ����Ⴄ�悤�Ȃ̂Œ��� upd koyama
	if(intLengthDiff >= 0){
		strcat(input_term,tempStr + (strlen(input_term) + intLengthDiff));
	}else{
		memset(input_term,' ',abs(intLengthDiff));
		strcat(input_term,tempStr + (strlen(input_term)));
	}

	return 0;
}

//��ʂ���̎󂯎�����l�������̒l�ƍ�����(status��PF5�̂Ƃ�)
//in : temporaryObj �������ʃI�u�W�F�N�g,inArgPos ���͎��̃f�t�H���g,input_term����
//author : koyama 20170314
int mergeInputValueConditionPF5(struct screenObject *temporaryObj,char *inArgPos,char *input_term){
	int varLength = 0;
	int relationFlg = 0;
	//using,from������Ƃ��͂��̒l���g��
	if(temporaryObj->fromVar.iVarSize != 0){
		// inArgPos = SD_getFromVarPnt(temporaryObj->fromVar,temporaryObj->length);
		relationFlg=1;
		varLength = temporaryObj->fromVar.iVarSize;
	}else if(temporaryObj->usingVar.iVarSize != 0){
		// inArgPos = SD_getInputVarPnt(temporaryObj->usingVar,temporaryObj->length);
		relationFlg=1;
		varLength = temporaryObj->usingVar.iVarSize;
	}
	// PF5�̂Ƃ���relation�̒l��
	if(relationFlg != 0){
		strncpy(input_term,inArgPos,varLength);
	}

	return 0;
}

//��ʂ���̎󂯎�����l�������̒l�ƍ�����(status��Enter�̂Ƃ�)
//in : temporaryObj �������ʃI�u�W�F�N�g,inArgPos ���͎��̃f�t�H���g,input_term����
//author : koyama 20170314
int mergeInputValueConditionEnter(struct screenObject *temporaryObj,char *inArgPos,char *input_term){
	int varLength = 0;
	//using,from������Ƃ��͂��̒l���g��
	if(temporaryObj->fromVar.iVarSize != 0){
		inArgPos = SD_getFromVarPnt(temporaryObj->fromVar,temporaryObj->length);
		varLength = temporaryObj->fromVar.iVarSize;
	}else if(temporaryObj->usingVar.iVarSize != 0){
		inArgPos = SD_getInputVarPnt(temporaryObj->usingVar,temporaryObj->length);
		varLength = temporaryObj->usingVar.iVarSize;
	}else{
		memset(inArgPos,' ',strlen(inArgPos));
		varLength = temporaryObj->length;
	}
	strncpy(input_term,inArgPos,varLength);

	return 0;
}


//��ʂ���̒l����֐�
//in : buff,targObj
void getDataRecursive(char *buff,struct screenObject *targObj,char *status,char *inputArg,int recursiveLever){
	struct screenObject *temporaryObj;
	char term_buff[BUFF_SIZE]="";
	char *inArgPos = inputArg;

	//�֐��������L�ϐ��ɃZ�b�g
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"getDataRecursive ");

	temporaryObj = targObj;

	while(temporaryObj != NULL){
		//�q������Ȃ�q���ċA�I��
		if(temporaryObj->childObj != NULL){
			getDataRecursive(term_buff,temporaryObj->childObj,status,inArgPos,(recursiveLever + 1));
			//���̒l������ꏊ�͌��݂�Obj�̒��������炷
			inArgPos = inArgPos + temporaryObj->length;
		}else{
			SD_AcceptStdin(temporaryObj,term_buff);

			//�X�e�[�^�X�̃Z�b�g�ƕ����̕s�v�����̏���
			setStatusASplit(term_buff,status);
			//�G���^�[�ł͖�����
			if(strncmp(status,"01",strlen("01")) != 0){
				//TAB�ł͖�����
				if(strncmp(status,"06",strlen("06")) == 0){
					//using,from������Ƃ��͂��̒l���g��
					//20170314 �����̊ȗ����̂��ߊ֐���
					mergeInputValueConditionSkip(temporaryObj,inArgPos,term_buff);
					setDataRecursive(term_buff,temporaryObj,inArgPos,status);
				}else if(strncmp(status,"P5",strlen("P5")) == 0){
					//PF5�j(F5)�̂Ƃ��͒l�������Ă���
					mergeInputValueConditionPF5(temporaryObj,inArgPos,term_buff);
					//�f�[�^���ڂ��Ȃ��p�^�[��
					setDataRecursive(term_buff,temporaryObj,inArgPos,status);
				//using��input������Ƃ��ɂ͈��p���Ȃ� TODO::koyama 20161116
					//20170314 �����̊ȗ����̂��ߊ֐���
					// mergeInputValueConditionEnter(temporaryObj,inArgPos,term_buff);
					//
				}
			}else{
				//01(Enter�̏ꍇ�͂���ő���)���͂�����
//					strncpy(term_buff,inArgPos,temporaryObj->length);
				setDataRecursive(term_buff,temporaryObj,inArgPos,status);
			}
			// setDataRecursive(term_buff,temporaryObj,inArgPos,status);
		}
		strncat(buff,term_buff,strlen(term_buff));

		if(recursiveLever == 0){
			break;
		}

		temporaryObj = temporaryObj->nextObj;
	}

	//�擾�����l���R�s�[
	//	strcpy(inputArg,buff);
	//���L�ϐ������ɖ߂�
	unsetCommonFunctionName(map_source_func,strStack);
}


//SD_Accept �W�����͎󂯎��̍ۂɃR�[�h������
int SD_Accept(char *inputArg,char *argName,char *type,char *size,char *eStatus){
	//cobc��8192�Ŏ���Ă����̂�
	char term_buff[BUFF_SIZE]="";int counter=0,argumentFlg = 0;
	struct screenObject *targObject;
	int iSize = atoi(size),i=0;

	//�֐��������L�ϐ��ɃZ�b�g
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"SD_Accept :%.10s",inputArg);

	//�_�~�[�̕ϐ����쐬
	// type, digits, scale, flags, pic
	const cob_field_attr a_dummy = {1, 0, 0, 0, NULL};
	unsigned char b_dummy[128]="";//__attribute__((aligned))�͕ϐ���Ȃ̂ł���Ȃ�
	cob_field f_dummy = {128, b_dummy, &a_dummy};

	//Search target
	targObject = searchTargetObject(SD_cobScreen,argName,SD_cobScreenLen);

	//��������̎󂯎��̂Ƃ�
	cob_accept_arg_value(&f_dummy);
	//���͂�cobol arg���Z�b�g
	targObject->bodyPnt = cob_current_module->cob_procedure_parameters[0];

	//���pSP����������Ȃ��Ƃ��͉��炩�̃G���[����(NULL�ƕ�����|�C���^���r�ł��Ȃ�����)
	//�擪���������p�X�y�[�X�łȂ��Ƃ�
	if( (strchr(b_dummy,0x20) == NULL) &&  ( (char *)b_dummy != strchr(b_dummy,0x20) ) ){
		struct screenObject *temporaryObject; //�q�������e�̂Ƃ��̂��߂ɒl����p
		temporaryObject = targObject;

		getDataRecursive(term_buff,temporaryObject,eStatus,inputArg,0);
	}else{
		//��������̓��͂̂Ƃ�
		strncpy(term_buff,b_dummy,(strchr(b_dummy,0x20) - (char *)b_dummy));
		//HTB
		strcpy(eStatus,"01");
		//�l���Z�b�g����
		setDataRecursive(term_buff,targObject,inputArg,eStatus);
	}

	//�m��{�^���ȊO�͉�ʕ\����؂�ւ��Ȃ�
	if(strcmp(eStatus,"01") == 0 || strcmp(eStatus,"06") == 0 ){
		//from,using�̎��͓��͂��ꂽ�l��\������
		if(targObject->fromVar.iVarSize != 0 || targObject->usingVar.iVarSize != 0){
			//���͂��������𔽉f
			if(targObject->fromVar.iVarSize != 0){
				//from�͓��͍��ڂƂ��Ă͗��Ȃ��Ǝv����
				struct fromObject tempVar;
				tempVar = targObject->fromVar;
				SD_Output(targObject->cobargname,tempVar.arrPnt," ");
			}else{
				struct inputObject tempVar;
				tempVar = targObject->usingVar;
				SD_Output(targObject->cobargname,tempVar.arrPnt," ");
			}
		}else{
			if(targObject->bodyPnt != NULL){
				SD_Output(targObject->cobargname,targObject->bodyPnt->data," ");
			}else{
				SD_Output(targObject->cobargname,inputArg," ");
			}
		}
	}

	//���L�ϐ������ɖ߂�
	unsetCommonFunctionName(map_source_func,strStack);
	return 0;
}

int stopExec(){
	int result;
	char term_buff[BUFF_SIZE];int counter;
	char *eStatus;
	int intStatLen=3;
	struct screenObject temporaryObj;

	//stop�̎��͉���`��obj�ŕ\��
	temporaryObj.y = SD_LINE_MAX;
	temporaryObj.x = SD_COL_MAX - 1;
	temporaryObj.lineShift = 0;
	temporaryObj.ColShift = 0;
	temporaryObj.length = 0;
	strcpy(temporaryObj.cobtype,"X");
	strcpy(temporaryObj.cobargname,"STOP");

	eStatus = malloc(intStatLen);
	memset(eStatus,'\0',intStatLen);
	do{
		SD_AcceptStdin(&temporaryObj,term_buff);
		setStatusASplit(term_buff,eStatus);
		//01=Enter�ł�ESC�ł��Ȃ���
	}while(strncmp(eStatus,"01",strlen("01")) && strncmp(eStatus,"ESC",strlen("ESC")));

	free(eStatus);
}

//FROM��̑Ή�
//
//cobArgName:�Ώەϐ��̖��O argPoint:�ڑ�����ϐ��̃|�C���^
int SD_From(char *cobArgName,char *argPoint,char *cSize,char *cArgc,...){
	int i=0, j=0, iArgc=0,iSize;
	int argVaiable = 4;    //�ǂ��܂ł��K�����������(���̊֐����ł̒萔�Ƃ���)
	va_list list;
	char *subscript;
	int funcId = 4;

	//�֐��������L�ϐ��ɃZ�b�g
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"SD_From  :%.10s ",cobArgName);

	//�����̌Œ�l�����������������ĂȂ��Ƃ��̓G���[��
	if(cob_call_params < 4){
		cob_runtime_error(" Error C [%02d]: Option Missing  set option %d, need option is 4 target %s \n",99,cob_call_params,cobArgName);
		executeEnd();
	}
	//(!isdigit(*cArgc) == 1 && isNullOrEmpty(cArgc) == 1)��isNullOrEmpty(cArgc)�͂���Ȃ����Ȃ̂ō폜 upd koyama 20170818
	//�T�C�Y�������ł���A�z��̐����������A�z��̐����̉ӏ��ɋ󕶎���,'0',�X�y�[�X�̎�|| *cArgc == '0'
	if(!isdigit(*cSize) ||  (!isdigit(*cArgc) && isNullOrEmpty(cArgc) == 0 )){
		//cSize,cArgc���s����������֐����I��
		cob_runtime_error(" Error C [%02d]:invalid parameter %s ",99,map_source_func);
		//���L�ϐ������ɖ߂�
		unsetCommonFunctionName(map_source_func,strStack);
		return 1;
	}

	//������ŗ���̂�int�ɕϊ�
	//������
	iArgc = atoi(cArgc);
	iSize = atoi(cSize);

	struct screenObject *targObject;
	targObject = searchTargetObject(SD_cobScreen,cobArgName,SD_cobScreenLen);

	//�Ώۂ̃I�u�W�F�N�g������ꍇ�A�O�̃I�u�W�F�N�g��T���Ă���next�̃|�C���^���i�[
	if(iArgc == 0){
		//argPoint�̓��e���z��ł͂Ȃ�
		targObject->fromVar.arrPnt = (char *)argPoint;
		targObject->fromVar.iVarSize = iSize;
		targObject->fromVar.iDim = iArgc;
		targObject->fromVar.bodyPnt = cob_current_module->cob_procedure_parameters[1];

		//editing fromat�̂Ƃ��͓���ւ���
		if(strlen(targObject->cobtype) > 1
		&& strchr(targObject->cobtype,'S') == NULL
		&& strchr(targObject->cobtype,'b') == NULL
		&& strchr(targObject->cobtype,'R') == NULL ){
//					targObject->length = iSize;
		}
	} else {
		//argPoint�̓��e���z��
		va_start( list, cArgc );
		//�Ώەϐ��̒T��
		//editing fromat�̂Ƃ��͓���ւ���
		if(strlen(targObject->cobtype) > 1
		&& strchr(targObject->cobtype,'S') == NULL
		&& strchr(targObject->cobtype,'b') == NULL
		&& strchr(targObject->cobtype,'R') == NULL ){
//					targObject->length = iSize;
		}
		//�ϐ��̐��Ɋ֌W�̂Ȃ����ڂ�
		targObject->fromVar.arrPnt = (char *)argPoint;
		targObject->fromVar.iVarSize = iSize;
		targObject->fromVar.iDim = iArgc;
		//������OCCURS�̉\��������̂œ���Ȃ������K�v 20150525
		targObject->fromVar.bodyPnt = malloc( sizeof(cob_field) );
		targObject->fromVar.bodyPnt->size = cob_current_module->cob_procedure_parameters[1]->size;
		targObject->fromVar.bodyPnt->data = cob_current_module->cob_procedure_parameters[1]->data;
		targObject->fromVar.bodyPnt->attr = cob_current_module->cob_procedure_parameters[1]->attr;
		for(j = 0;j < (iArgc * 2); j++){

			subscript = va_arg( list, char* );
			struct cob_field *curParam;

			switch(j){
			case 0:
				//COBOL����n���������̕����񒷂Ƃ��̈����̃T�C�Y����v���Ȃ�A'0'�łȂ�
				if((strlen(cob_current_module->cob_procedure_parameters[argVaiable + j]->data)
					== cob_current_module->cob_procedure_parameters[argVaiable + j]->size)
					&& atoi(subscript) != 0){
					targObject->fromVar.actualFlg1 = atoi(subscript);
				}else{
					targObject->fromVar.fromVar1 = subscript;
					targObject->fromVar.var1Length = cob_current_module->cob_procedure_parameters[argVaiable + j]->size;
					targObject->fromVar.actualFlg1 = -99;
				}
				break;
			case 1:
				//
				memcpy((int *)&targObject->fromVar.var1Size,subscript,sizeof(int));
				break;
			case 2:
				//COBOL����n���������̕����񒷂Ƃ��̈����̃T�C�Y����v���Ȃ�A'0'�łȂ�
				if((strlen(cob_current_module->cob_procedure_parameters[argVaiable + j]->data)
					== cob_current_module->cob_procedure_parameters[argVaiable + j]->size)
					&& atoi(subscript) != 0){
					targObject->fromVar.actualFlg2 = atoi(subscript);
				}else{
					targObject->fromVar.fromVar2 = subscript;
					targObject->fromVar.var2Length = cob_current_module->cob_procedure_parameters[argVaiable + j]->size;
					targObject->fromVar.actualFlg2 = -99;
				}
				break;
			case 3:
				memcpy((int *)&targObject->fromVar.var2Size,subscript,sizeof(int));
				break;
			case 4:
				//COBOL����n���������̕����񒷂Ƃ��̈����̃T�C�Y����v���Ȃ�A'0'�łȂ�
				if((strlen(cob_current_module->cob_procedure_parameters[argVaiable + j]->data)
					== cob_current_module->cob_procedure_parameters[argVaiable + j]->size)
					&& atoi(subscript) != 0){
					targObject->fromVar.actualFlg3 = atoi(subscript);
				}else{
					targObject->fromVar.fromVar3 = subscript;
					targObject->fromVar.var3Length = cob_current_module->cob_procedure_parameters[argVaiable + j]->size;
					targObject->fromVar.actualFlg3 = -99;
				}
				break;
			case 5:
				memcpy((int *)&targObject->fromVar.var3Size,subscript,sizeof(int));
				break;
			default:
				break;

			}
		}
	}
	//���L�ϐ������ɖ߂�
	unsetCommonFunctionName(map_source_func,strStack);
	return 0;
}

//into �̕ϐ�����͍��ڂƂ��ďo��
//From�Ǝd�g�݂͓����̖͗l,���̓`�F�b�N������Ƃ��낪�Ⴄ
int SD_Into(char *cobArgName,char *argPoint,char *cSize,char *cArgc,...){
	int i=0, j=0, iArgc=0,iSize;
	int argVaiable = 4;    //�ǂ��܂ł��K�����������(���̊֐����ł̒萔�Ƃ���)
	va_list list;
	char *subscript;

	//�֐��������L�ϐ��ɃZ�b�g
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"SD_Into :%.10s",cobArgName);

	//�����̌Œ�l�����������������ĂȂ��Ƃ��̓G���[��
	if(cob_call_params < 4){
		fprintf(stderr," Error C [%02d]: Option Missing  set option %d, need option is 4 target %s \n",99,cob_call_params,cobArgName);
		executeEnd();
	}

	//(!isdigit(*cArgc) == 1 && isNullOrEmpty(cArgc) == 1)��isNullOrEmpty(cArgc)�͂���Ȃ����Ȃ̂ō폜 upd koyama 20170818
	if(!isdigit(*cSize) ||  (!isdigit(*cArgc) == 1)){
		//cSize,cArgc���s����������֐����I��
		cob_runtime_error(" Error C [%02d]:invalid parameter %s ",99,map_source_func);
		//���L�ϐ������ɖ߂�
		unsetCommonFunctionName(map_source_func,strStack);
		return 1;
	}

	//������ŗ���̂�int�ɕϊ�
	iArgc = atoi(cArgc);
	iSize = atoi(cSize);

	struct screenObject *targObject;
	targObject = searchTargetObject(SD_cobScreen,cobArgName,SD_cobScreenLen);

	//�Ώۂ̃I�u�W�F�N�g������ꍇ�A�O�̃I�u�W�F�N�g��T���Ă���next�̃|�C���^���i�[
	if(iArgc == 0){
		//argPoint�̓��e���z��ł͂Ȃ�
				targObject->inputVar.arrPnt = (char *)argPoint;
				targObject->inputVar.iVarSize = iSize;
				targObject->inputVar.iDim = iArgc;
				//������OCCURS�̉\��������̂œ���Ȃ������K�v 20150525
				targObject->inputVar.bodyPnt = malloc( sizeof(cob_field) );
				targObject->inputVar.bodyPnt->size = cob_current_module->cob_procedure_parameters[1]->size;
				targObject->inputVar.bodyPnt->data = cob_current_module->cob_procedure_parameters[1]->data;
				targObject->inputVar.bodyPnt->attr = cob_current_module->cob_procedure_parameters[1]->attr;

				//editing fromat�̂Ƃ��͓���ւ���
				if(strlen(targObject->cobtype) > 1
				&& strchr(targObject->cobtype,'S') == NULL
				&& strchr(targObject->cobtype,'b') == NULL
				&& strchr(targObject->cobtype,'R') == NULL ){
//					targObject->length = iSize;
				}
	} else {
		//argPoint�̓��e���z��
		va_start( list, cArgc );
		//�Ώەϐ��̒T��
		//editing fromat�̂Ƃ��͓���ւ���
		if(strlen(targObject->cobtype) > 1
		&& strchr(targObject->cobtype,'S') == NULL
		&& strchr(targObject->cobtype,'b') == NULL
		&& strchr(targObject->cobtype,'R') == NULL ){
//					targObject->length = iSize;
		}
		//�ϐ��̐��Ɋ֌W�̂Ȃ����ڂ�
		targObject->inputVar.arrPnt = (char *)argPoint;
		targObject->inputVar.iVarSize = iSize;
		targObject->inputVar.iDim = iArgc;
		//������OCCURS�̉\��������̂œ���Ȃ������K�v 20150525
		targObject->inputVar.bodyPnt = malloc( sizeof(cob_field) );
		targObject->inputVar.bodyPnt->size = cob_current_module->cob_procedure_parameters[1]->size;
		targObject->inputVar.bodyPnt->data = cob_current_module->cob_procedure_parameters[1]->data;
		targObject->inputVar.bodyPnt->attr = cob_current_module->cob_procedure_parameters[1]->attr;

		for(j = 0;j < (iArgc * 2); j++){
			subscript = va_arg( list, char* );
			switch(j){
			case 0:
				//COBOL����n���������̕����񒷂Ƃ��̈����̃T�C�Y����v���Ȃ�A'0'�łȂ�
				if((strlen(cob_current_module->cob_procedure_parameters[argVaiable + j]->data)
					== cob_current_module->cob_procedure_parameters[argVaiable + j]->size)
					&& atoi(subscript) != 0){
					//���̒l�������Ă���Ƃ�
					targObject->inputVar.actualFlg1 = atoi(subscript);
				}else{
					//�|�C���^���Ƀf�[�^���L��Ƃ�
					targObject->inputVar.var1 = subscript;
					targObject->inputVar.var1Length = cob_current_module->cob_procedure_parameters[argVaiable + j]->size;
					targObject->inputVar.actualFlg1 = -99;
				}
				break;
			case 1:
				memcpy((int *)&targObject->inputVar.var1Size,subscript,sizeof(int));
				break;
			case 2:
				//COBOL����n���������̕����񒷂Ƃ��̈����̃T�C�Y����v���Ȃ�A'0'�łȂ�
				if((strlen(cob_current_module->cob_procedure_parameters[argVaiable + j]->data)
					== cob_current_module->cob_procedure_parameters[argVaiable + j]->size)
					&& atoi(subscript) != 0){
					//���̒l�������Ă���Ƃ�
					targObject->inputVar.actualFlg2 = atoi(subscript);
				}else{
					//�|�C���^���Ƀf�[�^���L��Ƃ�
					targObject->inputVar.var2 = subscript;
					targObject->inputVar.var2Length = cob_current_module->cob_procedure_parameters[argVaiable + j]->size;
					targObject->inputVar.actualFlg2 = -99;
				}
				break;
			case 3:
				memcpy((int *)&targObject->inputVar.var2Size,subscript,sizeof(int));
				break;
			case 4:
				//COBOL����n���������̕����񒷂Ƃ��̈����̃T�C�Y����v���Ȃ�A'0'�łȂ�
				if((strlen(cob_current_module->cob_procedure_parameters[argVaiable + j]->data)
					== cob_current_module->cob_procedure_parameters[argVaiable + j]->size)
					&& atoi(subscript) != 0){
					//���̒l�������Ă���Ƃ�
					targObject->inputVar.actualFlg3 = atoi(subscript);
				}else{
					//�|�C���^���Ƀf�[�^���L��Ƃ�
					targObject->inputVar.var3 = subscript;
					targObject->inputVar.var3Length = cob_current_module->cob_procedure_parameters[argVaiable + j]->size;
					targObject->inputVar.actualFlg3 = -99;
				}
				break;
			case 5:
				memcpy((int *)&targObject->inputVar.var3Size,subscript,sizeof(int));
				break;
			default:
				break;

			}
		}
	}
	//���L�ϐ������ɖ߂�
	unsetCommonFunctionName(map_source_func,strStack);
	return 0;
}


//Using �̕ϐ�����͍��ڂƂ��ďo��
//From�Ǝd�g�݂͓����̖͗l,���̓`�F�b�N������Ƃ��낪�Ⴄ
int SD_Using(char *cobArgName,char *argPoint,char *cSize,char *cArgc,...){
	int i=0, j=0, iArgc=0,iSize;
	int argVaiable = 4;    //�ǂ��܂ł��K�����������(���̊֐����ł̒萔�Ƃ���)
	va_list list;
	char *subscript;

	//�֐��������L�ϐ��ɃZ�b�g
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"SD_Using :%.10s",cobArgName);

	//�����̌Œ�l�����������������ĂȂ��Ƃ��̓G���[��
	if(cob_call_params < 4){
		cob_runtime_error(" Error C [%02d]: Option Missing  set option %d, need option is 4 target %s \n",99,cob_call_params,cobArgName);
		executeEnd();
	}

	//(!isdigit(*cArgc) == 1 && isNullOrEmpty(cArgc) == 1)��isNullOrEmpty(cArgc)�͂���Ȃ����Ȃ̂ō폜 upd koyama 20170818
	if(!isdigit(*cSize) ||  (!isdigit(*cArgc) == 1)){
		//cSize,cArgc���s����������֐����I��
		cob_runtime_error(" Error C [%02d]:invalid parameter %s ",99,map_source_func);
		//���L�ϐ������ɖ߂�
		unsetCommonFunctionName(map_source_func,strStack);
		return 1;
	}

	//������ŗ���̂�int�ɕϊ�
	iArgc = atoi(cArgc);
	iSize = atoi(cSize);

	struct screenObject *targObject;
	targObject = searchTargetObject(SD_cobScreen,cobArgName,SD_cobScreenLen);

	//�Ώۂ̃I�u�W�F�N�g������ꍇ�A�O�̃I�u�W�F�N�g��T���Ă���next�̃|�C���^���i�[
	if(iArgc == 0){
		//argPoint�̓��e���z��ł͂Ȃ�
		targObject->usingVar.arrPnt = (char *)argPoint;
		targObject->usingVar.iVarSize = iSize;
		targObject->usingVar.iDim = iArgc;
		//������OCCURS�̉\��������̂œ���Ȃ������K�v 20150525
		targObject->usingVar.bodyPnt = malloc( sizeof(cob_field) );
		targObject->usingVar.bodyPnt->size = cob_current_module->cob_procedure_parameters[1]->size;
		targObject->usingVar.bodyPnt->data = cob_current_module->cob_procedure_parameters[1]->data;
		targObject->usingVar.bodyPnt->attr = cob_current_module->cob_procedure_parameters[1]->attr;
		//editing fromat�̂Ƃ��͓���ւ���
		if(strlen(targObject->cobtype) > 1
		&& strchr(targObject->cobtype,'S') == NULL
		&& strchr(targObject->cobtype,'b') == NULL
		&& strchr(targObject->cobtype,'R') == NULL ){
//					targObject->length = iSize;
		}
	} else {
		if(cob_call_params <= 4){
			fprintf(stderr," Error C [%02d]: Option Missing  set option %d,  %s \n",99,cob_call_params,cobArgName);
			executeEnd();
		}
		//argPoint�̓��e���z��
		va_start( list, cArgc );
		//�Ώەϐ��̒T��
		//editing fromat�̂Ƃ��͓���ւ���
		if(strlen(targObject->cobtype) > 1
		&& strchr(targObject->cobtype,'S') == NULL
		&& strchr(targObject->cobtype,'b') == NULL
		&& strchr(targObject->cobtype,'R') == NULL ){
//					targObject->length = iSize;
		}
		//�ϐ��̐��Ɋ֌W�̂Ȃ����ڂ�
		targObject->usingVar.arrPnt = (char *)argPoint;
		targObject->usingVar.iVarSize = iSize;
		targObject->usingVar.iDim = iArgc;
		//������OCCURS�̉\��������̂œ���Ȃ������K�v 20150525
		targObject->usingVar.bodyPnt = malloc( sizeof(cob_field) );
		targObject->usingVar.bodyPnt->size = cob_current_module->cob_procedure_parameters[1]->size;
		targObject->usingVar.bodyPnt->data = cob_current_module->cob_procedure_parameters[1]->data;
		targObject->usingVar.bodyPnt->attr = cob_current_module->cob_procedure_parameters[1]->attr;

		for(j = 0;j < (iArgc * 2); j++){
			subscript = va_arg( list, char* );
			if(cob_current_module->cob_procedure_parameters[argVaiable + j] == '\0'){
				fprintf(stderr," Error C [%02d]: Option Missing  set option %d, Invalid index %s \n",99,cob_call_params,cobArgName);
				executeEnd();
			}
			switch(j){
			case 0:
				//COBOL����n���������̕����񒷂Ƃ��̈����̃T�C�Y����v���Ȃ�A'0'�łȂ�
				if((strlen(cob_current_module->cob_procedure_parameters[argVaiable + j]->data)
					== cob_current_module->cob_procedure_parameters[argVaiable + j]->size)
					&& atoi(subscript) != 0){
					targObject->usingVar.actualFlg1 = atoi(subscript);
				}else{
					targObject->usingVar.var1 = subscript;
					targObject->usingVar.var1Length = cob_current_module->cob_procedure_parameters[argVaiable + j]->size;
					targObject->usingVar.actualFlg1 = -99;
				}
				break;
			case 1:
				memcpy((int *)&targObject->usingVar.var1Size,subscript,sizeof(int));
				break;
			case 2:
				//COBOL����n���������̕����񒷂Ƃ��̈����̃T�C�Y����v���Ȃ�A'0'�łȂ�
				if((strlen(cob_current_module->cob_procedure_parameters[argVaiable + j]->data)
					== cob_current_module->cob_procedure_parameters[argVaiable + j]->size)
					&& atoi(subscript) != 0){
					targObject->usingVar.actualFlg2 = atoi(subscript);
				}else{
					targObject->usingVar.var2 = subscript;
					targObject->usingVar.var2Length = cob_current_module->cob_procedure_parameters[argVaiable + j]->size;
					targObject->usingVar.actualFlg2 = -99;
				}
				break;
			case 3:
				memcpy((int *)&targObject->usingVar.var2Size,subscript,sizeof(int));
				break;
			case 4:
				//COBOL����n���������̕����񒷂Ƃ��̈����̃T�C�Y����v���Ȃ�A'0'�łȂ�
				if((strlen(cob_current_module->cob_procedure_parameters[argVaiable + j]->data)
					== cob_current_module->cob_procedure_parameters[argVaiable + j]->size)
					&& atoi(subscript) != 0){
					targObject->usingVar.actualFlg3 = atoi(subscript);
				}else{
					targObject->usingVar.var3 = subscript;
					targObject->usingVar.var3Length = cob_current_module->cob_procedure_parameters[argVaiable + j]->size;
					targObject->usingVar.actualFlg3 = -99;
				}
				break;
			case 5:
				memcpy((int *)&targObject->usingVar.var3Size,subscript,sizeof(int));
				break;
			default:
				break;

			}
		}
	}
	//���L�ϐ������ɖ߂�
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return 0;
}

//using �̕ϐ�����v������H
//
int SD_Arg_Match(char *arg1,char *arg2,char *arg3){
	int i=0;
	//�Ώۂ̃I�u�W�F�N�g������ꍇ�A�O�̃I�u�W�F�N�g��T���Ă���next�̃|�C���^���i�[
//	for(i=0;i < SD_cobScreenLen;i++){
//		if(strcmp(SD_cobScreen[i].cobargname,arg1) == 0){
//			break;
//		}
//	}
	memcpy(arg3, arg2, strlen(arg2));
	return 0;
}

//�ϐ��̍s����v������
//
int SD_Arg_Match_Line(char *argname, char *size, char *arg){
	int i=0;
	char temp[64]="";
	struct screenObject *temporaryObj;
	//arg��낪�Ȃ����Ă���P�[�X������̂Œ����ł���
	strncpy(temp,arg,atoi(size));
	//�Ώۂ̃I�u�W�F�N�g������ꍇ�A�O�̃I�u�W�F�N�g��T���Ă���next�̃|�C���^���i�[
	setYNameTargetObject(SD_cobScreen,argname,(atoi(temp)),SD_cobScreenLen);
	return 0;
}

//�ϐ��̍s����v������
//
int SD_Arg_Match_Col(char *argname, char *size,char *arg){
	int i=0;
	char temp[64]="";
	struct screenObject *temporaryObj;
	//arg��낪�Ȃ����Ă���P�[�X������̂Œ����ł���
	strncpy(temp,arg,atoi(size));
	//�Ώۂ̃I�u�W�F�N�g������ꍇ�A�O�̃I�u�W�F�N�g��T���Ă���next�̃|�C���^���i�[
	setXNameTargetObject(SD_cobScreen,argname,(atoi(temp) + SD_cobScreen[i].ColShift),SD_cobScreenLen);
	return 0;
}

//DISPLAY�̑���ɏo�͂��s��
//��ʏo�͂̂��߂ɕK�v�Ȃ��̂�t�����鏈�����s��
//�q�ǂ��͍ċA?�Z��̓��[�v�ŏ�������
int resetAllViewFlg(){
	int i = 0;

	for(i=0;i < SD_cobScreenLen;i++){
		SD_cobScreen[i].onViewFlg = 0;
	}
}


//DISPLAY�̑���ɏo�͂��s��
//��ʏo�͂̂��߂ɕK�v�Ȃ��̂�t�����鏈�����s��
//�q�ǂ��͍ċA?�Z��̓��[�v�ŏ�������
int SD_Output(char *argname,char *printarg,char *curLength){
	int i=0,j=0;                                             //roop�ϐ�
	int currentLength = 0;
	struct screenObject *currentObject;
	int printflg = 0;
	char strTime[] = "0000/00/00 00:00:00.000000";
//	char strTemp[HRIZONTAL_LENGTH];

	//�֐��������L�ϐ��ɃZ�b�g
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"SD_Output :%.10s",argname);

	if(myConfDebugFlg){
		cob_runtime_error(" Error [%04d]: %s Info SD_Output ",__LINE__,local_server_time(strTime));
	}
	//�ŏ��̑���
	//curLength��1�����ڂ������ȊO��0����8���ɂȂ��Ă��Ȃ���ΊO���痈�Ă���->����ȊO�̎���pointer�������Ă���
	if(isdigit(*curLength) == 0 || strlen(curLength) != 8){
		currentObject = searchTargetObject(SD_cobScreen,argname,SD_cobScreenLen);
	}else{
		//
		currentObject = (struct screenObject *)argname;
	}
	//atoi�͐����ȊO�Ȃ�0��Ԃ����ߓ��͂������Ȃ�0
	currentLength = atoi(curLength);
	printflg = 1;                                     //�^�[�Q�b�g����ł���������flg on
	for(j = 0;currentLength <= strlen(printarg);j++){
		//�Ōオ���p�X�y�[�X�̏ꍇ��菜����A���̕����ɉe�����o�邽��"�ň͂�ł���
		char *buzzerPointer;
		char strTemp[HRIZONTAL_LENGTH] = "\"";        //"�݂̂̕�����ŏ�����
		char strTemp2[HRIZONTAL_LENGTH] = "";         //���當����ŏ�����
		if(currentObject->childObj != 0){
			//�ċA�ŉ����Ăԗ\��
			char charCurrentLength[10];                         //���̊֐��֌��݂�
			sprintf(charCurrentLength, "%08d", currentLength);
			//char�|�C���^�Ƃ��đ��邪�󂯎�������Ƃŕʂ̃|�C���^�Ɏ󂯎��
			SD_Output((char *)currentObject->childObj,printarg,charCurrentLength);
		} else {
			//-,Z,B�̑Ή�

			//�\�������邱�Ƃ��m�肵���\���ϐ��̃t���O��ς���
			currentObject->onViewFlg = 1;

			//������̐ڑ�
			if(currentObject->fromVar.iVarSize != 0 || currentObject->usingVar.iVarSize != 0){
				//From�̂Ƃ��̓|�C���^����o��
				//input�̏������܂Ƃ߂�
				char toData[2048]="";
				char *fromData;
				char *strPic;
				cob_field_attr  a_from = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
				cob_field_attr  a_to = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
				cob_field       d_from;
				cob_field       d_to;
				cob_field       *from_base;

				//-----------------------------------------------------------�����̐ݒ�
				if(strchr(currentObject->cobtype,'N')!=0){
					a_to.type = (unsigned char)COB_TYPE_NATIONAL;
				}else if(strchr(currentObject->cobtype,'X')!=0){
					a_to.type = (unsigned char)COB_TYPE_ALPHANUMERIC;
				}else{
					a_to.type = (unsigned char)COB_TYPE_NUMERIC_EDITED;
				}

				a_to.flags=0;
				//������edit
				if(a_to.type == (unsigned char)COB_TYPE_NUMERIC_EDITED){
					strPic = setFormatEdit(currentObject,strPic);
					a_to.pic = strPic;
				}else{
					//�̂Ă�̂œ��e�͊֌W�Ȃ�
					strPic = malloc(2);
					memset(strPic,'\0',2);
				}
				if(currentObject->length < strlen(currentObject->cobtype)){
					a_to.digits=strlen(currentObject->cobtype);
					if(strchr(currentObject->cobtype,'R') != 0){
						a_to.digits--;
					}
					if(strchr(currentObject->cobtype,'b') != 0){
						a_to.digits--;
					}
					d_to.size = a_to.digits;
				}else{
					//�����_�ł͂Ȃ�pic������Ƃ��ɂ��ׂ�
					//S9�̎w��������_���܂ޏꍇ������
					//TODO ����𐔎��̎��ɐ������݂̂�
					if( (*strPic != '\0') && (strcmp(currentObject->cobtype,"9") != 0) && (strcmp(currentObject->cobtype,"S9") != 0)){
						//�����_������Ƃ���scale���v�Z
						a_to.digits = cobtype_calc_digits(currentObject->cobtype);
						d_to.size   = currentObject->length;
						a_to.flags=1;
					}else{
						a_to.digits = currentObject->length;
						d_to.size   = currentObject->length;
						a_to.flags=0;
					}
				}

				//�����_�̂��Ƃ��l����
				if(strchr(currentObject->cobtype,cob_current_module->decimal_point) != NULL){
					//.������Ώ����_��
					a_to.scale = SD_calc_editing_scale(a_to.digits,currentObject->cobtype,cob_current_module->decimal_point);
//					a_to.scale = a_to.digits - (int)(strchr(currentObject->cobtype,'.') - (int)currentObject->cobtype);
//					a_to.scale = strlen(currentObject->cobtype) -(currentObject->cobtype - strchr(currentObject->cobtype,'.'));
				}else{
					//.���Ȃ���Ώ����_����0
					a_to.scale = 0;
				}

				//-----------------------------------------------------------���茳�̐ݒ�
				//Z��-(�n�C�t��)�̎��͕����񒷂�1�ł�editing format
				//�R�s�[���ƂȂ�ϐ���������
				if(currentObject->fromVar.iVarSize != 0){
					fromData = SD_getFromVarPnt(currentObject->fromVar,currentObject->length);
					from_base = currentObject->fromVar.bodyPnt;
					//size��data���擾�������̃T�C�Y
					d_from.size = currentObject->fromVar.iVarSize;
				}else{
					if(currentObject->inputVar.iVarSize != 0){
						fromData = SD_getInputVarPnt(currentObject->inputVar,currentObject->length);
						from_base = currentObject->inputVar.bodyPnt;
						//size��data���擾�������̃T�C�Y
						d_from.size = currentObject->inputVar.iVarSize;
					}else{
						fromData = SD_getInputVarPnt(currentObject->usingVar,currentObject->length);
						from_base = currentObject->usingVar.bodyPnt;
						//size��data���擾�������̃T�C�Y
						d_from.size = currentObject->usingVar.iVarSize;
					}
				}

				//input OR From�Ȃ̂Ō��̑����ɂ��킹��
				a_from.type   = from_base->attr->type;
				a_from.digits = from_base->attr->digits;
				a_from.scale  = from_base->attr->scale;
				a_from.flags  = from_base->attr->flags;

				d_to.data = toData;
				d_from.data = fromData;
				d_to.attr = &a_to;
				d_from.attr = &a_from;
				//------------------------------------------move���钼�O�ŏ�����������������Ă��Ȃ���Ώ���������
				SD_ObjectExistsInitData(&d_from);
				//-------------------------------------------------------------move
				cob_move(&d_from,&d_to);

				//function�Œ��g������Ă���̂Œ���
				free(strPic);
				strncpy(strTemp2, toData, strlen(toData));
//				free(toData);
			}else{
				strncpy(strTemp2, (printarg + currentLength), (currentObject->length));
			}
			strcat(strTemp,strTemp2);
			strcat(strTemp,"\"");

			//�\���̃o�b�t�@�����Ȃ��ݒ�
//					setvbuf( stdout, NULL, _IOLBF, 0 );

			//������̒����ŏo��
			//�u�U�[�̒萔�����݂����
			if((buzzerPointer = strchr(strTemp,0x1B)) > 0){
				printf("BUZ (%02d,%02d) (%c,%d)_\r\n", getLineTargetObject(currentObject), getColTargetObject(currentObject), buzzerPointer[1],buzzerPointer[2]);
				stopExec();
			}else{
				//"���悯���Ƃ��낪��v���邩�ǂ���
				//��v������COBOL�R�[�h��̃X�e�[�^�X
				if(strncmp((strTemp + 1),"VER",strlen("VER")) == 0||strncmp((strTemp + 1),"UND",strlen("UND")) == 0||strncmp((strTemp + 1),"OVE",strlen("OVE")) == 0){
					char *strPrint;
					strPrint = malloc(sizeof(strTemp[0]) * strlen(strTemp + 1));
					memcpy(strPrint,(strTemp + 1),(strlen(strTemp + 1) - 1));
					printf("%s\r\n", strPrint);
					free(strPrint);
				}else{
					printf("CON (%02d,%02d)d %s_\r\n", getLineTargetObject(currentObject), getColTargetObject(currentObject), strTemp);
				}
			}
			if((currentObject->cobtype[0] == 'R')){
				printf("REV (%02d,%02d) (,%02d)_\r\n", getLineTargetObject(currentObject), getColTargetObject(currentObject), (getColTargetObject(currentObject) + currentObject->length));
			}else if(currentObject->cobtype[0] == 'b'){
				printf("BLI (%02d,%02d) (,%02d)_\r\n", getLineTargetObject(currentObject), getColTargetObject(currentObject), (getColTargetObject(currentObject) + currentObject->length));
			}
//					fflush( stdout );
		}
		//���݂̊J�n�ʒu�����̊J�n�ʒu�ɕύX
		currentLength = currentLength + currentObject->length;
//printf("|(%d\t^_^\t%s %d)| \n", currentLength, currentObject->cobargname,currentObject->length);

		//
		if(strncmp(strTemp,"CLEAR",strlen("CLEAR")) == 0){
			resetAllViewFlg();
		}

		//�����ϐ���break
		//���͂��ꂽ�l��0-9�ȊO�Ȃ�
		if(isdigit(curLength[0]) == 0){
			break;
		}

		//���̃m�[�h�����ă��[�v
		if((currentObject->nextObj != 0)){
			currentObject = currentObject->nextObj;
		}else{
			//���̃m�[�h���Ȃ�or�����w��m�[�h�̎��͎���ǂ܂Ȃ�
			//�����w��m�[�h�̎��͂��̃m�[�h,���̃m�[�h�̎q���o��
			break;
		}

		//�~�܂�Ȃ��Ɗ댯�Ȃ̂�
		if(j > 50){
			break;
		}
	}

	if(printflg == 0){
		printf("CON (%02d,%02d) %s_\r\n", SD_LINE_MAX, 1, printarg);
	}
	if(strcmp(curLength,"STOP") == 0){
		stopExec();
	}
	if(myConfDebugFlg){
		cob_runtime_error(" Error [%04d]: %s Info SD_Output ",__LINE__,local_server_time(strTime));
	}
	//���L�ϐ������ɖ߂�
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return 0;
}

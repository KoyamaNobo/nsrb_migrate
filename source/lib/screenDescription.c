/* COBOL ScreenDescription Version 0.1 */
/* Create 20140210  Author koyama */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>    /*atoiを利用*/
#include <stdarg.h>    /*va_listを使うために必要*/
#include <ctype.h>     /* toupperに必要 */
#include <time.h>      /* localtimeに必要 20150828 */
#include <libxml/xmlreader.h>    /*confファイルのxmlを読むため*/
#include <libxml/xpath.h>        /*confファイルのxmlを読むため*/
#include <libcob.h>
#include <sys/stat.h>
#include "confheader.h"

#ifndef SD_CONST
#define SD_CONST 1
#define HRIZONTAL_LENGTH 1024 //画面の横幅+""+'\0'
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
const char* CONF_DEFPATH  = "//conf/*/text()";    //トップレベルから一直線を予定
#endif

//DBのタグ名
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

//confから取得するdebug_flg.cob-mysqlで宣言
//static int myConfDebugFlg;

static char strTime[] = "0000/00/00 00:00:00.000000";
static char SD_screenDefPath[256];
static char SD_screenDefExt[32];
//headerでexternつきで宣言
int SD_cobScreenLen = 0;                 /*次に使用可能なオブジェクトの番号を示す*/
struct screenObject *SD_cobScreen;      /*トップレベルを定義？*/

//////////////////Function liet AND prototype Start (idはError Outputがあるもののみ)
//Id=00 RTrim実装
int isNullOrEmpty(char *targ);
//ID=02 設定ファイルのパスを取得
char *getConfFilename(char *strConfPath);
//Id=03 特定の文字を除去しながらつなげる
void remTargChar(char *origText,char targ);
//ID=04 FROM句の対応
int SD_From(char *cobArgName,char *argPoint,char *cSize,char *cArgc,...);
//targObjの列番号を返す
int getColTargetObject(struct screenObject *targObj);
//targObjの行番号を返す
int getLineTargetObject(struct screenObject *targObj);
//cob_fieldがpackedとして正しい値か確認
int SD_isPackedFormat(cob_field *);
//////////////////Function liet AND prototype End

//----------------------------------------------------------異常終了処理
//author:n.koyama
//date  :20140530
//異常終了処理のときに何かする必要がある時のため
//
void executeEnd(){
	exit(1);
}

//現在時間をフォーマット(YY/MM/dd hh:mm:ss.mmmmmm)で返す
//in/out :retStr 返す対象文字列のポインタ
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

//RTrim実装(空白文字のみかどうかを判定)
//(それぞれにRtimがあるのでここでも専用を書き直す)
//date:20160520
//auth: koyama
int isNullOrEmpty(char *targ){
	char *strstart;
	char *strend;
	int num=0;
	strstart = targ;
	//最後の文字から判定対象
	strend = targ + strlen(strstart);
	for(strend--;strend >= strstart;strend--){
		if(*strend == ' '){
			num++;
		}
	}
	//全てがスペースなら
	if(num == strlen(strstart)){
		num = 1;
	}else{
		num = 0;
	}
	return num;
}

//設定ファイルのパスを取得
//date:20150828
//auth: koyama
//カレントdirのconfだけを開くとどこででも実行できないので
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

//-----------------------------------------------------------文字をxからyに
//author:n.koyama
//date  :20140609
//p:対象文字列
//x:置換対象
//y:置換文字
char *chrchange(char *p,char x,char y){
	int i = 0;
	char  s[128];
	char* t;

	strncpy(s,p,strlen(p));

	while(t = strrchr(s,x)){
		*t = (char)y;
	}

	memcpy(p,s,(strlen(s) + 1));

	return (p);                  /* 文字列の先頭アドレスを返す */
}

//-----------------------------------------------------------特定の文字を除去しながらつなげる
//author:n.koyama
//date  :20151222
//origText:対象文字列
//targ:削除文字
//return なし
void remTargChar(char *origText,char targ){
	char *origTextEnd;
	char *copyText;
	int addlen = 0;

	copyText  = origText;
	//\0までをコピーしたい
	origTextEnd = origText + strlen(origText) + 1;
	//
	for(;origText < origTextEnd;origText++){
		//改行文字を飛ばす
		if(*copyText == targ ){
			//こちらに入ったときは余分に+
			copyText++;
		}
		//文字のコピー
		*origText = *copyText;
		copyText++;
	}
}

//-----------------------------------------------------------特定の文字で後ろ詰め
//author:n.koyama
//date  :20151222
//origText:対象文字列
//targ:削除文字
//return なし
void addAfterTargChar(char *origText,char targ,int length){
	char *origTextEnd;
	char *copyText;
	int addlen = 0;

	copyText  = origText + strlen(origText);
	//\0までをコピーしたい
	origTextEnd = origText + strlen(origText) + length;
	//最後からスタート
	for(;copyText < origTextEnd;copyText++){
		//文字のコピー
		*copyText = targ;
	}
	//最後に終端記号を入れて終了
	*copyText = '\0';
}

//画面オブジェクトの配列から名前に対応するオブジェクトのポインタを返す
//author:n.koyama
//date  :20150812
//
struct screenObject *searchTargetObject(struct screenObject *screenObjArray,char *targName,int arrayLen){
	struct screenObject *targObject;
	int ii = 0;

	//関数名を共有変数にセット
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
	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return targObject;
}

//画面オブジェクトの配列から行変数(YName)に対応するオブジェクトのポインタを返す
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

//画面オブジェクトの配列から列変数(XName)に対応するオブジェクトのポインタを返す
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
//cobtypeが数字のとき小数点以下の長さを判定
//author:koyama
//in :: digits :全体の長さ,editForm:対象とする文字列, delim:小数点の区切り
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
			//特殊文字の時はカウントしない
			break;
		case 'R':
			//DBはキーワード
			if(*(strToEnd - 1) == 'C'){
				strToEnd--;
				break;
			}
		case 'B':
			//DBはキーワード
			if(*(strToEnd - 1) == 'D'){
				strToEnd--;
				break;
			}
		case '9' :
		case '0' :
		case 'Z' :
		case '*' :
		case 0x5c :
			//円記号はEscになるので
		default  :
			//特殊文字,キーワードではないものをカウント
			cnt++;
		}

	}

	return cnt;
}

//cobtypeが数字のとき数字の部分の長さを判定
//PICがある時はdigitsの範囲が整数,無い時はdigitsの範囲が小数部込み
//author:koyama
int cobtype_calc_digits(char *cobtype){
	char *start_cobtype;
	char *end_cobtype;
	int hyphen_flg = 0;
	int retVal     = 0;

	start_cobtype = cobtype;
	//一つ後ろまで
	end_cobtype = cobtype + strlen(cobtype) +1;

	for(;start_cobtype < end_cobtype;start_cobtype++){
		if(*start_cobtype == cob_current_module->decimal_point){
			break;
		}
		if(*start_cobtype == '9' || *start_cobtype == 'Z' || *start_cobtype == '-'){
			//ハイフンが含まれる時はハイフン一つ少ない値になる
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
//editingFormatの数字項目をカウント
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

	//type文字列を一時変数にコピー
	if(targObj->cobtype[0] != 'R'){
		strcpy(tempCobtype,targObj->cobtype);
	}else{
		//1文字目がRならとばす
		strcpy(tempCobtype,(targObj->cobtype + 1));
	}

	//Rはreverseの指定なので無視させる
	if(strlen(tempCobtype) == 1 || strchr(tempCobtype,'S') != NULL){
		char setChar;
		if(strchr(tempCobtype,'S') == NULL){
			setChar = tempCobtype[0];
		}else{
			setChar = '-';
		}
		for(i=0;i < targObj->length;i++){
			//特定の文字の連続
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
			//最初は入れないために
			if(targCharCnt != 0){
				*(strReturn + (((targCharCnt - 1) * 5) + 1) )= (unsigned char)currCharCnt;
			}
			currChar = *tPoint;
			*(strReturn + (((targCharCnt) * 5)) ) = currChar;
			//ここで一つ見つかったことに
			currCharCnt=1;
			targCharCnt++;
		}else{
			currCharCnt++;
		}
	}
	//最後の指定の長さを入れる
	*(strReturn + (((targCharCnt - 1) * 5) + 1) )= (unsigned char)currCharCnt;


//	free(strFromat);
	return strReturn;
}

//
//targObjの行番号を返す
//author:koyama
int getLineTargetObject(struct screenObject *targObj){
	return (int)(targObj->y + targObj->lineShift );
}


//
//targObjの列番号を返す
//author:koyama
int getColTargetObject(struct screenObject *targObj){
	return (int)(targObj->x + targObj->ColShift );
}

//
//cob_fieldがpackedとして正しい値か確認
//author:koyama
int SD_isPackedFormat(cob_field *checkTarg){
	int ret=1,counter=0;
	//packの最後の0.5バイトにフラグがあるので正しいかどうかを判定
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
//editingFormatの数字項目をカウント
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
	//入力を求める
	printf("INP (%02d,%02d)(,%02d) %s \"%s\"_\r\n",
		getLineTargetObject(temporaryObj), getColTargetObject(temporaryObj),
		(getColTargetObject(temporaryObj) + temporaryObj->length), temporaryObj->cobtype, temporaryObj->cobargname);
	//何かとれるまで繰り返しておく
	counter = 0;
	while(fgets((char *)term_buff, BUFF_SIZE, stdin) == NULL){
		//値が無ければ何もしない                        check 20140508 koyama
		//数値か文字か
		usleep(WAIT_SLEEP);    //0.000001のsleep?
		//
	}
	printf("NIN (%02d,%02d)(,%02d) %s \"%s\"_\r\n",
		getLineTargetObject(temporaryObj), getColTargetObject(temporaryObj),
		(getColTargetObject(temporaryObj) + temporaryObj->length), temporaryObj->cobtype, temporaryObj->cobargname);
}



//
//2バイト文字を初期化
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
//添え字から変数の現在の値のポインタを返す
//author:n.koyama
//date  :20140611
//
char *SD_getFromVarPnt(struct fromObject fromVar,int length){

	char t;   //変数のサイズ用
	char *currentP;    //現在有効だと思われるデータのポインタ
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
			//ポインタの時はポインタの先を一時保存、それを数値化
			memcpy(tmp1,fromVar.fromVar1,fromVar.var1Length);
			//配列の添え字が1から始まるので
			dim1 = (fromVar.var1Size * sizeof(t)) * (atoi(tmp1) - 1);
		}else{
			dim1 = (fromVar.var1Size * sizeof(t)) * (fromVar.actualFlg1 - 1);
		}
		fromVar.curPnt = currentP + dim1;
		break;
	case 2:
		if(fromVar.actualFlg1 == -99){
			//ポインタの時はポインタの先を一時保存、それを数値化
			memcpy(tmp1,fromVar.fromVar1,fromVar.var1Length);
			//配列の添え字が1から始まるので
			dim1 = (fromVar.var1Size * sizeof(t)) * (atoi(tmp1) - 1);
		}else{
			dim1 = (fromVar.var1Size * sizeof(t)) * (fromVar.actualFlg1 - 1);
		}
		if(fromVar.actualFlg2 == -99){
			//ポインタの時はポインタの先を一時保存、それを数値化
			memcpy(tmp2,fromVar.fromVar2,fromVar.var2Length);
			//配列の添え字が1から始まるので
			dim2 = (fromVar.var2Size * sizeof(t)) * (atoi(tmp2) - 1);
		}else{
			dim2 = (fromVar.var2Size * sizeof(t)) * (fromVar.actualFlg2 - 1);
		}
		//0から始まるので - 1?
		fromVar.curPnt = currentP + dim1 + dim2 ;
		break;
	case 3:
		if(fromVar.actualFlg1 == -99){
			//ポインタの時はポインタの先を一時保存、それを数値化
			memcpy(tmp1,fromVar.fromVar1,fromVar.var1Length);
			//配列の添え字が1から始まるので
			dim1 = (fromVar.var1Size * sizeof(t)) * (atoi(tmp1) - 1);
		}else{
			dim1 = (fromVar.var1Size * sizeof(t)) * (fromVar.actualFlg1 - 1);
		}
		if(fromVar.actualFlg2 == -99){
			//ポインタの時はポインタの先を一時保存、それを数値化
			memcpy(tmp2,fromVar.fromVar2,fromVar.var2Length);
			//配列の添え字が1から始まるので
			dim2 = (fromVar.var2Size * sizeof(t)) * (atoi(tmp2) - 1);
		}else{
			dim2 = (fromVar.var2Size * sizeof(t)) * (fromVar.actualFlg2 - 1);
		}
		if(fromVar.actualFlg3 == -99){
			//ポインタの時はポインタの先を一時保存、それを数値化
			memcpy(tmp3,fromVar.fromVar3,fromVar.var3Length);
			//配列の添え字が1から始まるので
			dim3 = (fromVar.var3Size * sizeof(t)) * (atoi(tmp3) - 1);
		}else{
			dim3 = (fromVar.var3Size * sizeof(t)) * (fromVar.actualFlg3 - 1);
		}
		//0から始まるので - 1?
		fromVar.curPnt = currentP + dim1 + dim2 + dim3;
		break;
	default:
		break;
	}


	return fromVar.curPnt;
}

//---------------------------------------------------------------------------------------------SD_getInputVarPnt
//添え字から変数の現在の値のポインタを返す(input)
//author:n.koyama
//date  :20140611
//
char *SD_getInputVarPnt(struct inputObject inputVar,int length){

	char t;   //変数のサイズ用
	char *currentP;    //現在有効だと思われるデータのポインタ
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
			//ポインタの時はポインタの先を一時保存、それを数値化
			memcpy(tmp1,inputVar.var1,inputVar.var1Length);
			dim1 = (inputVar.var1Size * sizeof(t)) * (atoi(tmp1) - 1);
		}else{
			//添え字番号がひとつずれているから
			dim1 = (inputVar.var1Size * sizeof(t)) * inputVar.actualFlg1;
		}
		inputVar.curPnt = currentP + dim1;
		break;
	case 2:
		if(inputVar.actualFlg1 == -99){
			//ポインタの時はポインタの先を一時保存、それを数値化
			memcpy(tmp1,inputVar.var1,inputVar.var1Length);
			dim1 = (inputVar.var1Size * sizeof(t)) * (atoi(tmp1) - 1);
		}else{
			dim1 = (inputVar.var1Size * sizeof(t)) * inputVar.actualFlg1;
		}
		if(inputVar.actualFlg2 == -99){
			//ポインタの時はポインタの先を一時保存、それを数値化
			memcpy(tmp2,inputVar.var2,inputVar.var2Length);
			dim2 = (inputVar.var2Size * sizeof(t)) * (atoi(tmp2) - 1);
		}else{
			dim2 = (inputVar.var2Size * sizeof(t)) * inputVar.actualFlg2;
		}
		inputVar.curPnt = currentP + dim1 + dim2;
		break;
	case 3:
		if(inputVar.actualFlg1 == -99){
			//ポインタの時はポインタの先を一時保存、それを数値化
			memcpy(tmp1,inputVar.var1,inputVar.var1Length);
			dim1 = (inputVar.var1Size * sizeof(t)) * (atoi(tmp1) - 1);
		}else{
			dim1 = (inputVar.var1Size * sizeof(t)) * inputVar.actualFlg1;
		}
		if(inputVar.actualFlg2 == -99){
			//ポインタの時はポインタの先を一時保存、それを数値化
			memcpy(tmp2,inputVar.var2,inputVar.var2Length);
			dim2 = (inputVar.var2Size * sizeof(t)) * (atoi(tmp2) - 1);
		}else{
			dim2 = (inputVar.var2Size * sizeof(t)) * inputVar.actualFlg2;
		}
		if(inputVar.actualFlg3 == -99){
			//ポインタの時はポインタの先を一時保存、それを数値化
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
	//PACKED数字項目の時スペース埋め
	//COB_TYPE_NUMERIC_PACKEDがCOMP-3
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

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"SD_Initialize");

	memset(SD_cobScreen,'\0',sizeof(tmp) * SCR_OBJ_MAX);

	//表示のバッファをしない設定
	setvbuf( stdout, NULL, _IOLBF, 0 );

	//処理管理の関数呼び出し
	if(MT_Initialize() == 1){
		return 1;
	}

	//ファイルネームを元にリーダポインタを作成   //ファイル名を変数に変更 20150828
	xmlTextReaderPtr reader = xmlNewTextReaderFilename(getConfFilename(strConfPath));
	//リーダをリードできる状態に
	xmlTextReaderRead(reader);
	//現在のノードのポインタをセット？
	xmlTextReaderExpand(reader);
	//現在のノードからDOMを取り出している?
	xmlDocPtr doc = xmlTextReaderCurrentDoc(reader);
	if (!doc) return 1;
	//ドキュメントからコンテキスト()
	xmlXPathContextPtr ctx = xmlXPathNewContext(doc);
    if (!ctx) return 1;
	//xpathで指定したノードリストを取得
	xmlXPathObjectPtr xpobj = xmlXPathEvalExpression((xmlChar *)CONF_DEFPATH, ctx);
    if (!xpobj) return 1;
	//ノードリストをノードの配列のようなものに
	xmlNodeSetPtr nodes = xpobj->nodesetval;
	//ノード数の取得(取得できないなら0)
	int size = (nodes) ? nodes->nodeNr : 0;
	//ノードリストから値を表示
	for (i = 0; i < size; ++i) {
		if (!xmlXPathNodeSetIsEmpty(nodes)) {
			xmlNodePtr node = xmlXPathNodeSetItem(nodes, i);
			if (node->content) {
				//設定ファイルから画面定義のパスを取得
				if(strcmp(node->parent->name,CONF_SCRPATH) == 0){
					strcpy(SD_screenDefPath,node->content);
				}
				//設定ファイルから拡張子を取得
				if(strcmp(node->parent->name,CONF_SCREXT) == 0){
					strcpy(SD_screenDefExt,node->content);
				}
				//debugフラグがあるなら取得
				if(strcmp(node->parent->name,DEBUG_FLGNAME) == 0){
					//返還できない文字列は0になるはず
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
	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);
	return 0;
}

//cobolの変数をc側で管理するために定義をする
//
int SD_Init(char *argname,char *type,char *ychar,char *xchar,char *length,char *prevObjName,char *parentObjName){
	int i=0;
	char stry[128]="",strx[128]="";

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"SD_Init :%.10s",argname);

	//引数の固定値部分が正しく入ってないときはエラーに
	if(cob_call_params < 7){
		fprintf(stderr," Error C [%02d]: Option Missing  set option %d, need option is 7 target %s \n",99,cob_call_params,map_source_func);
		executeEnd();
	}

	//文字列が文字定数だと問題になるので
	strcpy(stry,ychar);
	//文字列が文字定数だと問題になるので
	strcpy(strx,xchar);

	if(SD_cobScreenLen > SCR_OBJ_MAX){
		fprintf(stderr," Error C [%02d]: I exceeds the maxsize screenObj \n",99);
		exit(1);
	}

	//ここで作った変数の初期化
	SD_cobScreen[SD_cobScreenLen].lineShift  = 0;
	SD_cobScreen[SD_cobScreenLen].ColShift  = 0;
	//ここで作ったポインタの初期化
	SD_cobScreen[SD_cobScreenLen].nextObj   = (void *)0;
	SD_cobScreen[SD_cobScreenLen].parentObj = (void *)0;
	SD_cobScreen[SD_cobScreenLen].childObj  = (void *)0;
	//cobolの変数を格納(ここではNULLで定義して一度でも使われたら格納する)
	SD_cobScreen[SD_cobScreenLen].bodyPnt  = (void *)0;
	SD_cobScreen[SD_cobScreenLen].inputVar.iVarSize = 0;
	SD_cobScreen[SD_cobScreenLen].fromVar.iVarSize = 0;
	SD_cobScreen[SD_cobScreenLen].onViewFlg = 0;

	//変数名の代入
	strcpy(SD_cobScreen[SD_cobScreenLen].cobargname,argname);

	//行番号の代入
	if(((int)ychar[0] >= 48) && ((int)ychar[0] < 58)){
		SD_cobScreen[SD_cobScreenLen].y = atoi(ychar);
	}else{
		if(strstr(stry,"PLUS ") != 0&& strrchr(stry,' ') != NULL){
			//PLUSが存在するとしてずらす
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
			//PLUSが存在するとしてずらす [arg] PLUS [VAL]なので二つ目のスペース
			SD_cobScreen[SD_cobScreenLen].ColShift = atoi((strrchr(strx,' ') + 1));
			SD_cobScreen[SD_cobScreenLen].x = 0;
			strcpy(SD_cobScreen[SD_cobScreenLen].xname,chrchange(strx,' ','\0'));
		}else{
			SD_cobScreen[SD_cobScreenLen].x = 0;
			strcpy(SD_cobScreen[SD_cobScreenLen].xname,strx);
		}
	}

	//型の格納
	strcpy(SD_cobScreen[SD_cobScreenLen].cobtype,type);

	SD_cobScreen[SD_cobScreenLen].length = atoi(length);


	//前のオブジェクトがある場合、前のオブジェクトを探してそのnextのポインタを格納
//	printf("prev:%s  parent:%s scrLen:%d \n",prevObjName,parentObjName,SD_cobScreenLen);
	if(strcmp(prevObjName," ") != 0){
		//後ろから見ることで同じ名前の時は直近のものをみる
		for(i=SD_cobScreenLen;i >= 0;i--){
			if(strcmp(SD_cobScreen[i].cobargname,prevObjName) == 0){
//				printf("%s %s\n",SD_cobScreen[i].cobargname,prevObjName);
				SD_cobScreen[i].nextObj = &SD_cobScreen[SD_cobScreenLen];
				break;
			}
		}
	}

	//親のオブジェクトがある場合、前のオブジェクトを探してそのnextのポインタを格納
	if(strcmp(parentObjName," ") != 0){
		//後ろから見ることで同じ名前の時は直近のものをみる
		for(i=SD_cobScreenLen;i >= 0;i--){
			if(strcmp(SD_cobScreen[i].cobargname,parentObjName) == 0){
				if(strcmp(SD_cobScreen[i].cobargname,SD_cobScreen[(SD_cobScreenLen - 1)].cobargname) == 0){
					SD_cobScreen[SD_cobScreenLen].parentObj = &SD_cobScreen[i];
					SD_cobScreen[i].childObj = &SD_cobScreen[SD_cobScreenLen];
					break;
				}else{
					//親子関係の対応
					//親指定は必ず直前でするコーディングのはずなので-1と一致しないのはおかしいはず？
					fprintf(stderr," Error C [%02d]: Parent-child relationship : %s's parent is %s ? \n",99,SD_cobScreen[SD_cobScreenLen].cobargname,SD_cobScreen[(SD_cobScreenLen - 1)].cobargname);
					executeEnd();
				}
			}
		}
	}
	SD_cobScreenLen += 1;

	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);
	return 0;
}

//行が可変の場合(未使用)
int SD_Init_Linename(char *argname,char *linename){
	int i=0;
	//対象のオブジェクトがある場合、前のオブジェクトを探してそのnextのポインタを格納
	for(i=0;i < SD_cobScreenLen;i++){
		if(strcmp(SD_cobScreen[i].cobargname,argname) == 0){
			strcpy(SD_cobScreen[i].yname,linename);
			break;
		}
	}
	return 0;
}

//行が可変の場合
int SD_Init_Colname(char *argname,char *colname){
	int i=0;
	//対象のオブジェクトがある場合、前のオブジェクトを探してそのnextのポインタを格納
	for(i=0;i < SD_cobScreenLen;i++){
		if(strcmp(SD_cobScreen[i].cobargname,argname) == 0){
			strcpy(SD_cobScreen[i].xname,colname);
			break;
		}
	}
	return 0;
}


//画面定義を出力形式に合わせて出力
//scrSrcPath:対象の画面定義のフルパス
int SD_Screen_Output(char *scrSrcPath){
	FILE *fp;
	char fpath[512];
	char filname[256] = "";    //ファイル名の受け取り用に必要
	char s[256];

	//ファイルのパスをコピー
	strcpy(fpath,SD_screenDefPath);

	//大文字で開く試行
	strcat(filname,scrSrcPath);

	//画面定義の名前を追加
	strcat(fpath,filname);
	//拡張子を追加
	strcat(fpath,SD_screenDefExt);

	if((fp = fopen(fpath,"r")) == NULL){
		//ファイルのパスをコピー
		strcpy(fpath,SD_screenDefPath);
		//小文字文字に変換でretry
		StrToLowerCpy(filname,scrSrcPath);
		//画面定義の名前を追加
		strcat(fpath,filname);
		//拡張子を追加
		strcat(fpath,SD_screenDefExt);

		if((fp = fopen(fpath,"r")) == NULL){
			//ファイルのオープンができなかったら関数を終了
			printf(" Error C [%02d]:can't open screen file %s \n",99,fpath);
			return 1;
		}
	}

	//  (4)ファイルの読み（書き）
	while (fgets(s, 256, fp) != NULL) {
		char *lastChar = NULL;
		// ここではfgets()により１行単位で読み出し
		//76文字目以降は認識しないことにする
		s[75] = '\0';
		//_or)が最後になる
		lastChar      = (char *)strrchr(s,'_');
		if(lastChar == NULL){
			lastChar      = (char *)strrchr(s,')');
		}
		//_も)も無いときは75文字目までをそのまま出力
		if(lastChar != NULL){
			*lastChar     = '\n';
			*(lastChar+1) = '\0';
		}
		printf("%s", s);
	}
	fclose(fp);   // (5)ファイルのクローズ
	return 0;
}

//画面からの値を変数に代入関数(cob変数版)
//in : targScreenObj:コピー対象,inputArg:コピー元文字列,recursiveLever:初期呼び出しを0に0なら兄弟を見ない
//author:koyama 20151019
void setDataRelation(struct screenObject *targScreenObj,cob_field *inputArg,char *status,int recursiveLever){
	struct screenObject *temporaryObj;
	temporaryObj = targScreenObj;

	while(temporaryObj != NULL){
		//子がいるなら子を再帰的に
		if(temporaryObj->childObj != NULL){
			setDataRelation(temporaryObj->childObj,inputArg,status,(recursiveLever + 1));
		}
		if(temporaryObj->inputVar.iVarSize != 0 || temporaryObj->usingVar.iVarSize != 0){
			//キャンセル系の処理の時は中身を消すべき？   check koyama ::
//			if(strncmp(status,"06",strlen("06")) != 0){
				//attrは宣言
				cob_field_attr  a_from = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
				cob_field_attr  a_to = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
				//cob_moveで移動しないとずれた時は対応できない
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
				//上で入れられたものを利用
				d_from.attr   = &a_from;

				d_to.size = tempVar.bodyPnt->size;
				d_to.data = SD_getInputVarPnt(tempVar,temporaryObj->length);
				d_to.attr = tempVar.bodyPnt->attr;

				cob_move(&d_from,&d_to);
//			}
		}

		//再帰0段目なら次に行かない
		if(recursiveLever == 0){
			break;
		}

		temporaryObj = temporaryObj->nextObj;
	}
}

//画面からの値を変数に代入関数(cob変数版)
//in : buff:コピー元文字列,targObj:コピー先オブジェクト,setArg:
//author:koyama 20150310 仕様変更が必要なため作り直し
void setDataCobarg(char *buff,struct screenObject *targScreenObj,cob_field *inputArg,char *status){
	char *term_buff;
	char t[1];
	cob_field *targObj;
	targObj = targScreenObj->bodyPnt;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"setDataCobarg ");

	//初期化
	term_buff = buff;

	cob_field_attr  a_from = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
	cob_field_attr  a_to   = {COB_TYPE_NUMERIC, 0, 0, 0, NULL};
	cob_field       d_from;
	cob_field       d_to;

	//元の変数の属性をコピー
	a_to.type  = targObj->attr->type;
	a_to.flags = targObj->attr->flags;
	a_to.digits= targObj->attr->digits;
	a_to.scale = targObj->attr->scale;
	a_to.pic   = targObj->attr->pic;

	//送り元のtypeはCOB_TYPE_ALPHANUMERICなので変えない
	//送り元の長さは一応文字列長で設定
	//中身を初期化
	a_from.flags = 0;
	a_from.scale = 0;
	a_from.digits = strlen(term_buff);
	if(a_to.type == COB_TYPE_NUMERIC){
//		a_from.type = COB_TYPE_NUMERIC;
		//ドットを探しあればそれ以降を小数点に
		if(strchr(term_buff,0x2e) != 0){
			//\0との比較なので-1
			a_from.type = COB_TYPE_NUMERIC_DISPLAY;
			a_from.scale = (strchr(term_buff,0x00) - strchr(term_buff,0x2e)) - 1;
			remTargChar(term_buff,0x2e);
		}else{
			a_from.scale = a_to.scale;
			//入力に小数点の分だけ後ろを0埋め 埋める必要はない upd koyama 20170829
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

	//ここにコピー
	d_to.size   = targObj->size;
	d_to.data   = inputArg->data;
	d_to.attr   = &a_to;
	d_from.size = a_from.digits;
	d_from.data = term_buff;
	d_from.attr = &a_from;
	cob_move(&d_from,&d_to);
	//入れた時のattrを持って帰る
	inputArg->size = d_to.size;
	inputArg->attr = &a_to;
	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);

}


//空文字列の時、元の値相当のものをセット
//in : temporaryObj:元の値を取り出すオブジェクト,targObj:コピー先オブジェクト,setArg:
//author:koyama 20150310 仕様変更が必要なため作り直し
int influenceWhenEmptyString(char *inputArg,struct screenObject *temporaryObj,cob_field *d_from){
	int retVal = 0;
	char *strPic;
	//空にする処理に変更
	//データの挿入位置を始点からどれだけずらすか
	cob_field       d_null;
	cob_field       d_to;
	//attrは共有するので外で宣言
	cob_field_attr  a_null = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
	cob_field_attr  a_to = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
	//空を作成(d_fromがbodyから取られている場合困るので、作成し直す
	a_null.digits  = temporaryObj->length;
	d_null.data    = malloc(temporaryObj->length + 1);
	memset(d_null.data,'\0',(temporaryObj->length + 1));
	memset(d_null.data,' ',temporaryObj->length);
	d_null.size    = temporaryObj->length;
	d_null.attr    = &a_null;
	//サイズからならコピーされないので
	d_to.size = 0 ;
	//値の反映はinputとusingに行う
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
	//データmoveでリセットする処理.d_fromはポインタ受取なので
	cob_move(&d_null,d_from);
	cob_move(d_from,&d_to);

	free(d_null.data);

	//最後に値を写して終了
	memcpy(inputArg,d_to.data,d_to.size);
	return retVal;
}

//画面からの値を変数に代入関数
//in : buff:コピー元文字列,targObj:コピー先オブジェクト,setArg:
//author:koyama 20150310 仕様変更が必要なため作り直し
void setDataRecursive(char *buff,struct screenObject *targObj,char *inputArg,char *status){
	struct screenObject *temporaryObj;
	char *term_buff;
	char t[1];

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"setDataRecursive ");

	//初期化
	term_buff = buff;
	temporaryObj = targObj;

	//データの挿入位置を始点からどれだけずらすか
	cob_field       d_from;
	cob_field       d_to;
	//attrは共有するので外で宣言
	cob_field_attr  a_from = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
	cob_field_attr  a_to = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};

	//あとで汎用的に使うためにセット
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

	//ENTER or TAB以外の時は入力とみなさない
	if((strncmp(status,"01",strlen("01")) == 0 && strncmp(status,"06",strlen("06")) == 0)){
		//共有変数を元に戻す
		unsetCommonFunctionName(map_source_func,strStack);
		return ;
	}
	//(文字の長さが0)若しくは全てから(Enterで確定された時以外) (TODO:スペースのみを入力するケースがある場合out)
	if((strlen(term_buff) == 0 || isNullOrEmpty(term_buff) == 1) && strncmp(status,"01",strlen("01")) != 0){
		//中身を初期化
		influenceWhenEmptyString(inputArg,targObj,&d_from);
		//共有変数を元に戻す
		unsetCommonFunctionName(map_source_func,strStack);
		return ;
	}
	if(targObj->bodyPnt != NULL){
		//cobarrgが存在するなら
		setDataCobarg(term_buff,targObj,&d_from,status);
		//下で使うために値コピー
		a_to.type   = targObj->bodyPnt->attr->type;
		a_to.digits = targObj->bodyPnt->attr->digits;
		a_to.scale  = targObj->bodyPnt->attr->scale;
		a_to.flags  = targObj->bodyPnt->attr->flags;

	}else if(strchr(temporaryObj->cobtype,'Z') != 0 || strchr(temporaryObj->cobtype,'-') != 0){
		//数値か文字か(フォーマット付き数字列)
		//中身を初期化
		memset(inputArg, ' ', temporaryObj->length);
		//数値のときは挿入の開始位置をずらす(後ろが長さなのでそのまま-)
		memcpy((inputArg + (temporaryObj->length - strlen(term_buff))), term_buff, strlen(term_buff));

	}else if(strchr(temporaryObj->cobtype,'9') != 0){
		//数字(Z,-を含まない数字列)
		//型に合わせて
		if(strchr(temporaryObj->cobtype,'S') == 0){
			//S9ではない
			a_to.type = (unsigned char)COB_TYPE_NUMERIC;
			//符号なしなので0
			a_to.flags=0;
		}else{
			//画面オブジェクトにCOMP3は存在できないはず
			//符号付き数(S9)
			a_to.type = (unsigned char)COB_TYPE_NUMERIC;
			//符号なしなので0
			a_to.flags=1;
		}
		//送り先の長さはtypeより取得
		a_to.digits=temporaryObj->length;
		a_to.pic = NULL;

		//送り元のtypeはCOB_TYPE_ALPHANUMERICなので変えない
		//送り元の長さは一応文字列長で設定
		a_from.digits=strlen(term_buff);
		a_from.flags = 0;
		a_from.scale = 0;

		//中身を初期化
		memset(inputArg, '0', temporaryObj->length);

		//ここにコピー
		d_from.size = strlen(term_buff);
		d_from.data = term_buff;
		// 先にセットしたので
		// d_from.attr = &a_from;
		d_to.size   = temporaryObj->length;
		d_to.data   = inputArg;
		d_to.attr   = &a_to;
		cob_move(&d_from,&d_to);
	}else if(strchr(temporaryObj->cobtype,'N') != 0||strchr(temporaryObj->cobtype,'X') != 0){
		//型だけ先に
		a_from.type = COB_TYPE_NATIONAL;
		a_to.type = COB_TYPE_NATIONAL;

		//送り先の長さはtypeより取得
		a_to.digits = temporaryObj->length;
		a_to.pic    = NULL;

		//送り元のtypeはCOB_TYPE_ALPHANUMERICなので変えない
		//送り元の長さは一応文字列長で設定
		a_from.digits = strlen(term_buff);
		a_from.flags  = 0;
		a_from.scale  = 0;

		//中身を初期化
		if(strncmp(status,"01",strlen("01")) == 0){
			//HTab
			memset2byte(inputArg, "　", strlen(term_buff));
		}else if(strncmp(status,"06",strlen("06")) == 0){
			//skip
			memset2byte(inputArg, "　", temporaryObj->length);
		}

		//ここにコピー
		d_from.size = strlen(term_buff);
		d_to.size   = temporaryObj->length;
		d_to.data   = inputArg;
		d_from.data = term_buff;
		d_to.attr   = &a_to;
		// d_from.attr = &a_from;    //先にセットしたので
		cob_move(&d_from,&d_to);
	}else{
		//中身を初期化
		memset(inputArg, ' ', temporaryObj->length);
		memcpy(inputArg, term_buff,  strlen(term_buff));
	}

	setDataRelation(temporaryObj,&d_from,status,0);
	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);
}



//ステータスのセットと文字の不要部分の除去する関数
//in:(ref)term_buff,(ref)eStatus
void setStatusASplit(char *term_buff,char *eStatus){
	char *split_start=NULL;
	//直strchrを書くと問題があるのでポインタを取得
	//`が存在し,それより後ろの長さが`プラス2文字+改行以内
	if(strrchr(term_buff,'`') != 0 && (strlen(strrchr(term_buff,'`')) < 5  || strcmp(strrchr(term_buff,'`'),"`esc") == 1)){
		split_start = strrchr(term_buff,'`');
	}
	//後ろにステータスがないときは改行だけを除去
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
		// スプリッタを除去する前に改行コードを除去
		if(strchr(term_buff,'\n') != 0){
			term_buff[(strchr(term_buff,'\n') - term_buff)] = '\0';
		}
		//すぷりったを削除(この時に改行コードなども除去される)
		memset(split_start ,'\0',strlen(strchr(term_buff,'`')));
	}else{
		//すぷりったを削除(この時に改行コードなども除去される)
		//ステータスの記述がなければエンターだということにする
		strcpy(eStatus,"01");
		if(strchr(term_buff,'\n') != 0){
			memset(strchr(term_buff,'\n'),'\0',strlen(term_buff));
		}
	}
}

//画面からの受け取った値を既存の値と混ぜる(statusがSKIPのとき)
//in : temporaryObj 混ぜる画面オブジェクト,inArgPos 入力時のデフォルト,input_term入力
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
		//compならコピーの時に
		if(temporaryObj->fromVar.bodyPnt != NULL
			&& temporaryObj->fromVar.bodyPnt->attr->type == COB_TYPE_NUMERIC_PACKED){
			from_base   = temporaryObj->fromVar.bodyPnt;
			d_from.size = temporaryObj->fromVar.iVarSize;
		}
	}else if(temporaryObj->usingVar.iVarSize != 0){
		inArgPos = SD_getInputVarPnt(temporaryObj->usingVar,temporaryObj->length);
		varLength = temporaryObj->usingVar.iVarSize;
		//compならコピーの時に
		if(temporaryObj->usingVar.bodyPnt != NULL
			&& temporaryObj->usingVar.bodyPnt->attr->type == COB_TYPE_NUMERIC_PACKED){
			from_base   = temporaryObj->usingVar.bodyPnt;
			d_from.size = temporaryObj->usingVar.iVarSize;
		}
	}else{
		memset(inArgPos,' ',strlen(inArgPos));
		varLength   = temporaryObj->length;
	}
	//ここのsizeはformのデータ長に併せなければダメ？
	a_to.digits = varLength;
	d_to.size   = temporaryObj->length;
	d_to.data   = tempStr;
	d_to.attr   = &a_to;
	d_from.data = inArgPos;
	d_from.attr = &a_from;
	if(from_base != NULL){
		//COMP3は必ず数字なので
		a_to.type     = COB_TYPE_NUMERIC_DISPLAY;
		//input OR Fromなので元の属性にあわせる(typeを数字にしたので、必要になる)
		a_from.type     = from_base->attr->type;
		a_from.digits = from_base->attr->digits;
		a_from.scale  = from_base->attr->scale;
		a_from.flags  = from_base->attr->flags;
		//COMP3の場合バイト長とデータの長さが違うので
		// varLength     = from_base->attr->digits;
		varLength     = temporaryObj->length;
		if(SD_isPackedFormat(&d_from) == 0){
			//COMP3なのにcomp3として正しい値ではなかったら
			cob_set_packed_zero (&d_from);
		}
	}
	cob_move(&d_from,&d_to);
	// strncat(tempStr,inArgPos,varLength);

	//入力と出力のサイズが違う時
	intLengthDiff = varLength - temporaryObj->length;
	//intLengthDiffが意味が違うようなので調整 upd koyama
	if(intLengthDiff >= 0){
		strcat(input_term,tempStr + (strlen(input_term) + intLengthDiff));
	}else{
		memset(input_term,' ',abs(intLengthDiff));
		strcat(input_term,tempStr + (strlen(input_term)));
	}

	return 0;
}

//画面からの受け取った値を既存の値と混ぜる(statusがPF5のとき)
//in : temporaryObj 混ぜる画面オブジェクト,inArgPos 入力時のデフォルト,input_term入力
//author : koyama 20170314
int mergeInputValueConditionPF5(struct screenObject *temporaryObj,char *inArgPos,char *input_term){
	int varLength = 0;
	int relationFlg = 0;
	//using,fromがあるときはその値を使う
	if(temporaryObj->fromVar.iVarSize != 0){
		// inArgPos = SD_getFromVarPnt(temporaryObj->fromVar,temporaryObj->length);
		relationFlg=1;
		varLength = temporaryObj->fromVar.iVarSize;
	}else if(temporaryObj->usingVar.iVarSize != 0){
		// inArgPos = SD_getInputVarPnt(temporaryObj->usingVar,temporaryObj->length);
		relationFlg=1;
		varLength = temporaryObj->usingVar.iVarSize;
	}
	// PF5のときはrelationの値を
	if(relationFlg != 0){
		strncpy(input_term,inArgPos,varLength);
	}

	return 0;
}

//画面からの受け取った値を既存の値と混ぜる(statusがEnterのとき)
//in : temporaryObj 混ぜる画面オブジェクト,inArgPos 入力時のデフォルト,input_term入力
//author : koyama 20170314
int mergeInputValueConditionEnter(struct screenObject *temporaryObj,char *inArgPos,char *input_term){
	int varLength = 0;
	//using,fromがあるときはその値を使う
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


//画面からの値受取り関数
//in : buff,targObj
void getDataRecursive(char *buff,struct screenObject *targObj,char *status,char *inputArg,int recursiveLever){
	struct screenObject *temporaryObj;
	char term_buff[BUFF_SIZE]="";
	char *inArgPos = inputArg;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"getDataRecursive ");

	temporaryObj = targObj;

	while(temporaryObj != NULL){
		//子がいるなら子を再帰的に
		if(temporaryObj->childObj != NULL){
			getDataRecursive(term_buff,temporaryObj->childObj,status,inArgPos,(recursiveLever + 1));
			//次の値を入れる場所は現在のObjの長さ分ずらす
			inArgPos = inArgPos + temporaryObj->length;
		}else{
			SD_AcceptStdin(temporaryObj,term_buff);

			//ステータスのセットと文字の不要部分の除去
			setStatusASplit(term_buff,status);
			//エンターでは無い時
			if(strncmp(status,"01",strlen("01")) != 0){
				//TABでは無い時
				if(strncmp(status,"06",strlen("06")) == 0){
					//using,fromがあるときはその値を使う
					//20170314 処理の簡略化のため関数化
					mergeInputValueConditionSkip(temporaryObj,inArgPos,term_buff);
					setDataRecursive(term_buff,temporaryObj,inArgPos,status);
				}else if(strncmp(status,"P5",strlen("P5")) == 0){
					//PF5）(F5)のときは値を持ってくる
					mergeInputValueConditionPF5(temporaryObj,inArgPos,term_buff);
					//データを移さないパターン
					setDataRecursive(term_buff,temporaryObj,inArgPos,status);
				//usingやinputがあるときには引用しない TODO::koyama 20161116
					//20170314 処理の簡略化のため関数化
					// mergeInputValueConditionEnter(temporaryObj,inArgPos,term_buff);
					//
				}
			}else{
				//01(Enterの場合はからで送る)入力を拒否
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

	//取得した値をコピー
	//	strcpy(inputArg,buff);
	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);
}


//SD_Accept 標準入力受け取りの際にコードを入れる
int SD_Accept(char *inputArg,char *argName,char *type,char *size,char *eStatus){
	//cobcで8192で取られていたので
	char term_buff[BUFF_SIZE]="";int counter=0,argumentFlg = 0;
	struct screenObject *targObject;
	int iSize = atoi(size),i=0;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"SD_Accept :%.10s",inputArg);

	//ダミーの変数を作成
	// type, digits, scale, flags, pic
	const cob_field_attr a_dummy = {1, 0, 0, 0, NULL};
	unsigned char b_dummy[128]="";//__attribute__((aligned))は変数一つなのでいらない
	cob_field f_dummy = {128, b_dummy, &a_dummy};

	//Search target
	targObject = searchTargetObject(SD_cobScreen,argName,SD_cobScreenLen);

	//引数からの受け取りのとき
	cob_accept_arg_value(&f_dummy);
	//入力のcobol argをセット
	targObject->bodyPnt = cob_current_module->cob_procedure_parameters[0];

	//半角SPが検索されないときは何らかのエラーかも(NULLと文字列ポインタを比較できないから)
	//先頭文字が半角スペースでないとき
	if( (strchr(b_dummy,0x20) == NULL) &&  ( (char *)b_dummy != strchr(b_dummy,0x20) ) ){
		struct screenObject *temporaryObject; //子供を持つ親のときのために値受取り用
		temporaryObject = targObject;

		getDataRecursive(term_buff,temporaryObject,eStatus,inputArg,0);
	}else{
		//引数からの入力のとき
		strncpy(term_buff,b_dummy,(strchr(b_dummy,0x20) - (char *)b_dummy));
		//HTB
		strcpy(eStatus,"01");
		//値をセットする
		setDataRecursive(term_buff,targObject,inputArg,eStatus);
	}

	//確定ボタン以外は画面表示を切り替えない
	if(strcmp(eStatus,"01") == 0 || strcmp(eStatus,"06") == 0 ){
		//from,usingの時は入力された値を表示する
		if(targObject->fromVar.iVarSize != 0 || targObject->usingVar.iVarSize != 0){
			//入力した文字を反映
			if(targObject->fromVar.iVarSize != 0){
				//fromは入力項目としては来ないと思われる
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

	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);
	return 0;
}

int stopExec(){
	int result;
	char term_buff[BUFF_SIZE];int counter;
	char *eStatus;
	int intStatLen=3;
	struct screenObject temporaryObj;

	//stopの時は仮定義のobjで表示
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
		//01=EnterでもESCでもない時
	}while(strncmp(eStatus,"01",strlen("01")) && strncmp(eStatus,"ESC",strlen("ESC")));

	free(eStatus);
}

//FROM句の対応
//
//cobArgName:対象変数の名前 argPoint:接続する変数のポインタ
int SD_From(char *cobArgName,char *argPoint,char *cSize,char *cArgc,...){
	int i=0, j=0, iArgc=0,iSize;
	int argVaiable = 4;    //どこまでが必ず来る引数か(この関数内での定数とする)
	va_list list;
	char *subscript;
	int funcId = 4;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"SD_From  :%.10s ",cobArgName);

	//引数の固定値部分が正しく入ってないときはエラーに
	if(cob_call_params < 4){
		cob_runtime_error(" Error C [%02d]: Option Missing  set option %d, need option is 4 target %s \n",99,cob_call_params,cobArgName);
		executeEnd();
	}
	//(!isdigit(*cArgc) == 1 && isNullOrEmpty(cArgc) == 1)のisNullOrEmpty(cArgc)はいらなそうなので削除 upd koyama 20170818
	//サイズが数字である、配列の数字が無い、配列の数字の箇所に空文字列,'0',スペースの時|| *cArgc == '0'
	if(!isdigit(*cSize) ||  (!isdigit(*cArgc) && isNullOrEmpty(cArgc) == 0 )){
		//cSize,cArgcが不正だったら関数を終了
		cob_runtime_error(" Error C [%02d]:invalid parameter %s ",99,map_source_func);
		//共有変数を元に戻す
		unsetCommonFunctionName(map_source_func,strStack);
		return 1;
	}

	//文字列で来るのでintに変換
	//すうじ
	iArgc = atoi(cArgc);
	iSize = atoi(cSize);

	struct screenObject *targObject;
	targObject = searchTargetObject(SD_cobScreen,cobArgName,SD_cobScreenLen);

	//対象のオブジェクトがある場合、前のオブジェクトを探してそのnextのポインタを格納
	if(iArgc == 0){
		//argPointの内容が配列ではない
		targObject->fromVar.arrPnt = (char *)argPoint;
		targObject->fromVar.iVarSize = iSize;
		targObject->fromVar.iDim = iArgc;
		targObject->fromVar.bodyPnt = cob_current_module->cob_procedure_parameters[1];

		//editing fromatのときは入れ替える
		if(strlen(targObject->cobtype) > 1
		&& strchr(targObject->cobtype,'S') == NULL
		&& strchr(targObject->cobtype,'b') == NULL
		&& strchr(targObject->cobtype,'R') == NULL ){
//					targObject->length = iSize;
		}
	} else {
		//argPointの内容が配列
		va_start( list, cArgc );
		//対象変数の探索
		//editing fromatのときは入れ替える
		if(strlen(targObject->cobtype) > 1
		&& strchr(targObject->cobtype,'S') == NULL
		&& strchr(targObject->cobtype,'b') == NULL
		&& strchr(targObject->cobtype,'R') == NULL ){
//					targObject->length = iSize;
		}
		//変数の数に関係のない項目を
		targObject->fromVar.arrPnt = (char *)argPoint;
		targObject->fromVar.iVarSize = iSize;
		targObject->fromVar.iDim = iArgc;
		//ここはOCCURSの可能性があるので入れなおしが必要 20150525
		targObject->fromVar.bodyPnt = malloc( sizeof(cob_field) );
		targObject->fromVar.bodyPnt->size = cob_current_module->cob_procedure_parameters[1]->size;
		targObject->fromVar.bodyPnt->data = cob_current_module->cob_procedure_parameters[1]->data;
		targObject->fromVar.bodyPnt->attr = cob_current_module->cob_procedure_parameters[1]->attr;
		for(j = 0;j < (iArgc * 2); j++){

			subscript = va_arg( list, char* );
			struct cob_field *curParam;

			switch(j){
			case 0:
				//COBOLから渡される引数の文字列長とこの引数のサイズが一致しないA'0'でない
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
				//COBOLから渡される引数の文字列長とこの引数のサイズが一致しないA'0'でない
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
				//COBOLから渡される引数の文字列長とこの引数のサイズが一致しないA'0'でない
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
	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);
	return 0;
}

//into の変数を入力項目として出力
//Fromと仕組みは同じの模様,入力チェックがあるところが違う
int SD_Into(char *cobArgName,char *argPoint,char *cSize,char *cArgc,...){
	int i=0, j=0, iArgc=0,iSize;
	int argVaiable = 4;    //どこまでが必ず来る引数か(この関数内での定数とする)
	va_list list;
	char *subscript;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"SD_Into :%.10s",cobArgName);

	//引数の固定値部分が正しく入ってないときはエラーに
	if(cob_call_params < 4){
		fprintf(stderr," Error C [%02d]: Option Missing  set option %d, need option is 4 target %s \n",99,cob_call_params,cobArgName);
		executeEnd();
	}

	//(!isdigit(*cArgc) == 1 && isNullOrEmpty(cArgc) == 1)のisNullOrEmpty(cArgc)はいらなそうなので削除 upd koyama 20170818
	if(!isdigit(*cSize) ||  (!isdigit(*cArgc) == 1)){
		//cSize,cArgcが不正だったら関数を終了
		cob_runtime_error(" Error C [%02d]:invalid parameter %s ",99,map_source_func);
		//共有変数を元に戻す
		unsetCommonFunctionName(map_source_func,strStack);
		return 1;
	}

	//文字列で来るのでintに変換
	iArgc = atoi(cArgc);
	iSize = atoi(cSize);

	struct screenObject *targObject;
	targObject = searchTargetObject(SD_cobScreen,cobArgName,SD_cobScreenLen);

	//対象のオブジェクトがある場合、前のオブジェクトを探してそのnextのポインタを格納
	if(iArgc == 0){
		//argPointの内容が配列ではない
				targObject->inputVar.arrPnt = (char *)argPoint;
				targObject->inputVar.iVarSize = iSize;
				targObject->inputVar.iDim = iArgc;
				//ここはOCCURSの可能性があるので入れなおしが必要 20150525
				targObject->inputVar.bodyPnt = malloc( sizeof(cob_field) );
				targObject->inputVar.bodyPnt->size = cob_current_module->cob_procedure_parameters[1]->size;
				targObject->inputVar.bodyPnt->data = cob_current_module->cob_procedure_parameters[1]->data;
				targObject->inputVar.bodyPnt->attr = cob_current_module->cob_procedure_parameters[1]->attr;

				//editing fromatのときは入れ替える
				if(strlen(targObject->cobtype) > 1
				&& strchr(targObject->cobtype,'S') == NULL
				&& strchr(targObject->cobtype,'b') == NULL
				&& strchr(targObject->cobtype,'R') == NULL ){
//					targObject->length = iSize;
				}
	} else {
		//argPointの内容が配列
		va_start( list, cArgc );
		//対象変数の探索
		//editing fromatのときは入れ替える
		if(strlen(targObject->cobtype) > 1
		&& strchr(targObject->cobtype,'S') == NULL
		&& strchr(targObject->cobtype,'b') == NULL
		&& strchr(targObject->cobtype,'R') == NULL ){
//					targObject->length = iSize;
		}
		//変数の数に関係のない項目を
		targObject->inputVar.arrPnt = (char *)argPoint;
		targObject->inputVar.iVarSize = iSize;
		targObject->inputVar.iDim = iArgc;
		//ここはOCCURSの可能性があるので入れなおしが必要 20150525
		targObject->inputVar.bodyPnt = malloc( sizeof(cob_field) );
		targObject->inputVar.bodyPnt->size = cob_current_module->cob_procedure_parameters[1]->size;
		targObject->inputVar.bodyPnt->data = cob_current_module->cob_procedure_parameters[1]->data;
		targObject->inputVar.bodyPnt->attr = cob_current_module->cob_procedure_parameters[1]->attr;

		for(j = 0;j < (iArgc * 2); j++){
			subscript = va_arg( list, char* );
			switch(j){
			case 0:
				//COBOLから渡される引数の文字列長とこの引数のサイズが一致しないA'0'でない
				if((strlen(cob_current_module->cob_procedure_parameters[argVaiable + j]->data)
					== cob_current_module->cob_procedure_parameters[argVaiable + j]->size)
					&& atoi(subscript) != 0){
					//実の値が入ってくるとき
					targObject->inputVar.actualFlg1 = atoi(subscript);
				}else{
					//ポインタ内にデータが有るとき
					targObject->inputVar.var1 = subscript;
					targObject->inputVar.var1Length = cob_current_module->cob_procedure_parameters[argVaiable + j]->size;
					targObject->inputVar.actualFlg1 = -99;
				}
				break;
			case 1:
				memcpy((int *)&targObject->inputVar.var1Size,subscript,sizeof(int));
				break;
			case 2:
				//COBOLから渡される引数の文字列長とこの引数のサイズが一致しないA'0'でない
				if((strlen(cob_current_module->cob_procedure_parameters[argVaiable + j]->data)
					== cob_current_module->cob_procedure_parameters[argVaiable + j]->size)
					&& atoi(subscript) != 0){
					//実の値が入ってくるとき
					targObject->inputVar.actualFlg2 = atoi(subscript);
				}else{
					//ポインタ内にデータが有るとき
					targObject->inputVar.var2 = subscript;
					targObject->inputVar.var2Length = cob_current_module->cob_procedure_parameters[argVaiable + j]->size;
					targObject->inputVar.actualFlg2 = -99;
				}
				break;
			case 3:
				memcpy((int *)&targObject->inputVar.var2Size,subscript,sizeof(int));
				break;
			case 4:
				//COBOLから渡される引数の文字列長とこの引数のサイズが一致しないA'0'でない
				if((strlen(cob_current_module->cob_procedure_parameters[argVaiable + j]->data)
					== cob_current_module->cob_procedure_parameters[argVaiable + j]->size)
					&& atoi(subscript) != 0){
					//実の値が入ってくるとき
					targObject->inputVar.actualFlg3 = atoi(subscript);
				}else{
					//ポインタ内にデータが有るとき
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
	//共有変数を元に戻す
	unsetCommonFunctionName(map_source_func,strStack);
	return 0;
}


//Using の変数を入力項目として出力
//Fromと仕組みは同じの模様,入力チェックがあるところが違う
int SD_Using(char *cobArgName,char *argPoint,char *cSize,char *cArgc,...){
	int i=0, j=0, iArgc=0,iSize;
	int argVaiable = 4;    //どこまでが必ず来る引数か(この関数内での定数とする)
	va_list list;
	char *subscript;

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"SD_Using :%.10s",cobArgName);

	//引数の固定値部分が正しく入ってないときはエラーに
	if(cob_call_params < 4){
		cob_runtime_error(" Error C [%02d]: Option Missing  set option %d, need option is 4 target %s \n",99,cob_call_params,cobArgName);
		executeEnd();
	}

	//(!isdigit(*cArgc) == 1 && isNullOrEmpty(cArgc) == 1)のisNullOrEmpty(cArgc)はいらなそうなので削除 upd koyama 20170818
	if(!isdigit(*cSize) ||  (!isdigit(*cArgc) == 1)){
		//cSize,cArgcが不正だったら関数を終了
		cob_runtime_error(" Error C [%02d]:invalid parameter %s ",99,map_source_func);
		//共有変数を元に戻す
		unsetCommonFunctionName(map_source_func,strStack);
		return 1;
	}

	//文字列で来るのでintに変換
	iArgc = atoi(cArgc);
	iSize = atoi(cSize);

	struct screenObject *targObject;
	targObject = searchTargetObject(SD_cobScreen,cobArgName,SD_cobScreenLen);

	//対象のオブジェクトがある場合、前のオブジェクトを探してそのnextのポインタを格納
	if(iArgc == 0){
		//argPointの内容が配列ではない
		targObject->usingVar.arrPnt = (char *)argPoint;
		targObject->usingVar.iVarSize = iSize;
		targObject->usingVar.iDim = iArgc;
		//ここはOCCURSの可能性があるので入れなおしが必要 20150525
		targObject->usingVar.bodyPnt = malloc( sizeof(cob_field) );
		targObject->usingVar.bodyPnt->size = cob_current_module->cob_procedure_parameters[1]->size;
		targObject->usingVar.bodyPnt->data = cob_current_module->cob_procedure_parameters[1]->data;
		targObject->usingVar.bodyPnt->attr = cob_current_module->cob_procedure_parameters[1]->attr;
		//editing fromatのときは入れ替える
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
		//argPointの内容が配列
		va_start( list, cArgc );
		//対象変数の探索
		//editing fromatのときは入れ替える
		if(strlen(targObject->cobtype) > 1
		&& strchr(targObject->cobtype,'S') == NULL
		&& strchr(targObject->cobtype,'b') == NULL
		&& strchr(targObject->cobtype,'R') == NULL ){
//					targObject->length = iSize;
		}
		//変数の数に関係のない項目を
		targObject->usingVar.arrPnt = (char *)argPoint;
		targObject->usingVar.iVarSize = iSize;
		targObject->usingVar.iDim = iArgc;
		//ここはOCCURSの可能性があるので入れなおしが必要 20150525
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
				//COBOLから渡される引数の文字列長とこの引数のサイズが一致しないA'0'でない
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
				//COBOLから渡される引数の文字列長とこの引数のサイズが一致しないA'0'でない
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
				//COBOLから渡される引数の文字列長とこの引数のサイズが一致しないA'0'でない
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
	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return 0;
}

//using の変数を一致させる？
//
int SD_Arg_Match(char *arg1,char *arg2,char *arg3){
	int i=0;
	//対象のオブジェクトがある場合、前のオブジェクトを探してそのnextのポインタを格納
//	for(i=0;i < SD_cobScreenLen;i++){
//		if(strcmp(SD_cobScreen[i].cobargname,arg1) == 0){
//			break;
//		}
//	}
	memcpy(arg3, arg2, strlen(arg2));
	return 0;
}

//変数の行を一致させる
//
int SD_Arg_Match_Line(char *argname, char *size, char *arg){
	int i=0;
	char temp[64]="";
	struct screenObject *temporaryObj;
	//arg後ろがつながっているケースがあるので長さできる
	strncpy(temp,arg,atoi(size));
	//対象のオブジェクトがある場合、前のオブジェクトを探してそのnextのポインタを格納
	setYNameTargetObject(SD_cobScreen,argname,(atoi(temp)),SD_cobScreenLen);
	return 0;
}

//変数の行を一致させる
//
int SD_Arg_Match_Col(char *argname, char *size,char *arg){
	int i=0;
	char temp[64]="";
	struct screenObject *temporaryObj;
	//arg後ろがつながっているケースがあるので長さできる
	strncpy(temp,arg,atoi(size));
	//対象のオブジェクトがある場合、前のオブジェクトを探してそのnextのポインタを格納
	setXNameTargetObject(SD_cobScreen,argname,(atoi(temp) + SD_cobScreen[i].ColShift),SD_cobScreenLen);
	return 0;
}

//DISPLAYの代わりに出力を行う
//画面出力のために必要なものを付加する処理を行う
//子どもは再帰?兄弟はループで処理かな
int resetAllViewFlg(){
	int i = 0;

	for(i=0;i < SD_cobScreenLen;i++){
		SD_cobScreen[i].onViewFlg = 0;
	}
}


//DISPLAYの代わりに出力を行う
//画面出力のために必要なものを付加する処理を行う
//子どもは再帰?兄弟はループで処理かな
int SD_Output(char *argname,char *printarg,char *curLength){
	int i=0,j=0;                                             //roop変数
	int currentLength = 0;
	struct screenObject *currentObject;
	int printflg = 0;
	char strTime[] = "0000/00/00 00:00:00.000000";
//	char strTemp[HRIZONTAL_LENGTH];

	//関数名を共有変数にセット
	char strStack[MAP_SRC_FUNC_LEN];
	setCommonFunctionName(strStack,map_source_func,"SD_Output :%.10s",argname);

	if(myConfDebugFlg){
		cob_runtime_error(" Error [%04d]: %s Info SD_Output ",__LINE__,local_server_time(strTime));
	}
	//最初の第一歩
	//curLengthの1文字目が数字以外で0うめ8桁になっていなければ外から来ている->それ以外の時はpointerが送られてくる
	if(isdigit(*curLength) == 0 || strlen(curLength) != 8){
		currentObject = searchTargetObject(SD_cobScreen,argname,SD_cobScreenLen);
	}else{
		//
		currentObject = (struct screenObject *)argname;
	}
	//atoiは数字以外なら0を返すため入力が文字なら0
	currentLength = atoi(curLength);
	printflg = 1;                                     //ターゲットを一つでも見つけたらflg on
	for(j = 0;currentLength <= strlen(printarg);j++){
		//最後が半角スペースの場合取り除かれ、後ろの文字に影響が出るため"で囲んでおく
		char *buzzerPointer;
		char strTemp[HRIZONTAL_LENGTH] = "\"";        //"のみの文字列で初期化
		char strTemp2[HRIZONTAL_LENGTH] = "";         //から文字列で初期化
		if(currentObject->childObj != 0){
			//再帰で下を呼ぶ予定
			char charCurrentLength[10];                         //次の関数へ現在の
			sprintf(charCurrentLength, "%08d", currentLength);
			//charポインタとして送るが受け取ったあとで別のポインタに受け取る
			SD_Output((char *)currentObject->childObj,printarg,charCurrentLength);
		} else {
			//-,Z,Bの対応

			//表示させることが確定した表示変数のフラグを変える
			currentObject->onViewFlg = 1;

			//文字列の接続
			if(currentObject->fromVar.iVarSize != 0 || currentObject->usingVar.iVarSize != 0){
				//Fromのときはポインタから出力
				//inputの処理もまとめる
				char toData[2048]="";
				char *fromData;
				char *strPic;
				cob_field_attr  a_from = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
				cob_field_attr  a_to = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
				cob_field       d_from;
				cob_field       d_to;
				cob_field       *from_base;

				//-----------------------------------------------------------送り先の設定
				if(strchr(currentObject->cobtype,'N')!=0){
					a_to.type = (unsigned char)COB_TYPE_NATIONAL;
				}else if(strchr(currentObject->cobtype,'X')!=0){
					a_to.type = (unsigned char)COB_TYPE_ALPHANUMERIC;
				}else{
					a_to.type = (unsigned char)COB_TYPE_NUMERIC_EDITED;
				}

				a_to.flags=0;
				//送り先のedit
				if(a_to.type == (unsigned char)COB_TYPE_NUMERIC_EDITED){
					strPic = setFormatEdit(currentObject,strPic);
					a_to.pic = strPic;
				}else{
					//捨てるので内容は関係ない
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
					//小数点ではなくpicがあるときにすべき
					//S9の指定も小数点を含む場合がある
					//TODO 判定を数字の時に数字桁のみに
					if( (*strPic != '\0') && (strcmp(currentObject->cobtype,"9") != 0) && (strcmp(currentObject->cobtype,"S9") != 0)){
						//小数点があるときはscaleを計算
						a_to.digits = cobtype_calc_digits(currentObject->cobtype);
						d_to.size   = currentObject->length;
						a_to.flags=1;
					}else{
						a_to.digits = currentObject->length;
						d_to.size   = currentObject->length;
						a_to.flags=0;
					}
				}

				//小数点のことを考えて
				if(strchr(currentObject->cobtype,cob_current_module->decimal_point) != NULL){
					//.があれば小数点下
					a_to.scale = SD_calc_editing_scale(a_to.digits,currentObject->cobtype,cob_current_module->decimal_point);
//					a_to.scale = a_to.digits - (int)(strchr(currentObject->cobtype,'.') - (int)currentObject->cobtype);
//					a_to.scale = strlen(currentObject->cobtype) -(currentObject->cobtype - strchr(currentObject->cobtype,'.'));
				}else{
					//.がなければ小数点下は0
					a_to.scale = 0;
				}

				//-----------------------------------------------------------送り元の設定
				//Zや-(ハイフン)の時は文字列長が1でもediting format
				//コピー元となる変数を初期化
				if(currentObject->fromVar.iVarSize != 0){
					fromData = SD_getFromVarPnt(currentObject->fromVar,currentObject->length);
					from_base = currentObject->fromVar.bodyPnt;
					//sizeはdataを取得した時のサイズ
					d_from.size = currentObject->fromVar.iVarSize;
				}else{
					if(currentObject->inputVar.iVarSize != 0){
						fromData = SD_getInputVarPnt(currentObject->inputVar,currentObject->length);
						from_base = currentObject->inputVar.bodyPnt;
						//sizeはdataを取得した時のサイズ
						d_from.size = currentObject->inputVar.iVarSize;
					}else{
						fromData = SD_getInputVarPnt(currentObject->usingVar,currentObject->length);
						from_base = currentObject->usingVar.bodyPnt;
						//sizeはdataを取得した時のサイズ
						d_from.size = currentObject->usingVar.iVarSize;
					}
				}

				//input OR Fromなので元の属性にあわせる
				a_from.type   = from_base->attr->type;
				a_from.digits = from_base->attr->digits;
				a_from.scale  = from_base->attr->scale;
				a_from.flags  = from_base->attr->flags;

				d_to.data = toData;
				d_from.data = fromData;
				d_to.attr = &a_to;
				d_from.attr = &a_from;
				//------------------------------------------moveする直前で初期化が正しくされていなければ初期化する
				SD_ObjectExistsInitData(&d_from);
				//-------------------------------------------------------------move
				cob_move(&d_from,&d_to);

				//functionで中身を作っているので注意
				free(strPic);
				strncpy(strTemp2, toData, strlen(toData));
//				free(toData);
			}else{
				strncpy(strTemp2, (printarg + currentLength), (currentObject->length));
			}
			strcat(strTemp,strTemp2);
			strcat(strTemp,"\"");

			//表示のバッファをしない設定
//					setvbuf( stdout, NULL, _IOLBF, 0 );

			//文字列の長さで出力
			//ブザーの定数が存在すれば
			if((buzzerPointer = strchr(strTemp,0x1B)) > 0){
				printf("BUZ (%02d,%02d) (%c,%d)_\r\n", getLineTargetObject(currentObject), getColTargetObject(currentObject), buzzerPointer[1],buzzerPointer[2]);
				stopExec();
			}else{
				//"をよけたところが一致するかどうか
				//一致したらCOBOLコード上のステータス
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
		//現在の開始位置を次の開始位置に変更
		currentLength = currentLength + currentObject->length;
//printf("|(%d\t^_^\t%s %d)| \n", currentLength, currentObject->cobargname,currentObject->length);

		//
		if(strncmp(strTemp,"CLEAR",strlen("CLEAR")) == 0){
			resetAllViewFlg();
		}

		//初期変数はbreak
		//入力された値が0-9以外なら
		if(isdigit(curLength[0]) == 0){
			break;
		}

		//次のノードを入れてループ
		if((currentObject->nextObj != 0)){
			currentObject = currentObject->nextObj;
		}else{
			//次のノードがないor初期指定ノードの時は次を読まない
			//初期指定ノードの時はそのノード,そのノードの子を出力
			break;
		}

		//止まらないと危険なので
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
	//共有変数を元に戻す
	memcpy(map_source_func,strStack,MAP_SRC_FUNC_LEN);
	return 0;
}

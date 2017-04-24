/* header files */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libcob.h>
#include "confheader.h"

#ifndef MT_COMMON
#define MT_COMMON 1
#define MAP_SRC_FUNC_LEN 50
char map_source_func[MAP_SRC_FUNC_LEN]="";
int MT_Initialize();
#endif


#define min(a,b) ((a) < (b) ? (a) : (b))
#define MAX_STRINGS 20		//入力ファイル読込最大文字数

/* 文字列をcobolに転写する関数 */
void C3_move_to_cob(char *cob_dat, const char *dat)
{
    int len = strlen(cob_dat);                      // data length in cob
    memset(cob_dat, ' ', min(len, strlen(dat)));    // clear with spaces
    memcpy(cob_dat, dat, min(len, strlen(dat)));    // data copy
    return;
}

/* JRCODE読込み */
/* 引数：
		iId		:ログインID,
		oValue	:JRCODEの値
*/
int C3_Get_Jrcode(char *iId,char *oValue){
	int ret = 0;
	int cnt = 0;	//ループカウンタ
	char *chrDefPath;
	char fName[100];
	char data[MAX_STRINGS];		//読込文字列
	FILE *fp;			//ファイルポインター

	//文字列初期化
	fName[0] ='\0';
	
	//ファイル名「FILE_NAME + iId」作成
	chrDefPath = getenv( "JRCODE_PATH" );
	if(chrDefPath != NULL){
		strcat(fName, chrDefPath);
	}else{
		strcat(fName, "");
	}
	strcat(fName,iId);

	if((fp = fopen(fName,"w")) != NULL ){
		// ファイルが存在する場合

		/* ファイルデータ処理 */
		while(fgets(data, MAX_STRINGS, fp) != NULL){
			if(cnt == 0){
				//1文字目のみデータ取得
				cnt += 1;
				C3_move_to_cob(oValue, data);
			}else{
				//2文字目移行が存在すればエラー
				ret = 1;
				break;
			}
		}
		
		/* ファイル・クローズ */
		fclose(fp);
	}else{
		// ファイルが存在しない場合(fopenでファイルが作成できなかった場合)
		ret = 1;
	}

	return ret;
}

/* JRCODE書込み */
/* 引数：
		iId		:ログインID,
		iValue	:JRCODEの値
*/
int C3_Set_Jrcode(char *iId,char *code,int *iValue){
	int ret = 0;
	char *chrDefPath;
	char fName[100]="";
	char chrCode[4]="";
	FILE *fp;

	//文字列初期化
	fName[0] ='\0';
	
	if(cob_call_params != 3){
		fprintf(stderr," Error C [%02d]: C3_Set_Jrcode is not match Argument count \n",99);
		exit(1);
	}
	
	//引数を文字に変換
	sprintf(chrCode,"%03d",*iValue);
	
	//ファイル名「FILE_NAME + iId」作成
	chrDefPath = getenv( "JRCODE_PATH" );
	if(chrDefPath != NULL){
		strcat(fName, chrDefPath);
	}else{
		strcat(fName, "");
	}
	strcat(fName,iId);

	/* ファイル・オープン */
	if((fp = fopen(fName, "w")) != NULL){
		
		/* ファイルデータ処理 */
		if(fputs(chrCode, fp) == EOF){
			ret = 1;
		}
		
		//ファイルへの書き込みができたら元のCODEへも返す
		memcpy(code,chrCode,strlen(chrCode));
		
		/* ファイル・クローズ */
		fclose(fp);
	
	}else{
		// ファイルが存在しない場合
		ret = 1;
	}

	return ret;
}


